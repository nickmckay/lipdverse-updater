#!/usr/bin/env Rscript
#
# Screen every time axis in the database for a name that disagrees with its
# units, and for units no calendar can be derived from.
#
#   ./scripts/scan-axis-units.R              # whole database -> review/
#   ./scripts/scan-axis-units.R --out=x.csv
#
# Written after MaeHongSon.Buckley.2007 reported a span of -56 to 390 AD for a
# record covering 1560-2005: it names its axis `age` and gives it `units =
# "yr AD"`, and lv_table_axes() used to pick the axis by name. The picker now
# goes by units (see R/calculate.R), so nothing here is load-bearing for a run.
# It exists to size the file-side defect, which is issue #14.
#
# The flags, and why each is a problem:
#
#   name and units disagree      the two fields cannot both be right; the units
#                                are the ones the pipeline believes
#   kiloyear/megayear axis       values are not years, and the corpus is not
#                                consistent enough to scale automatically --
#                                `yr ka` values run from 0.84 to 33,116
#   uncalibrated radiocarbon     radiocarbon years are not calendar years
#   b2k axis                     ages count from 2000, so a year derived with
#                                the usual 1950 datum is 50 years off
#   units carry no axis info     `unitless`, `count/yr`, `needsToBeChanged`;
#                                the pipeline falls back to the variableName,
#                                which is a guess rather than a reading

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
out  <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (!length(out)) out <- file.path("review", "axis-units-review.csv")
cores <- max(1L, parallel::detectCores() - 2L)

files <- list.files(lv_path("database"), "[.]lpd$", full.names = TRUE)
cli::cli_alert_info("Scanning {length(files)} dataset{?s} on {cores} core{?s}")

AXIS_NAMES <- c("year", "year ad", "yearad", "age", "agebp", "age bp", "yearbp")

one <- function(p) {
  L <- tryCatch(suppressWarnings(suppressMessages(lipdR::readLipd(p))), error = function(e) NULL)
  if (is.null(L)) return(NULL)
  rows <- list()
  for (pd in L$paleoData) for (tb in pd$measurementTable) {
    if (!is.list(tb)) next
    cols <- if (!is.null(tb$columns)) tb$columns else tb
    for (col in cols) {
      if (!is.list(col) || is.null(col$TSid)) next
      nm <- tolower(trimws(as.character(col$variableName)[1] %||% ""))
      if (!nm %in% AXIS_NAMES) next
      v <- suppressWarnings(as.numeric(unlist(col$values)))
      fin <- v[is.finite(v)]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        dataset = sub("[.]lpd$", "", basename(p)),
        tsid = as.character(col$TSid)[1],
        variableName = nm,
        units = tolower(trimws(as.character(col$units)[1] %||% "")),
        n_values = length(v),
        min = if (length(fin)) min(fin) else NA_real_,
        max = if (length(fin)) max(fin) else NA_real_)
    }
  }
  if (!length(rows)) NULL else purrr::list_rbind(rows)
}

r <- purrr::list_rbind(parallel::mclapply(files, one, mc.cores = cores))
cli::cli_alert_success("{nrow(r)} axis column{?s} across {dplyr::n_distinct(r$dataset)} dataset{?s}")

# Asked of lv_axis_kind() itself, so the flags describe what the pipeline will
# actually do rather than offering a second opinion about it. Reading the units
# twice under opposite fallbacks separates the two cases the function collapses:
# where the answers agree the units decided, and where they differ the units
# said nothing and the variableName was guessed from.
as_age  <- vapply(r$units, lv_axis_kind, "", name = "age")
as_year <- vapply(r$units, lv_axis_kind, "", name = "year")
r$kind <- ifelse(is.na(as_age), NA_character_,
                 ifelse(as_age == as_year, as_age, "silent"))
r$name_kind <- ifelse(r$variableName %in% c("year", "year ad", "yearad"), "year", "age")

r$flag <- case_when(
  grepl("14c|c14", r$units)                        ~ "uncalibrated radiocarbon axis",
  is.na(r$kind)                                    ~ "kiloyear/megayear axis",
  grepl("b2k", r$units)                            ~ "b2k axis (ages count from 2000, not 1950)",
  r$kind %in% c("year", "age") & r$kind != r$name_kind ~ "name and units disagree",
  r$kind == "silent"                               ~ "units carry no axis information",
  TRUE                                             ~ NA_character_)

# Which compilations reach the dataset, so the list can be worked in the order
# that matters: a mislabelled axis nobody's QC sheet asks about is inert, and a
# hydroclimate2k one is a wrong number on a published sheet.
ts <- lv_db_index(lv_scan(lv_path("database")), cache = TRUE)$timeseries
memb <- tibble::tibble(dataset = rep(ts$dataSetName, lengths(ts$compilations)),
                       comp = unlist(ts$compilations)) |>
  filter(!is.na(comp), nzchar(comp)) |>
  group_by(dataset) |>
  summarise(compilations = paste(sort(unique(comp)), collapse = "; "), .groups = "drop")

flagged <- r |> filter(!is.na(flag)) |>
  left_join(memb, by = "dataset") |>
  select(dataset, flag, compilations, tsid, variableName, units, n_values, min, max) |>
  arrange(flag, dataset)

print(as.data.frame(flagged |> group_by(flag) |>
  summarise(columns = n(), datasets = n_distinct(dataset)) |> arrange(desc(columns))), right = FALSE)

fs::dir_create(dirname(out))
readr::write_csv(flagged, out)
cli::cli_alert_success("Wrote {.path {out}} -- {nrow(flagged)} flagged column{?s}")
