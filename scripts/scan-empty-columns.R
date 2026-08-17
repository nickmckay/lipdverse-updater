#!/usr/bin/env Rscript
#
# Find measurement columns that carry no value of any kind, and record what
# would block deleting each one.
#
#   ./scripts/scan-empty-columns.R              # whole database -> review/
#   ./scripts/scan-empty-columns.R --out=x.csv
#
# Found while working out why hydroclimate2k's QC sheet showed a minYear on
# columns like `mineralogy` and `uncertainty`: those columns hold nothing, so
# there is no year range to state. Deleting them looked like an easy tidy-up
# and is not -- see the `blocker` column, and issue #15.
#
# "Empty" here means no value of ANY type, not merely no number. That
# distinction matters: `as.numeric()` cannot tell an empty column from one
# holding "calcite", and clearing a year range from a column that does carry
# text would be a different and much less obvious call.
#
# The blockers, in the order they are applied:
#
#   compilation member  the column is declared in an inCompilation entry, so
#                       deleting it changes that compilation's membership.
#                       Mostly SISAL-LiPD, whose fixed schema expresses "no
#                       correction applied" as an empty column -- a schema
#                       disagreement, not a defect to sweep up
#   axis                deleting `year`/`age`/`depth` removes the record that
#                       the table is supposed to have an axis, which is a
#                       different statement from the axis being empty
#   empties the dataset every column in the dataset is empty; what needs fixing
#                       is the dataset, not its columns
#   none                safe to delete

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
out  <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (!length(out)) out <- file.path("review", "empty-columns.csv")
cores <- max(1L, parallel::detectCores() - 2L)

files <- list.files(lv_path("database"), "[.]lpd$", full.names = TRUE)
cli::cli_alert_info("Scanning {length(files)} dataset{?s} on {cores} core{?s}")

AXIS <- c("year", "year ad", "yearad", "age", "agebp", "age bp", "yearbp", "depth")

one <- function(p) {
  L <- tryCatch(suppressWarnings(suppressMessages(lipdR::readLipd(p))), error = function(e) NULL)
  if (is.null(L)) return(NULL)
  rows <- list()
  for (blk in c("paleoData", "chronData")) {
    if (is.null(L[[blk]])) next
    for (pd in L[[blk]]) for (tb in pd$measurementTable) {
      if (!is.list(tb)) next
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      for (col in cols) {
        if (!is.list(col) || is.null(col$TSid)) next
        v <- unlist(col$values)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          dataset = sub("[.]lpd$", "", basename(p)),
          block = blk,
          tsid = as.character(col$TSid)[1],
          variableName = as.character(col$variableName)[1] %||% NA_character_,
          units = as.character(col$units)[1] %||% NA_character_,
          n_values = length(v),
          n_nonblank = sum(!is.na(v) & nzchar(as.character(v))))
      }
    }
  }
  if (!length(rows)) NULL else purrr::list_rbind(rows)
}

r <- purrr::list_rbind(parallel::mclapply(files, one, mc.cores = cores))
r <- r |> distinct(dataset, block, tsid, .keep_all = TRUE)
cli::cli_alert_success("{nrow(r)} column{?s} across {dplyr::n_distinct(r$dataset)} dataset{?s}")

empty <- r |> filter(n_nonblank == 0)

idx <- lv_db_index(lv_scan(lv_path("database")), cache = TRUE)
ts <- idx$timeseries
memb <- tibble::tibble(tsid = rep(ts$TSid, lengths(ts$compilations)),
                       comp = unlist(ts$compilations)) |>
  filter(!is.na(comp), nzchar(comp)) |>
  group_by(tsid) |>
  summarise(compilations = paste(sort(unique(comp)), collapse = "; "), .groups = "drop")

# Every column of the dataset empty: the dataset is the problem, not the column.
emptied <- r |> group_by(dataset) |>
  summarise(all_empty = all(n_nonblank == 0), .groups = "drop") |>
  filter(all_empty) |> pull(dataset)

empty <- empty |>
  left_join(memb, by = "tsid") |>
  mutate(blocker = case_when(
    !is.na(compilations)                          ~ "compilation member",
    tolower(trimws(variableName)) %in% AXIS       ~ "axis",
    dataset %in% emptied                          ~ "empties the dataset",
    TRUE                                          ~ "none")) |>
  select(dataset, blocker, compilations, block, tsid, variableName, units, n_values) |>
  arrange(blocker, dataset)

cat(sprintf("\ncolumns holding no value of any type: %d of %d (%.1f%%), in %d dataset%s\n",
            nrow(empty), nrow(r), 100 * nrow(empty) / nrow(r),
            dplyr::n_distinct(empty$dataset),
            if (dplyr::n_distinct(empty$dataset) == 1) "" else "s"))
print(as.data.frame(empty |> count(blocker, sort = TRUE)), right = FALSE)
cat("\nby compilation (a column can belong to several):\n")
cc <- empty |> filter(!is.na(compilations))
print(as.data.frame(tibble::tibble(comp = unlist(strsplit(cc$compilations, "; "))) |>
                      count(comp, sort = TRUE) |> head(12)), right = FALSE)

fs::dir_create(dirname(out))
readr::write_csv(empty, out)
cli::cli_alert_success("Wrote {.path {out}}")
