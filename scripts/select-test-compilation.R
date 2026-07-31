#!/usr/bin/env Rscript
#
# Choose the datasets and TSids for the standing test compilation.
#
#   ./scripts/select-test-compilation.R --out=review/test-compilation.csv
#
# This is a health check meant to be run regularly, so it is chosen for
# coverage rather than size: every structural condition the pipeline has to
# handle should appear at least a few times, and the whole thing should run in
# a couple of minutes.
#
# Selection is deterministic (fixed seed) so the compilation is stable across
# regenerations, and each row records why it was picked.

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})

args <- commandArgs(trailingOnly = TRUE)
out  <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (!length(out)) out <- "review/test-compilation.csv"
target_datasets <- as.integer(sub("^--n=", "", grep("^--n=", args, value = TRUE)))
if (!length(target_datasets) || is.na(target_datasets)) target_datasets <- 140L

set.seed(20260731)
idx <- readRDS(path.expand("~/.local/share/lipdverse-updater/index.rds"))
db  <- path.expand("~/Dropbox/lipdverse/database")
snap <- path.expand("~/GitHub/lipdverse-qcstore/snapshots")

# ---- evidence --------------------------------------------------------------

# Compilation membership, from the QC sheets' own membership tabs.
mem <- bind_rows(lapply(setdiff(list.dirs(snap, recursive = FALSE), file.path(snap, "_shared")), function(d) {
  f <- file.path(d, "datasetsInCompilation.csv"); if (!file.exists(f)) return(NULL)
  x <- suppressWarnings(read_csv(f, col_types = cols(.default = col_character()),
                                 progress = FALSE, name_repair = "minimal"))
  x <- x[, !duplicated(names(x)), drop = FALSE]
  nm <- intersect(c("dsn", "dataSetName"), names(x)); if (!length(nm)) return(NULL)
  inc <- if ("inComp" %in% names(x)) tolower(x$inComp) %in% c("true", "yes") else rep(TRUE, nrow(x))
  tibble(dataSetName = unique(na.omit(x[[nm[1]]][inc])), compilation = basename(d))
}))
memn <- mem |> count(dataSetName, name = "n_compilations")

ds <- idx$datasets |>
  select(dataSetName = fileDataSetName, datasetId, archiveType, n_ts, md5, path) |>
  left_join(memn, by = "dataSetName") |>
  mutate(n_compilations = tidyr::replace_na(n_compilations, 0L))

# File size stands in for weight: the pipeline should see both trivial and
# heavy datasets.
ds$bytes <- file.size(ds$path)

# Keys that indicate structure worth exercising.
raw <- readRDS("/private/tmp/claude-503/-Users-nicholas-GitHub-lipdverse-updater/c0f418c5-2449-4189-9059-ccac00df557c/scratchpad/raw_keys.rds")
has_key <- function(pattern) unique(raw$dataset[grepl(pattern, raw$key)])
csm_src <- read_csv("review/csm-field-names.csv", col_types = cols(.default = col_character()),
                    progress = FALSE)$key
csm_bare <- unique(sub("^(paleoData|chronData|geo|pub[0-9]*|calibration)_", "", csm_src))

# Ensembles are not visible in the key inventory (ensemble columns look like
# any other), so detect them directly. They are large, so only the heaviest
# files need scanning.
ens_candidates <- ds$path[order(-ds$bytes)][seq_len(min(900L, nrow(ds)))]
has_ensemble_file <- function(p) {
  m <- tryCatch(utils::unzip(p, list = TRUE)$Name, error = function(e) character())
  j <- grep("\\.jsonld$", m, value = TRUE)
  if (!length(j)) return(FALSE)
  con <- unz(p, j[1]); on.exit(close(con), add = TRUE)
  any(grepl("ensembleTable", readLines(con, warn = FALSE), fixed = TRUE))
}
message("scanning ", length(ens_candidates), " large files for ensemble tables")
ens_ds <- ds$dataSetName[order(-ds$bytes)][seq_len(length(ens_candidates))][
  vapply(ens_candidates, has_ensemble_file, logical(1))]

ds <- ds |> mutate(
  has_chron     = dataSetName %in% has_key("^chronData_"),
  has_ensemble  = dataSetName %in% ens_ds,
  has_interp    = dataSetName %in% has_key("^interpretation"),
  has_calib     = dataSetName %in% has_key("^calibration_"),
  has_csm       = dataSetName %in% unique(raw$dataset[sub("^(paleoData|chronData|geo|pub[0-9]*|calibration)_", "", raw$key) %in% csm_bare]),
  odd_name      = grepl("[^A-Za-z0-9._-]", dataSetName)
)

# Datasets carrying a substantive cross-compilation disagreement.
conf_path <- "/private/tmp/claude-503/-Users-nicholas-GitHub-lipdverse-updater/c0f418c5-2449-4189-9059-ccac00df557c/scratchpad/conflicts.csv"
conflicted <- character()
if (file.exists(conf_path)) {
  cf <- read_csv(conf_path, col_types = cols(.default = col_character()), progress = FALSE)
  cf <- cf[cf$kind == "substantive", ]
  conflicted <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% cf$TSid])
}
ds$has_conflict <- ds$dataSetName %in% conflicted

# Records named in the reported incidents.
# The reported records are site prefixes, not exact TSids: the real ones are
# LS12THAY01E, LS12THAY01B and so on.
incident_ts <- c("LS12THAY", "LS14FEZA")
incident_ds <- unique(idx$timeseries$dataSetName[
  Reduce(`|`, lapply(incident_ts, function(p) startsWith(idx$timeseries$TSid, p)))])
ds$is_incident <- ds$dataSetName %in% incident_ds

# ---- selection -------------------------------------------------------------
# Take a quota from each condition rather than sampling at random, so a rare
# condition cannot be missed. Datasets already chosen count toward later quotas.

picked <- character()
why <- list()
take <- function(pool, n, reason) {
  pool <- setdiff(pool, picked)
  if (!length(pool)) return(invisible())
  chosen <- if (length(pool) <= n) pool else sample(pool, n)
  picked <<- c(picked, chosen)
  why[[reason]] <<- chosen
  invisible()
}

take(ds$dataSetName[ds$is_incident], 99, "incident record")
take(ds$dataSetName[ds$has_conflict], 12, "cross-compilation conflict")
take(ds$dataSetName[ds$n_compilations == 0], 12, "in no compilation")
take(ds$dataSetName[ds$n_compilations == 1], 12, "in one compilation")
take(ds$dataSetName[ds$n_compilations %in% 2:3], 12, "in 2-3 compilations")
take(ds$dataSetName[ds$n_compilations >= 4], 12, "in 4+ compilations")
take(ds$dataSetName[ds$has_csm], 12, "carries compilation-specific metadata")
take(ds$dataSetName[ds$has_ensemble], 8, "has an ensemble table")
take(ds$dataSetName[ds$has_chron], 8, "has chron data")
take(ds$dataSetName[ds$has_calib], 6, "has calibration metadata")
take(ds$dataSetName[ds$has_interp], 6, "has interpretations")
take(ds$dataSetName[ds$odd_name], 5, "non-alphanumeric characters in the name")
take(ds$dataSetName[ds$n_ts <= 3], 5, "few columns")
take(ds$dataSetName[ds$n_ts >= 100], 5, "many columns")
take(ds$dataSetName[ds$bytes < stats::quantile(ds$bytes, 0.05)], 5, "very small file")
take(ds$dataSetName[ds$bytes > stats::quantile(ds$bytes, 0.98)], 5, "very large file")
# One of each archive type, so vocabulary handling is exercised broadly.
for (a in sort(unique(stats::na.omit(ds$archiveType)))) {
  take(ds$dataSetName[ds$archiveType %in% a], 2, paste0("archiveType: ", a))
}
# Top up to the target with a plain random sample, for realism.
take(ds$dataSetName, max(0, target_datasets - length(picked)), "random")

sel <- ds |> filter(dataSetName %in% picked)
sel$reason <- vapply(sel$dataSetName, function(d) {
  r <- names(why)[vapply(why, function(v) d %in% v, logical(1))]
  paste(r, collapse = "; ")
}, character(1))

# ---- TSids -----------------------------------------------------------------
# A compilation is a set of TSids. Take every paleo column of each chosen
# dataset: partial datasets would make the membership tab and the files
# disagree in a way no real compilation does.
ts <- idx$timeseries |>
  filter(dataSetName %in% sel$dataSetName, tableType == "paleo") |>
  left_join(sel |> select(dataSetName, reason, archiveType, n_compilations), by = "dataSetName")

write_csv(ts |> select(TSid, dataSetName, datasetId, variableName, archiveType,
                       n_compilations, reason), out, na = "")

cat("wrote ", out, "\n\n", sep = "")
cat("datasets: ", nrow(sel), "   TSids: ", nrow(ts), "\n", sep = "")
cat("total size: ", round(sum(sel$bytes) / 1e6, 1), " MB\n\n", sep = "")
cat("coverage:\n")
cov <- tibble(
  condition = c("incident record", "cross-compilation conflict", "in no compilation",
                "in 1", "in 2-3", "in 4+", "carries csm", "ensemble", "chron",
                "calibration", "interpretations", "odd name", "<=3 columns",
                ">=100 columns"),
  n = c(sum(sel$is_incident), sum(sel$has_conflict), sum(sel$n_compilations == 0),
        sum(sel$n_compilations == 1), sum(sel$n_compilations %in% 2:3),
        sum(sel$n_compilations >= 4), sum(sel$has_csm), sum(sel$has_ensemble),
        sum(sel$has_chron), sum(sel$has_calib), sum(sel$has_interp),
        sum(sel$odd_name), sum(sel$n_ts <= 3), sum(sel$n_ts >= 100)))
print(as.data.frame(cov), row.names = FALSE)
cat("\narchive types covered: ", dplyr::n_distinct(sel$archiveType), " of ",
    dplyr::n_distinct(ds$archiveType), "\n", sep = "")
