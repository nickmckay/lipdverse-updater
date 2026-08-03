#!/usr/bin/env Rscript
#
# Seed the version ledger from what has actually been published.
#
#   ./scripts/seed-versions.R
#   ./scripts/seed-versions.R --commit
#
# Versions are A_B_C: A ticks when the compilation is published, B when the set
# of included datasets changes, C when metadata changes. lipdverseR kept the
# current version in a Google Sheet and in the published output directories.
# Without a baseline in the ledger, the first run under this system computes a
# version from nothing and invents one -- hydroclimate2k would have been
# recorded as 1_0_0 when it is really at 0_4_0, claiming a publication that has
# not happened.
#
# The published directories under html/<compilation>/<version>/ are the record
# of what was actually released, so they are the source. The dataset set stored
# alongside is the compilation's membership now, which is what the next run
# compares against to decide whether B or C ticks.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
commit <- "--commit" %in% args
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) path.expand(v[1]) else d }
html   <- getarg("html", file.path(dirname(lv_path("database")), "html"))

store <- qc_store()
idx <- lv_db_index(lv_scan(lv_path("database")), cache = TRUE)
comps <- lv_compilations()$compilation

latest <- function(comp) {
  d <- file.path(html, comp)
  if (!dir.exists(d)) return(NA_character_)
  v <- grep("^[0-9]+_[0-9]+_[0-9]+$", list.files(d), value = TRUE)
  if (!length(v)) return(NA_character_)
  p <- do.call(rbind, lapply(strsplit(v, "_"), as.integer))
  v[order(p[, 1], p[, 2], p[, 3])][length(v)]
}

rows <- lapply(comps, function(comp) {
  ts <- lv_compilation_timeseries(idx, comp)
  ds <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% ts])
  tibble::tibble(compilation = comp, published = latest(comp),
                 in_ledger = lv_version_current(store, comp) %||% NA_character_,
                 members = length(ts), datasets = length(ds))
})
x <- bind_rows(rows)
print(as.data.frame(x), right = FALSE)

todo <- x[!is.na(x$published) & is.na(x$in_ledger), , drop = FALSE]
cat(sprintf("\n%d compilation%s to seed\n", nrow(todo), if (nrow(todo) == 1) "" else "s"))
already <- x[!is.na(x$in_ledger), , drop = FALSE]
if (nrow(already)) {
  cat(sprintf("%d already in the ledger, left alone: %s\n", nrow(already),
              paste(already$compilation, collapse = ", ")))
}
missing <- x[is.na(x$published) & is.na(x$in_ledger), , drop = FALSE]
if (nrow(missing)) {
  cat(sprintf("%d with no published version and none in the ledger, skipped: %s\n",
              nrow(missing), paste(missing$compilation, collapse = ", ")))
}

if (!commit) { cat("\npass --commit to seed.\n"); quit(save = "no") }

for (i in seq_len(nrow(todo))) {
  comp <- todo$compilation[i]
  ts <- lv_compilation_timeseries(idx, comp)
  ds <- sort(unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% ts]))
  p <- lv_version_parse(todo$published[i])
  # Recorded as-is: this is a statement about what was published, not a tick.
  ver <- structure(list(
    version = todo$published[i], previous = NA_character_,
    publication = p[["publication"]], dataset = p[["dataset"]], metadata = p[["metadata"]],
    reason = "seeded from the published output directory",
    n_datasets = length(ds), datasets = ds, added = character(), removed = character(),
    dataset_set_hash = lv_dataset_set_hash(ds)), class = "lv_version")
  lv_version_append(store, comp, ver, run_id = "seed-versions",
                    notes = sprintf("baseline from %s", file.path(html, comp, todo$published[i])))
  cat(sprintf("  %-24s %s (%d datasets)\n", comp, todo$published[i], length(ds)))
}
cat(sprintf("\nseeded %d\n", nrow(todo)))
