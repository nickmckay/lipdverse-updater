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
# Two records exist and they disagree for five compilations. The published
# directories under html/<compilation>/<version>/ are authoritative, per Nick:
# they are the actual backups, so they are what matters. The versioning
# spreadsheet is not used for the version itself, but it is the only record of
# *when* each compilation last ran, which is exactly what decides whether a
# baseline should be seeded from the files or from the sheet -- a compilation
# whose files have kept moving while its sheet sat untouched for years needs the
# opposite choice from hydroclimate2k. So the date is carried into the ledger.
#
# The dataset set stored alongside is the compilation's membership now, which is
# what the next run compares against to decide whether B or C ticks.
VERSION_SHEET <- "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY"

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

# When each compilation last ran, from the versioning spreadsheet.
log <- tryCatch({
  bk <- sheet_backend_google()
  y <- sheet_read(bk, VERSION_SHEET, sheet_tabs(bk, VERSION_SHEET)[1])
  y <- y[, !duplicated(names(y)), drop = FALSE]
  y |> mutate(logged = paste(publication, dataset, metadata, sep = "_")) |>
    group_by(project) |>
    slice_max(order_by = versionCreated, n = 1, with_ties = FALSE) |>
    ungroup() |> select(compilation = project, logged, last_run = versionCreated)
}, error = function(e) { cli::cli_alert_warning("Versioning sheet unavailable: {conditionMessage(e)}"); NULL })

rows <- lapply(comps, function(comp) {
  ts <- lv_compilation_timeseries(idx, comp)
  ds <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% ts])
  tibble::tibble(compilation = comp, published = latest(comp),
                 in_ledger = lv_version_current(store, comp) %||% NA_character_,
                 members = length(ts), datasets = length(ds))
})
x <- bind_rows(rows)
if (!is.null(log)) {
  x <- left_join(x, log, by = "compilation")
  d <- x[!is.na(x$logged) & !is.na(x$published) & x$logged != x$published, , drop = FALSE]
  if (nrow(d)) {
    cat(sprintf("\n%d compilation%s where the spreadsheet disagrees with the directories.\n",
                nrow(d), if (nrow(d) == 1) "" else "s"))
    cat("The directories win: they are the actual backups.\n")
    print(as.data.frame(d[, c("compilation", "published", "logged")]), right = FALSE)
    cat("\n")
  }
}
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
  when <- if ("last_run" %in% names(todo)) todo$last_run[i] else NA_character_
  lv_version_append(store, comp, ver, run_id = "seed-versions",
                    notes = sprintf("baseline from %s%s",
                                    file.path(html, comp, todo$published[i]),
                                    if (is.na(when)) "" else sprintf("; last run %s", when)))
  cat(sprintf("  %-24s %-8s %-24s %d datasets\n", comp, todo$published[i],
              if (is.na(when)) "(date unknown)" else substr(when, 1, 19), length(ds)))
}
cat(sprintf("\nseeded %d\n", nrow(todo)))
