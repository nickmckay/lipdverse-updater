#!/usr/bin/env Rscript
#
# Bring a directory of contributed LiPD files into the database.
#
#   ./scripts/ingest-batch.R ~/incoming                              # dry run
#   ./scripts/ingest-batch.R ~/incoming --compilation=hydroclimate2k
#   ./scripts/ingest-batch.R ~/incoming --compilation=hydroclimate2k --commit
#
# Replaces prepareAndAddBatch() + addLipdBatchToDatabase() +
# commitBatchToDatabase(). Same two-phase shape -- stage, review, commit -- but
# every stage reports before it acts, nothing is written outside staging until
# --commit, and the commit itself goes through lv_promote() with a snapshot,
# per-file verification and a rollback path.
#
# Stages, in order:
#
#   validate      names and required metadata; errors exclude a file
#   identity      TSids and datasetIds, preserving them for genuine updates
#   apply         write the resolved identity to staging, verify the result
#   standardize   six vocabulary fields, against the pinned tables
#   duplicates    screen the staged batch against the whole database
#   promote       lv_promote(partial = TRUE)
#   membership    add the new datasets to datasetsInCompilation as TRUE
#
# Compilation membership is deliberately NOT written into the files. A dataset
# joins a compilation the same way it always does: TRUE in datasetsInCompilation
# puts its timeseries in the QC tab with inThisCompilation = FALSE, and the lead
# admits them there. One path, and datasets with no compilation work for free.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) path.expand(v[1]) else d }
inc    <- args[!grepl("^--", args)][1]
comp   <- getarg("compilation")
commit <- "--commit" %in% args
db     <- getarg("db", lv_path("database"))
work   <- getarg("work", path.expand("~/lipdverse-staging"))

if (is.na(inc)) stop("usage: ingest-batch.R <directory> [--compilation=NAME] [--commit]")
inc <- path.expand(inc)
run <- lv_run_id()
rundir <- lv_run_dir(run)
stage_id <- fs::path(work, paste0("ingest-", run, "-identity"))
stage_std <- fs::path(work, paste0("ingest-", run, "-standardized"))

cat(sprintf("incoming    : %s\ncompilation : %s\nrun_id      : %s\nmode        : %s\n\n",
            inc, comp %||% "(none)", run, if (commit) "COMMIT" else "dry run"))

idx <- lv_db_index(lv_scan(db), cache = TRUE)
n_in <- length(list.files(inc, "[.]lpd$"))
cat(sprintf("files       : %d\ndatabase    : %d datasets\n\n", n_in, nrow(idx$datasets)))

# ---- validate --------------------------------------------------------------

val <- lv_ingest_validate(inc, idx, progress = FALSE)
write_csv(val, fs::path(rundir, "validation.csv"), na = "")
if (nrow(val)) {
  cat("validation:\n")
  print(as.data.frame(count(tibble::as_tibble(val), severity, check, sort = TRUE)), right = FALSE)
}
blocked <- unique(val$path[val$severity == "error"])
if (length(blocked)) {
  cat(sprintf("\n%d file%s excluded by a validation error:\n", length(blocked),
              if (length(blocked) == 1) "" else "s"))
  cat(paste0("  ", blocked), sep = "\n")
}
cat(sprintf("\nproceeding with %d of %d file%s\n", n_in - length(blocked), n_in,
            if (n_in == 1) "" else "s"))
if (n_in - length(blocked) == 0) { cat("\nnothing to ingest.\n"); quit(save = "no") }

# ---- identity --------------------------------------------------------------

scan <- lv_ingest_scan(inc, progress = FALSE)
scan <- scan[!scan$file %in% blocked, , drop = FALSE]
plan <- lv_ingest_identity(scan, idx)
write_csv(plan, fs::path(rundir, "identity-plan.csv"), na = "")
cat("\nidentity:\n")
print(as.data.frame(count(tibble::as_tibble(plan), action, reason, sort = TRUE)), right = FALSE)

upd <- unique(plan$dataSetName[grepl("^update", plan$reason)])
if (length(upd)) cat(sprintf("\ntreated as updates: %s\n", paste(upd, collapse = ", ")))

# ---- apply and standardize -------------------------------------------------

if (fs::dir_exists(stage_id)) fs::dir_delete(stage_id)
res <- lv_ingest_apply(plan, inc, stage_id, idx, progress = FALSE, compilation = comp)
cat(sprintf("\nstaged      : %d, skipped: %d\n", length(res$staged), length(res$skipped)))
if (nrow(res$issues)) {
  print(as.data.frame(res$issues[, c("check", "severity", "message", "dataSetName")]), right = FALSE)
}

if (fs::dir_exists(stage_std)) fs::dir_delete(stage_std)
std <- lv_ingest_standardize(stage_id, stage_std, progress = FALSE)
cat(sprintf("standardize : %d value%s changed, %d unmatched (vocab pin %s)\n",
            nrow(std$changes), if (nrow(std$changes) == 1) "" else "s",
            nrow(std$issues), substr(std$pin, 1, 8)))
write_csv(std$changes, fs::path(rundir, "standardization.csv"), na = "")
write_csv(std$issues, fs::path(rundir, "unknown-vocabulary.csv"), na = "")
if (nrow(std$changes)) {
  print(as.data.frame(head(count(std$changes, field, rule, sort = TRUE), 8)), right = FALSE)
}

# ---- duplicates ------------------------------------------------------------

dbh <- lv_value_hashes(db, cache = fs::path(lv_path("state"), "cache", "value-hashes.rds"),
                       progress = FALSE)
inh <- lv_value_hashes(stage_std, progress = FALSE)
dup <- lv_duplicate_screen(inh, dbh, idx)
write_csv(dup, fs::path(rundir, "duplicates.csv"), na = "")
cat(sprintf("\nduplicates  : %d candidate%s\n", nrow(dup), if (nrow(dup) == 1) "" else "s"))
if (nrow(dup)) {
  print(as.data.frame(count(dup, disposition, name = "pairs")), right = FALSE)
  for (i in seq_len(min(nrow(dup), 6))) {
    cat(sprintf("  %-34s %s\n", dup$new[i], dup$recommendation[i]))
  }
  # An incoming file that duplicates something already held, under a different
  # name, should not be added as a second copy.
  block <- dup$new[dup$disposition == "already_present" & !dup$same_name]
  if (length(block)) {
    cat(sprintf("\n%d file%s would add a second copy of an existing record. Review before committing.\n",
                length(block), if (length(block) == 1) "" else "s"))
  }
}

# ---- promote ---------------------------------------------------------------

cat("\n")
if (commit) system2("scripts/snapshot-database.sh", stdout = TRUE)
rec <- lv_promote(stage_std, db, run_id = run, partial = TRUE, dry_run = !commit)
print(rec)

# ---- membership ------------------------------------------------------------

if (!is.null(comp)) {
  cfg <- lv_config(comp)
  bk <- sheet_backend_google()
  tab <- sheet_read(bk, cfg$qc_sheet_id, cfg$qc_tabs$datasets)
  tab <- tab[, !duplicated(names(tab)) & nzchar(names(tab)) & !is.na(names(tab)), drop = FALSE]
  nm <- intersect(c("dsn", "dataSetName"), names(tab))[1]

  staged_ds <- sub("\\.lpd$", "", list.files(stage_std, "[.]lpd$"))
  add <- setdiff(staged_ds, tab[[nm]])
  cat(sprintf("\nmembership  : %d of %d staged dataset%s not yet in %s\n",
              length(add), length(staged_ds), if (length(staged_ds) == 1) "" else "s",
              cfg$qc_tabs$datasets))
  if (length(add) && commit) {
    ids <- lv_db_index(lv_scan(stage_std, cache = FALSE), cache = FALSE)$datasets
    new <- tibble::tibble(dsn = add,
                          dsid = ids$datasetId[match(add, ids$fileDataSetName)],
                          inComp = "TRUE", instructions = LV_MEMBERSHIP_INSTRUCTIONS)
    names(new)[1] <- nm
    sheet_write(bk, cfg$qc_sheet_id, cfg$qc_tabs$datasets,
                dplyr::bind_rows(tab, new[, intersect(names(tab), names(new))]))
    cat(sprintf("added %d dataset%s as TRUE\n", length(add), if (length(add) == 1) "" else "s"))
  } else if (length(add)) {
    cat("would add them as TRUE; they then appear in the QC tab with inThisCompilation = FALSE\n")
  }
}

cat(sprintf("\nreports: %s\n", rundir))
if (!commit) cat("dry run: nothing written outside staging. Pass --commit to ingest.\n")
