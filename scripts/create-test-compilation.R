#!/usr/bin/env Rscript
#
# Stand up the `lipdverseTest` compilation: membership in the files, and a QC
# sheet to drive it.
#
#   ./scripts/create-test-compilation.R --dry-run
#   ./scripts/create-test-compilation.R --stage        # write staging only
#   ./scripts/create-test-compilation.R --promote            # verify only
#   ./scripts/create-test-compilation.R --promote --commit   # write
#   ./scripts/create-test-compilation.R --sheet        # create the QC sheet
#
# The steps are separate on purpose. Membership is a write to the live
# database, so it goes through staging and `lv_promote(partial = TRUE)` like
# any other write, with a snapshot taken first. The sheet is a one-time
# creation whose id has to be recorded in `inst/extdata/compilations.tsv`, and
# re-running it would mint a second sheet, so it never happens implicitly.
#
# `lipdverseTest` exists to be run often. It is chosen for structural coverage
# (see scripts/select-test-compilation.R) rather than for scientific meaning,
# and it touches shared files only additively, by adding one `inCompilation`
# entry per column.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) v[1] else d }
comp    <- getarg("compilation", "lipdverseTest")
version <- getarg("version", "1_0_0")
sel     <- getarg("selection", "review/test-compilation.csv")
stage   <- path.expand(getarg("stage-dir", "~/lipdverse-staging/lipdverseTest"))
do_stage   <- "--stage" %in% args
do_promote <- "--promote" %in% args
do_sheet   <- "--sheet" %in% args
if (!do_stage && !do_promote && !do_sheet) cat("dry run: nothing will be written\n\n")

db  <- lv_path("database")
tsids <- read_csv(sel, col_types = cols(.default = col_character()), progress = FALSE)$TSid
cat(sprintf("compilation : %s (version %s)\n", comp, version))
cat(sprintf("selection   : %s -- %d TSids\n", sel, length(tsids)))

idx <- lv_db_index(lv_scan(db), cache = TRUE)
in_db <- tsids %in% idx$timeseries$TSid
ds <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% tsids])
cat(sprintf("resolved    : %d TSids in %d datasets (%d not found)\n",
            sum(in_db), length(ds), sum(!in_db)))

# ---- membership ------------------------------------------------------------

if (do_stage) {
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  r <- lv_create_compilation(tsids, comp, version, dir = db, out = stage, index = idx)
  cat(sprintf("\nstaged      : %d file%s, %d membership entries placed\n",
              length(list.files(stage, "[.]lpd$")),
              if (length(list.files(stage, "[.]lpd$")) == 1) "" else "s", r$placed))
  # as_tibble first: count() keeps the lv_issues class, so print.lv_issues
  # re-tallies the summary and reports its own row count instead of n.
  if (nrow(r$issues)) print(as.data.frame(count(tibble::as_tibble(r$issues), check, severity)))
}

if (do_promote) {
  stopifnot(dir.exists(stage))
  snap <- system2("scripts/snapshot-database.sh", stdout = TRUE)
  cat("\n", paste(tail(snap, 2), collapse = "\n"), "\n", sep = "")
  # partial: staging holds only the compilation's datasets, so the thousands of
  # files it does not touch must not read as deletions.
  rec <- lv_promote(stage, db, run_id = paste0("create-", comp), partial = TRUE,
                    dry_run = !("--commit" %in% args))
  if (!"--commit" %in% args) cat("\npass --commit to write.\n")
  print(rec)
}

# ---- QC sheet --------------------------------------------------------------

if (do_sheet) {
  cells <- qc_frame(db, datasets = ds)
  cells <- cells[cells$tsid %in% tsids, , drop = FALSE]
  tabs  <- lv_compilation_sheet(cells, idx)
  cat(sprintf("\nQC tab      : %d rows x %d columns\n", nrow(tabs$QC), ncol(tabs$QC)))
  cat(sprintf("members tab : %d datasets\n", nrow(tabs$datasetsInCompilation)))

  id <- sheet_create(sheet_backend_google(), paste0(comp, " QC"), tabs)
  cat(sprintf("\ncreated sheet: %s\n", id))
  cat(sprintf("  https://docs.google.com/spreadsheets/d/%s\n", id))
  cat("\nAdd to inst/extdata/compilations.tsv:\n")
  cat(sprintf("  %s\t%s\t\tyear\tdatabase\n", comp, id))
} else if (!do_stage && !do_promote) {
  cells <- qc_frame(db, datasets = ds)
  cells <- cells[cells$tsid %in% tsids, , drop = FALSE]
  tabs  <- lv_compilation_sheet(cells, idx)
  cat(sprintf("\nwould create a QC tab of %d rows x %d columns, %d datasets\n",
              nrow(tabs$QC), ncol(tabs$QC), nrow(tabs$datasetsInCompilation)))
}
