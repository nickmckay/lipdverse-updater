#!/usr/bin/env Rscript
#
# Run one compilation through the whole pipeline.
#
#   ./scripts/run-compilation.R lipdverseTest            # dry run
#   ./scripts/run-compilation.R lipdverseTest --commit   # write files and store
#
# scan -> frame -> sheet pull -> merge -> store -> apply -> stage -> promote,
# then the two invariants that hold regardless of what the legacy pipeline did:
#
#   idempotence          a second run against unchanged inputs changes nothing
#   no collateral change files outside the compilation are untouched
#
# Both are asserted here rather than eyeballed, because both have already
# caught real bugs that field-by-field diffs reported as clean.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
comp   <- args[!grepl("^--", args)][1]
if (is.na(comp)) stop("usage: run-compilation.R <compilation> [--commit]")
commit <- "--commit" %in% args
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) v[1] else d }
stage  <- path.expand(getarg("stage-dir", file.path("~/lipdverse-staging", paste0("run-", comp))))

cfg   <- lv_config(comp)
db    <- lv_path("database")
store <- qc_store()
bk    <- sheet_backend_google()
run   <- lv_run_id()
cat(sprintf("compilation : %s\nsheet       : %s\nrun_id      : %s\nmode        : %s\n\n",
            comp, cfg$qc_sheet_id, run, if (commit) "COMMIT" else "dry run"))

# ---- inputs ----------------------------------------------------------------

idx <- lv_db_index(lv_scan(db), cache = TRUE)

# Scope comes from the files, where membership actually lives, and is per
# column. Taking every column of every member dataset instead would widen the
# run well past the compilation: lipdverseTest's 161 datasets hold 4,370
# columns, of which 2,689 are members.
ts <- lv_compilation_timeseries(idx, comp)
ds <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% ts])
cat(sprintf("members     : %d datasets, %d timeseries\n", length(ds), length(ts)))

# The sheet's membership tab is a derived view, so disagreement with the files
# is worth reporting rather than resolving silently.
mem <- lv_compilation_datasets(cfg, bk, idx)
drift <- c(setdiff(mem$datasets, ds), setdiff(ds, mem$datasets))
if (length(drift) || length(mem$missing)) {
  cat(sprintf("sheet drift : %d dataset%s differ from the files, %d named but absent\n",
              length(drift), if (length(drift) == 1) "" else "s", length(mem$missing)))
}

base  <- qc_state_current(store, comp)
sheet <- qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)
frame <- qc_frame(db, datasets = ds, progress = FALSE)
frame <- frame[frame$tsid %in% ts, , drop = FALSE]
cat(sprintf("base        : %d cells\nsheet       : %d cells\nframe       : %d cells\n",
            nrow(base), nrow(sheet), nrow(frame)))

# ---- merge -----------------------------------------------------------------

plan <- qc_merge(base, sheet, frame)
print(plan)
# Aborts on a key-field disagreement, or on any unresolved conflict under the
# compilation's strict setting, after writing the report.
qc_plan_check(plan, path = file.path(lv_run_dir(run), "conflicts.csv"))

state <- qc_plan_state(plan)

# Only cells the curator's sheet moved need writing back into the files.
# "file" and "converged" mean the file already holds the resolved value, and
# rewriting them would churn 161 files to no effect.
write_cells <- plan$cells[plan$cells$resolution == "sheet", , drop = FALSE]
cat(sprintf("\nto write    : %d cell%s from the sheet\n", nrow(write_cells),
            if (nrow(write_cells) == 1) "" else "s"))

# ---- apply -----------------------------------------------------------------

if (dir.exists(stage)) unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)

if (nrow(write_cells)) {
  iss <- lv_apply_qc(write_cells, db, stage, index = idx, progress = FALSE)
  if (nrow(iss)) print(as.data.frame(count(tibble::as_tibble(iss), check, severity)))
  if (lv_n_issues(iss, "error")) stop("apply produced errors; not promoting")
}
staged <- list.files(stage, "[.]lpd$")
cat(sprintf("staged      : %d file%s\n", length(staged), if (length(staged) == 1) "" else "s"))

# ---- invariant: no collateral change ---------------------------------------

if (length(staged)) {
  touched <- sub("[.]lpd$", "", staged)
  outside <- setdiff(touched, ds)
  cat(sprintf("collateral  : %d staged file%s outside the compilation\n",
              length(outside), if (length(outside) == 1) "" else "s"))
  if (length(outside)) stop("staged files outside the compilation: ",
                            paste(head(outside, 5), collapse = ", "))
}

# ---- promote ---------------------------------------------------------------

if (length(staged)) {
  if (commit) system2("scripts/snapshot-database.sh", stdout = TRUE)
  rec <- lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = !commit)
  print(rec)
}

if (commit) {
  ev <- qc_diff_to_events(base, state, source = "sheet")
  qc_store_append(store, comp, ev, run_id = run)
  cat(sprintf("\nstore       : appended %d event%s\n", nrow(ev),
              if (nrow(ev) == 1) "" else "s"))
} else {
  cat(sprintf("\nstore       : would append %d event%s\n",
              nrow(qc_diff_to_events(base, state, source = "sheet")),
              if (nrow(qc_diff_to_events(base, state, source = "sheet")) == 1) "" else "s"))
}

# ---- invariant: idempotence ------------------------------------------------

if (commit) {
  cat("\n-- second pass --\n")
  base2  <- qc_state_current(store, comp)
  frame2 <- qc_frame(db, datasets = ds, progress = FALSE)
  frame2 <- frame2[frame2$tsid %in% ts, , drop = FALSE]
  plan2  <- qc_merge(base2, qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc), frame2)
  n2 <- plan2$summary$n_changed
  cat(sprintf("changes on a second run: %d\n", n2))
  if (n2 != 0) {
    print(as.data.frame(head(count(tibble::as_tibble(plan2$changes), field, resolution), 20)))
    stop("not idempotent: a second run against unchanged inputs still changes cells")
  }
  cat("idempotent.\n")
}
