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

# Two different sets, and conflating them is the whole point of this stage.
#
#   considered  datasets the curator has not excluded in datasetsInCompilation.
#               Every timeseries of these appears in the QC tab, which is how a
#               candidate becomes visible.
#   members     timeseries actually carrying an inCompilation entry.
#
# The QC tab is scoped to `considered`; the compilation is `members`.
mem <- lv_compilation_datasets(cfg, bk, idx)
ds  <- mem$datasets
ts  <- idx$timeseries$TSid[idx$timeseries$dataSetName %in% ds]
members <- lv_compilation_timeseries(idx, comp)
cat(sprintf("considered  : %d datasets, %d timeseries\n", length(ds), length(ts)))
cat(sprintf("members     : %d timeseries in %d datasets\n", length(members),
            dplyr::n_distinct(idx$timeseries$dataSetName[idx$timeseries$TSid %in% members])))
if (length(mem$missing)) {
  cat(sprintf("not in db   : %d dataset%s named by the sheet\n",
              length(mem$missing), if (length(mem$missing) == 1) "" else "s"))
}
# A member whose dataset the curator has since excluded is a contradiction the
# run must not silently act on either way.
orphan <- setdiff(members, ts)
if (length(orphan)) {
  cat(sprintf("orphaned    : %d member%s whose dataset is excluded in the membership tab\n",
              length(orphan), if (length(orphan) == 1) "" else "s"))
}

base  <- qc_state_current(store, comp)
sheet <- qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)
frame <- qc_frame(db, datasets = ds, progress = FALSE)
frame <- frame[frame$tsid %in% ts, , drop = FALSE]
# Membership is not stored as a field, so the file-side view is derived.
frame <- dplyr::bind_rows(frame, lv_membership_frame(idx, comp, ts))
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
write_cells <- plan$cells[plan$cells$resolution == "sheet" &
                          plan$cells$field != "inThisCompilation", , drop = FALSE]
cat(sprintf("\nto write    : %d cell%s from the sheet\n", nrow(write_cells),
            if (nrow(write_cells) == 1) "" else "s"))

# ---- apply -----------------------------------------------------------------

if (dir.exists(stage)) unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)

# Membership first, and from the merged plan rather than the raw sheet, so an
# admission is subject to the same rules and leaves the same events as any
# other curator edit.
mplan <- plan$cells[plan$cells$field == "inThisCompilation" &
                    plan$cells$resolution %in% c("sheet", "converged"), , drop = FALSE]
mres <- lv_apply_membership(mplan, idx, comp, dir = db, out = stage, progress = FALSE)
if (length(mres$added) || length(mres$removed)) {
  cat(sprintf("membership  : +%d, -%d across %d dataset%s\n",
              length(mres$added), length(mres$removed), length(mres$datasets),
              if (length(mres$datasets) == 1) "" else "s"))
}
if (lv_n_issues(mres$issues, "error")) stop("membership produced errors; not promoting")

if (nrow(write_cells)) {
  # Point the index at staging for datasets membership already rewrote, so the
  # two stages compose into one file. Reading those from the database again
  # would silently drop the membership change when this stage writes the same
  # filename.
  aidx <- idx
  if (length(mres$datasets)) {
    hit <- match(mres$datasets, aidx$datasets$fileDataSetName)
    aidx$datasets$path[hit] <- fs::path(stage, paste0(mres$datasets, ".lpd"))
  }
  iss <- lv_apply_qc(write_cells, db, stage, index = aidx, progress = FALSE)
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


# ---- push the sheet --------------------------------------------------------
#
# This is what closes the loop on membership. A dataset the curator set TRUE in
# datasetsInCompilation has all of its timeseries in scope from this run on, so
# they appear here -- carrying inThisCompilation = FALSE, which is the prompt to
# admit them one by one.

# Scoped to the considered datasets. The store keeps history for everything it
# has ever seen, but a dataset the curator has since set FALSE must drop out of
# the tab -- limiting what a lead has to look at is half the point of the
# membership tab.
push_state <- state[state$tsid %in% ts, , drop = FALSE]
wide <- qc_cells_to_sheet(push_state, registry = lv_qc_fields())
new_rows <- setdiff(wide$TSid, sheet$tsid)
new_cols <- setdiff(names(wide), c("TSid", unique(lv_display_field(sheet$field, lv_qc_fields()))))
cat(sprintf("\nsheet       : %d row%s, %d new row%s, %d new column%s\n",
            nrow(wide), if (nrow(wide) == 1) "" else "s",
            length(new_rows), if (length(new_rows) == 1) "" else "s",
            length(new_cols), if (length(new_cols) == 1) "" else "s"))

if (commit) {
  ev <- qc_diff_to_events(base, state, source = "sheet")
  qc_store_append(store, comp, ev, run_id = run)
  cat(sprintf("store       : appended %d event%s\n", nrow(ev),
              if (nrow(ev) == 1) "" else "s"))
  # Full rewrite whenever the shape changes; a patch cannot add rows.
  qc_sheet_push(push_state, bk, cfg$qc_sheet_id, cfg$qc_tabs$qc,
                mode = if (length(new_rows) || length(new_cols)) "full" else "patch",
                dry_run = FALSE)
  cat("sheet       : pushed\n")
} else {
  n <- nrow(qc_diff_to_events(base, state, source = "sheet"))
  cat(sprintf("store       : would append %d event%s\n", n, if (n == 1) "" else "s"))
  cat("sheet       : would push\n")
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
