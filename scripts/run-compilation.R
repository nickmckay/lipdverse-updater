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

# Report text corruption, never repair it here. Repairing means writing to a
# shared sheet, which is not something an unattended run should do. Reporting
# means a run cannot quietly carry `‚Äì` and `Œ¥` into the files, which is how
# they reached the database in the first place.
moji <- lv_detect_mojibake(sheet$value)
if (any(moji$is_mojibake)) {
  cat(sprintf("\nWARNING: %d sheet cell%s carry mis-decoded text (e.g. %s)\n",
              sum(moji$is_mojibake), if (sum(moji$is_mojibake) == 1) "" else "s",
              substr(moji$input[which(moji$is_mojibake)[1]], 1, 60)))
  cat("  Repair with lv_repair_mojibake(cfg, bk, dry_run = FALSE), then re-run.\n")
}
frame <- qc_frame(db, datasets = ds, progress = FALSE)
frame <- frame[frame$tsid %in% ts, , drop = FALSE]
# Membership is not stored as a field, so the file-side view is derived.
frame <- dplyr::bind_rows(frame, lv_membership_frame(idx, comp, ts))
cat(sprintf("base        : %d cells\nsheet       : %d cells\nframe       : %d cells\n",
            nrow(base), nrow(sheet), nrow(frame)))

# A dataset-level field repeats across every row of its dataset in the sheet, so
# the rows should agree. Where they do not, one of them is wrong -- CO07CAFR
# carries two pub1_citation values differing by an en-dash mangled into three
# characters. The merge cannot see this: only the row that differs from the
# baseline becomes a change, so by the time cells reach lv_apply_qc there is a
# single value and nothing to compare. Checked here, against the sheet itself.

# From the registry's cardinality, not from where the field is stored. Those are
# different questions: minYear sits at the dataset root in the file but varies
# per timeseries, so its rows are supposed to differ. Fields with no declared
# cardinality (44 of them) are left alone rather than guessed at.
reg <- lv_qc_fields()
ds_level <- reg$qc_name[reg$cardinality %in% "dataset"]
sd <- sheet[sheet$field %in% ds_level & !is.na(sheet$value), , drop = FALSE]
sd$dataSetName <- unname(stats::setNames(idx$timeseries$dataSetName,
                                         idx$timeseries$TSid)[sd$tsid])
sd <- sd[!is.na(sd$dataSetName), , drop = FALSE]
incon <- sd |>
  dplyr::group_by(dataSetName, field) |>
  dplyr::summarise(n = dplyr::n_distinct(value), .groups = "drop") |>
  dplyr::filter(n > 1)
if (nrow(incon)) {
  cat(sprintf("\ninconsistent: %d dataset-level field%s differ between rows of the same dataset\n",
              nrow(incon), if (nrow(incon) == 1) "" else "s"))
  print(as.data.frame(head(dplyr::count(incon, field, sort = TRUE), 6)), right = FALSE)
  readr::write_csv(dplyr::semi_join(sd, incon, by = c("dataSetName", "field")) |>
                     dplyr::arrange(dataSetName, field),
                   file.path(lv_run_dir(run), "inconsistent-dataset-fields.csv"), na = "")
}

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

# A curator can type anything into a cell. Quarantine values the files cannot
# legally hold, rather than letting one of them abort a several-hundred-file
# promote at the verification gate.
bad <- lv_validate_values(write_cells)
if (nrow(bad)) {
  cat(sprintf("\nrejected    : %d cell%s the files cannot hold\n", nrow(bad),
              if (nrow(bad) == 1) "" else "s"))
  print(as.data.frame(bad[, c("check", "TSid", "field", "value")]), right = FALSE)
  readr::write_csv(bad, file.path(lv_run_dir(run), "rejected-values.csv"), na = "")
  write_cells <- dplyr::anti_join(write_cells, bad[, c("TSid", "field")],
                                  by = c("tsid" = "TSid", "field" = "field"))
}
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

# ---- changelog -------------------------------------------------------------
#
# Per dataset, comparing what is staged against what is live. Each entry carries
# the compilation and run_id, which createChangelog() never recorded: 56% of
# datasets belong to two or more compilations and they share the fields stored
# in the file, so without those an entry says what changed but not which run
# did it.

if (length(staged)) {
  cl <- list()
  for (f in staged) {
    dsn <- sub("\\.lpd$", "", f)
    live <- fs::path(db, f)
    if (!fs::file_exists(live)) next
    a <- tryCatch(suppressWarnings(lipdR::readLipd(live)), error = function(e) NULL)
    b <- tryCatch(suppressWarnings(lipdR::readLipd(fs::path(stage, f))), error = function(e) NULL)
    if (is.null(a) || is.null(b)) next
    d <- lv_changelog_diff(a, b)
    if (!nrow(d)) next
    v <- lv_changelog_next_version(lv_changelog_last_version(b))
    entry <- lv_changelog_entry(d, version = v,
                                last_version = lv_changelog_last_version(b),
                                compilation = comp, run_id = run)
    b <- lv_changelog_append(b, entry)
    lipdR::writeLipd(b, path = stage, removeNamesFromLists = TRUE)
    cl[[length(cl) + 1L]] <- dplyr::mutate(d, dataSetName = dsn, version = v)
  }
  if (length(cl)) {
    cl <- dplyr::bind_rows(cl)
    readr::write_csv(cl, file.path(lv_run_dir(run), "changelog.csv"), na = "")
    cat(sprintf("changelog   : %d change%s across %d dataset%s\n", nrow(cl),
                if (nrow(cl) == 1) "" else "s", dplyr::n_distinct(cl$dataSetName),
                if (dplyr::n_distinct(cl$dataSetName) == 1) "" else "s"))
    print(as.data.frame(head(dplyr::count(cl, category, kind, sort = TRUE), 6)), right = FALSE)
  } else {
    cat("changelog   : no recordable changes\n")
  }
}

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


# ---- version ---------------------------------------------------------------
#
# The dataset set is what decides the bump, so it is taken after membership has
# been applied: a run that admits a timeseries from a dataset already in the
# compilation changes metadata, not membership.

prev <- lv_version_current(store, comp)
# The membership this run *would* produce, taken from the plan rather than by
# re-reading the database. On a dry run nothing has been written, so re-reading
# returns the membership we started with and every run looks like a metadata
# change -- hydroclimate2k reported 0_4_1 when admitting 15 timeseries and
# removing 2 should take it to 0_5_0.
members_now <- union(setdiff(members, mres$removed), mres$added)
ds_now <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% members_now])
ds_before <- {
  m <- fs::path(store$path, "version_datasets.csv")
  if (!is.null(prev) && fs::file_exists(m)) {
    v <- readr::read_csv(m, col_types = readr::cols(.default = readr::col_character()),
                         progress = FALSE)
    v$dataset[v$compilation == comp & v$version == prev]
  } else ds_now
}
ver <- lv_tick_version(prev, ds_before, ds_now)
print(ver)

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
  lv_version_append(store, comp, ver, run_id = run,
                    db_fingerprint = lv_scan(db)$fingerprint,
                    qc_state_hash = lv_dataset_set_hash(paste(state$tsid, state$field, state$value)))
  cat(sprintf("version     : %s\n", ver$version))
  # Full rewrite whenever the shape changes; a patch cannot add rows.
  qc_sheet_push(push_state, bk, cfg$qc_sheet_id, cfg$qc_tabs$qc,
                mode = if (length(new_rows) || length(new_cols)) "full" else "patch",
                dry_run = FALSE)
  cat("sheet       : pushed\n")
} else {
  n <- nrow(qc_diff_to_events(base, state, source = "sheet"))
  cat(sprintf("store       : would append %d event%s\n", n, if (n == 1) "" else "s"))
  cat("sheet       : would push\n")
  cat(sprintf("version     : would record %s\n", ver$version))
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
