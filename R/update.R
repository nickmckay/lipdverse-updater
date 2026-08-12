#' Run one compilation through the whole pipeline
#'
#' scan -> frame -> sheet pull -> merge -> store -> apply -> stage -> promote,
#' then the two invariants that hold regardless of what the legacy pipeline did:
#'
#' \describe{
#'   \item{idempotence}{a second run against unchanged inputs changes nothing}
#'   \item{no collateral change}{files outside the compilation are untouched}
#' }
#'
#' Both are asserted rather than eyeballed, because both have already caught real
#' bugs that field-by-field diffs reported as clean.
#'
#' This lived in `scripts/run-compilation.R` until 2026-08-12, which meant the
#' most consequential code in the package -- merge, apply, promote, version,
#' changelog, export, and both invariants -- was the only part no test ever
#' touched, and the shadow harness could not call it at all.
#'
#' @section Nothing is written unless `commit`:
#' A dry run reads the sheet, reads the files, merges, and stages into a
#' scratch directory, then reports what it would do. It writes nothing to the
#' database, the store, or the sheet. That is the mode to use for anything
#' unattended.
#'
#' @param compilation Compilation name.
#' @param commit Write: promote the staged files, append to the store, push the
#'   sheet. `FALSE` reports without writing anything.
#' @param cfg An `lv_config`; defaults to the compilation's.
#' @param dir Database directory.
#' @param store A QC store.
#' @param backend A sheet backend.
#' @param stage Staging directory; a scratch path under `~/lipdverse-staging` by
#'   default. Emptied at the start of the run.
#' @param run_id Run id. A run id names one write; never reuse one.
#' @param patch Force a patch push even when there are new rows, which keeps the
#'   tab's formatting and appends the new rows unformatted at the bottom.
#' @param snapshot Take a database snapshot before promoting. Only consulted
#'   when `commit`.
#' @param export Write the canonical export. Only consulted when `commit`.
#' @param rename_sheet Retitle the QC sheet with the new version. Only consulted
#'   when `commit`.
#' @param progress Print the running report.
#' @return An `lv_update_result`.
#' @export
lv_update <- function(compilation, commit = FALSE, cfg = lv_config(compilation),
                      dir = lv_path("database"), store = qc_store(),
                      backend = sheet_backend_google(), stage = NULL,
                      run_id = lv_run_id(), patch = FALSE, snapshot = TRUE,
                      export = TRUE, rename_sheet = TRUE, progress = TRUE) {
  comp <- compilation
  db <- dir
  bk <- backend
  run <- run_id
  stage <- stage %||% path.expand(file.path("~/lipdverse-staging", paste0("run-", comp)))
  say <- function(...) if (progress) cat(...)
  show <- function(x) if (progress) print(x)

  say(sprintf("compilation : %s\nsheet       : %s\nrun_id      : %s\nmode        : %s\n\n",
              comp, cfg$qc_sheet_id, run, if (commit) "COMMIT" else "dry run"))

  # ---- inputs --------------------------------------------------------------

  idx <- lv_db_index(lv_scan(db), cache = TRUE)

  # Two different sets, and conflating them is the whole point of this stage.
  #
  #   considered  datasets the curator has not excluded in datasetsInCompilation.
  #               Every timeseries of these appears in the QC tab, which is how a
  #               candidate becomes visible.
  #   members     timeseries actually carrying an inCompilation entry.
  #
  # The QC tab is scoped to `considered`; the compilation is `members`.
  # A dataset ingested for this compilation that never reached the membership tab
  # is invisible: the QC tab is scoped to that tab, so nobody can curate it. The
  # files record which compilation minted them, so this is self-healing rather
  # than dependent on remembering at ingest time.
  #
  # Only datasets absent from the tab are read, which is a handful.
  tab_now <- sheet_read(bk, cfg$qc_sheet_id, cfg$qc_tabs$datasets)
  absent <- setdiff(idx$datasets$fileDataSetName, tab_now$dsn)
  orphans <- if (length(absent)) lv_datasets_created_by(absent, comp, db) else character()
  if (length(orphans)) {
    say(sprintf("to offer    : %d dataset%s ingested for %s but not on %s\n",
                length(orphans), if (length(orphans) == 1) "" else "s", comp,
                cfg$qc_tabs$datasets))
    for (o in utils::head(sort(orphans), 30)) say("   ", o, "\n")
    # Added before membership is read, so they are curatable in this same run
    # rather than waiting for the next one.
    lv_offer_to_compilation(orphans, cfg, bk, idx, in_compilation = TRUE,
                            dry_run = !commit)
  }

  mem <- lv_compilation_datasets(cfg, bk, idx)
  ds <- mem$datasets
  ts <- lv_qc_timeseries(idx, datasets = ds)   # paleoData only; chron is not curated here
  members <- lv_compilation_timeseries(idx, comp)
  say(sprintf("considered  : %d datasets, %d timeseries\n", length(ds), length(ts)))
  say(sprintf("members     : %d timeseries in %d datasets\n", length(members),
              dplyr::n_distinct(idx$timeseries$dataSetName[idx$timeseries$TSid %in% members])))
  if (length(mem$missing)) {
    say(sprintf("not in db   : %d dataset%s named by the sheet\n",
                length(mem$missing), if (length(mem$missing) == 1) "" else "s"))
  }
  # A member whose dataset the curator has since excluded is a contradiction the
  # run must not silently act on either way.
  orphan <- setdiff(members, ts)
  if (length(orphan)) {
    say(sprintf("orphaned    : %d member%s whose dataset is excluded in the membership tab\n",
                length(orphan), if (length(orphan) == 1) "" else "s"))
  }

  base <- qc_state_current(store, comp)
  sheet <- qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)

  # A QC sheet can carry another compilation's csm column -- sheets are copied
  # from one another, and the registry has a synonym for iso2k's certification
  # column. Held back here rather than at write time, so a cross-compilation csm
  # conflict cannot arise: a foreign cell that reaches the plan also reaches the
  # resolved state, and from there the store and the sheet push.
  #
  # The store is deliberately not scoped. A cell the store holds and the state
  # does not becomes a tombstone, so filtering the baseline would record deletions
  # of another compilation's values instead of leaving them alone.
  scoped <- lv_csm_scope(sheet, comp)
  sheet <- scoped$cells
  if (nrow(scoped$foreign)) {
    say(sprintf("\nforeign csm : %d cell%s in %d column%s belonging to another compilation; not merged\n",
                nrow(scoped$foreign), if (nrow(scoped$foreign) == 1) "" else "s",
                dplyr::n_distinct(scoped$foreign$field),
                if (dplyr::n_distinct(scoped$foreign$field) == 1) "" else "s"))
    show(as.data.frame(dplyr::count(tibble::as_tibble(scoped$foreign), .data$field)))
    readr::write_csv(scoped$foreign, file.path(lv_run_dir(run), "foreign-csm.csv"), na = "")
  }

  # Report text corruption, never repair it here. Repairing means writing to a
  # shared sheet, which is not something an unattended run should do. Reporting
  # means a run cannot quietly carry mis-decoded text into the files, which is
  # how it reached the database in the first place.
  moji <- lv_detect_mojibake(sheet$value)
  if (any(moji$is_mojibake)) {
    say(sprintf("\nWARNING: %d sheet cell%s carry mis-decoded text (e.g. %s)\n",
                sum(moji$is_mojibake), if (sum(moji$is_mojibake) == 1) "" else "s",
                substr(moji$input[which(moji$is_mojibake)[1]], 1, 60)))
    say("  Repair with lv_repair_mojibake(cfg, bk, dry_run = FALSE), then re-run.\n")
  }

  frame <- qc_frame(db, datasets = ds, progress = FALSE)
  frame <- frame[frame$tsid %in% ts, , drop = FALSE]
  # Membership is not stored as a field, so the file-side view is derived.
  frame <- dplyr::bind_rows(frame, lv_membership_frame(idx, comp, ts))
  # Compilation-specific metadata lives inside the inCompilation entry rather than
  # in the shared namespace, so qc_frame() cannot see it and it arrives separately.
  frame <- dplyr::bind_rows(frame, lv_csm_frame(db, comp, datasets = ds, tsids = ts,
                                                progress = FALSE))

  # Calculated fields are derived from the data every run, not read back. Which
  # ones is decided by the QC tab's header: a lead adds the column to ask for the
  # value, and a compilation without the column gets no calculation.
  #
  # What the files currently hold is kept aside first, because the computed cells
  # replace it in the frame -- leaving both would put two rows for the same cell
  # into the merge -- and the comparison is what says which files need rewriting.
  calcs <- lv_sheet_calculations(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)
  stored_calc <- frame[frame$field %in% calcs, , drop = FALSE]
  computed <- qc_cells_empty()
  if (length(calcs)) {
    frame <- frame[!frame$field %in% calcs, , drop = FALSE]
    computed <- lv_calculate(calcs, db, datasets = ds, tsids = ts, index = idx,
                             progress = FALSE)
    frame <- dplyr::bind_rows(frame, computed)
    say(sprintf("calculated  : %s -- %d value%s, %d empty\n",
                paste(calcs, collapse = ", "), sum(computed$present),
                if (sum(computed$present) == 1) "" else "s", sum(!computed$present)))
  } else {
    say("calculated  : no calculated columns on this sheet\n")
  }
  say(sprintf("base        : %d cells\nsheet       : %d cells\nframe       : %d cells\n",
              nrow(base), nrow(sheet), nrow(frame)))

  # A dataset-level field repeats across every row of its dataset in the sheet, so
  # the rows should agree. Where they do not, one of them is wrong. The merge
  # cannot see this: only the row that differs from the baseline becomes a change,
  # so by the time cells reach lv_apply_qc there is a single value and nothing to
  # compare. Checked here, against the sheet itself.
  #
  # From the registry's cardinality, not from where the field is stored. Those are
  # different questions: minYear varies per timeseries but is not stored per
  # timeseries. Fields with no declared cardinality are left alone.
  reg <- lv_qc_fields()
  ds_level <- reg$qc_name[reg$cardinality %in% "dataset"]
  sd <- sheet[sheet$field %in% ds_level & !is.na(sheet$value), , drop = FALSE]
  sd$dataSetName <- unname(stats::setNames(idx$timeseries$dataSetName,
                                           idx$timeseries$TSid)[sd$tsid])
  sd <- sd[!is.na(sd$dataSetName), , drop = FALSE]
  incon <- sd |>
    dplyr::group_by(.data$dataSetName, .data$field) |>
    dplyr::summarise(n = dplyr::n_distinct(.data$value), .groups = "drop") |>
    dplyr::filter(.data$n > 1)
  if (nrow(incon)) {
    say(sprintf("\ninconsistent: %d dataset-level field%s differ between rows of the same dataset\n",
                nrow(incon), if (nrow(incon) == 1) "" else "s"))
    show(as.data.frame(utils::head(dplyr::count(incon, .data$field, sort = TRUE), 6)))
    readr::write_csv(dplyr::semi_join(sd, incon, by = c("dataSetName", "field")) |>
                       dplyr::arrange(.data$dataSetName, .data$field),
                     file.path(lv_run_dir(run), "inconsistent-dataset-fields.csv"), na = "")
  }

  # ---- merge ---------------------------------------------------------------

  plan <- qc_merge(base, sheet, frame)
  show(plan)
  # Aborts on a key-field disagreement, or on any unresolved conflict under the
  # compilation's strict setting, after writing the report.
  qc_plan_check(plan, path = file.path(lv_run_dir(run), "conflicts.csv"))

  state <- qc_plan_state(plan)

  # Only cells the curator's sheet moved need writing back into the files.
  # "file" and "converged" mean the file already holds the resolved value, and
  # rewriting them would churn hundreds of files to no effect.
  write_cells <- plan$cells[plan$cells$resolution == "sheet" &
                              plan$cells$field != "inThisCompilation", , drop = FALSE]

  # A curator can type anything into a cell. Quarantine values the files cannot
  # legally hold, rather than letting one of them abort a several-hundred-file
  # promote at the verification gate.
  bad <- lv_validate_values(write_cells)
  if (nrow(bad)) {
    say(sprintf("\nrejected    : %d cell%s the files cannot hold\n", nrow(bad),
                if (nrow(bad) == 1) "" else "s"))
    show(as.data.frame(bad[, c("check", "TSid", "field", "value")]))
    readr::write_csv(bad, file.path(lv_run_dir(run), "rejected-values.csv"), na = "")
    write_cells <- lv_drop_cells(write_cells, bad)

    # Vocabulary on the sheet path. Reported, never blocking: an unattended run
    # that has merged hundreds of datasets should not stop over one cell.
    vi <- lv_check_vocabulary(write_cells, index = idx)
    if (nrow(vi)) {
      say(sprintf("\nvocabulary : %d cell%s not in the controlled vocabulary\n",
                  nrow(vi), if (nrow(vi) == 1) "" else "s"))
      show(as.data.frame(dplyr::count(tibble::as_tibble(vi), .data$field, .data$value,
                                      sort = TRUE)))
      vrev <- fs::path(lv_path("state"), "review", paste0("vocab-sheet-", comp, "-", run, ".json"))
      lv_vocab_review(vi, vrev)
      say(sprintf("  review    : %s\n", vrev))
    }
  }

  # csm goes into the inCompilation entry, not into the shared namespace, so it is
  # written by its own applier. Split after validation, so a curator's csm value is
  # screened like any other, and before apply, because lv_apply_qc() drops these
  # cells silently -- which is how they reached no file for as long as they did.
  is_csm <- lv_field_rule(write_cells$field)$role %in% "csm"
  csm_cells <- write_cells[is_csm, , drop = FALSE]
  write_cells <- write_cells[!is_csm, , drop = FALSE]

  # A recalculated value has to reach the file as well as the sheet, or it is
  # recomputed from scratch every run and the file keeps a stale number forever.
  # These are resolution "file", not "sheet", so they are collected separately --
  # and only where the computed value differs from what the file already holds, so
  # an unchanged calculation rewrites nothing.
  calc_cells <- write_cells[0, , drop = FALSE]
  if (length(calcs)) {
    now <- plan$cells[plan$cells$field %in% calcs &
                        plan$cells$resolution %in% c("file", "converged"), , drop = FALSE]
    was <- stats::setNames(stored_calc$value, paste(stored_calc$tsid, stored_calc$field))
    prev_v <- unname(was[paste(now$tsid, now$field)])
    moved <- !values_equal(dplyr::coalesce(now$value, NA_character_),
                           dplyr::coalesce(prev_v, NA_character_))
    calc_cells <- now[moved, , drop = FALSE]
    say(sprintf("recalculated: %d cell%s differ from the files (%d cleared)\n",
                nrow(calc_cells), if (nrow(calc_cells) == 1) "" else "s",
                sum(is.na(calc_cells$value))))
    write_cells <- dplyr::bind_rows(write_cells, calc_cells)
  }
  n_edits <- nrow(write_cells) - nrow(calc_cells)

  # A corrected dataSetName is a rename, not just another cell: the file takes its
  # name from its metadata, so applying it writes a file under the new name and
  # the old one has to be retired or the promote adds a duplicate.
  renames <- lv_planned_renames(write_cells, idx)
  if (nrow(renames)) {
    say(sprintf("\nrenames     : %d dataset%s renamed on the QC sheet\n", nrow(renames),
                if (nrow(renames) == 1) "" else "s"))
    show(as.data.frame(renames[, c("dataSetName", "new_name", "datasetId", "issue")]))
    if (lv_n_issues(lv_rename_issues(renames), "error")) {
      readr::write_csv(renames, file.path(lv_run_dir(run), "renames.csv"), na = "")
      cli::cli_abort("Unsafe rename; not proceeding. See {.path {file.path(lv_run_dir(run), 'renames.csv')}}.",
                     class = "lv_error_rename")
    }
  }

  say(sprintf("\nto write    : %d cell%s -- %d curator edit%s, %d recalculated, %d compilation-specific\n",
              nrow(write_cells) + nrow(csm_cells),
              if (nrow(write_cells) + nrow(csm_cells) == 1) "" else "s",
              n_edits, if (n_edits == 1) "" else "s", nrow(calc_cells), nrow(csm_cells)))

  # ---- apply ---------------------------------------------------------------

  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)

  # Membership first, and from the merged plan rather than the raw sheet, so an
  # admission is subject to the same rules and leaves the same events as any
  # other curator edit.
  mplan <- plan$cells[plan$cells$field == "inThisCompilation" &
                        plan$cells$resolution %in% c("sheet", "converged"), , drop = FALSE]
  mres <- lv_apply_membership(mplan, idx, comp, dir = db, out = stage, progress = FALSE)
  if (length(mres$added) || length(mres$removed)) {
    say(sprintf("membership  : +%d, -%d across %d dataset%s\n",
                length(mres$added), length(mres$removed), length(mres$datasets),
                if (length(mres$datasets) == 1) "" else "s"))
  }
  if (lv_n_issues(mres$issues, "error")) {
    cli::cli_abort("Membership produced errors; not promoting.", class = "lv_error_membership")
  }

  # Point the index at staging for datasets an earlier stage already rewrote, so
  # the stages compose into one file. Reading those from the database again would
  # silently drop the earlier change when the next stage writes the same filename.
  aidx <- idx
  point_at_stage <- function(dsns) {
    if (!length(dsns)) return(invisible())
    hit <- match(dsns, aidx$datasets$fileDataSetName)
    hit <- hit[!is.na(hit)]
    aidx$datasets$path[hit] <<- fs::path(stage, paste0(aidx$datasets$fileDataSetName[hit], ".lpd"))
  }
  point_at_stage(mres$datasets)

  # csm after membership and before the shared fields: it writes into the
  # inCompilation entry membership may have just created, so a timeseries admitted
  # and certified in the same run gets both.
  csm_issues <- lv_issues_empty()
  if (nrow(csm_cells)) {
    cres <- lv_apply_csm(csm_cells, aidx, comp, dir = db, out = stage, progress = FALSE)
    csm_issues <- cres$issues
    say(sprintf("csm         : %d cell%s across %d dataset%s\n", cres$n,
                if (cres$n == 1) "" else "s", length(cres$datasets),
                if (length(cres$datasets) == 1) "" else "s"))
    if (nrow(cres$issues)) {
      show(as.data.frame(dplyr::count(tibble::as_tibble(cres$issues), .data$check, .data$severity)))
    }
    if (lv_n_issues(cres$issues, "error")) {
      cli::cli_abort("csm produced errors; not promoting.", class = "lv_error_csm")
    }
    point_at_stage(cres$datasets)
  }

  apply_issues <- lv_issues_empty()
  if (nrow(write_cells)) {
    apply_issues <- lv_apply_qc(write_cells, db, stage, index = aidx, progress = FALSE)
    if (nrow(apply_issues)) {
      show(as.data.frame(dplyr::count(tibble::as_tibble(apply_issues), .data$check, .data$severity)))
    }
    if (lv_n_issues(apply_issues, "error")) {
      cli::cli_abort("Apply produced errors; not promoting.", class = "lv_error_apply")
    }
  }
  staged <- list.files(stage, "[.]lpd$")
  say(sprintf("staged      : %d file%s\n", length(staged), if (length(staged) == 1) "" else "s"))

  # ---- changelog -----------------------------------------------------------
  #
  # Per dataset, comparing what is staged against what is live. Each entry carries
  # the compilation and run_id, which createChangelog() never recorded: 56% of
  # datasets belong to two or more compilations and they share the fields stored
  # in the file, so without those an entry says what changed but not which run
  # did it.

  changelog <- NULL
  if (length(staged)) {
    cl <- list()
    for (f in staged) {
      dsn <- sub("\\.lpd$", "", f)
      # A renamed dataset has no live file under its new name; its history is the
      # old one, so the diff is taken against that. Without this the rename shows
      # up as a dataset with no changelog entry at all.
      was <- renames$dataSetName[match(dsn, renames$new_name)]
      live <- fs::path(db, if (length(was) && !is.na(was)) paste0(was, ".lpd") else f)
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
      changelog <- dplyr::bind_rows(cl)
      readr::write_csv(changelog, file.path(lv_run_dir(run), "changelog.csv"), na = "")
      say(sprintf("changelog   : %d change%s across %d dataset%s\n", nrow(changelog),
                  if (nrow(changelog) == 1) "" else "s",
                  dplyr::n_distinct(changelog$dataSetName),
                  if (dplyr::n_distinct(changelog$dataSetName) == 1) "" else "s"))
      show(as.data.frame(utils::head(dplyr::count(changelog, .data$category, .data$kind,
                                                  sort = TRUE), 6)))
    } else {
      say("changelog   : no recordable changes\n")
    }
  }

  # ---- invariant: no collateral change -------------------------------------

  if (length(staged)) {
    touched <- sub("[.]lpd$", "", staged)
    # A renamed dataset lands under a name the membership tab does not know yet,
    # so it is expected outside the compilation's dataset list. An *undeclared*
    # name still stops the run -- that is what caught the rename in the first
    # place, before renaming was supported.
    outside <- setdiff(touched, c(ds, renames$new_name))
    say(sprintf("collateral  : %d staged file%s outside the compilation\n",
                length(outside), if (length(outside) == 1) "" else "s"))
    if (length(outside)) {
      cli::cli_abort("Staged files outside the compilation: {.val {utils::head(outside, 5)}}",
                     class = "lv_error_collateral")
    }
  }

  # ---- promote -------------------------------------------------------------

  receipt <- NULL
  if (length(staged)) {
    if (commit && snapshot && file.exists("scripts/snapshot-database.sh")) {
      system2("scripts/snapshot-database.sh", stdout = TRUE)
    }
    receipt <- lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = !commit,
                          delete = renames$file_old)
    show(receipt)
  }

  # ---- version -------------------------------------------------------------
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
  show(ver)

  # ---- push the sheet ------------------------------------------------------
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
  wide <- qc_cells_to_sheet(push_state, registry = reg)
  # Against the sheet's own rows, not the pulled cells: a TSid with no cells in
  # the pull is still a row on the sheet, so comparing against `sheet$tsid`
  # reported rows as new that were already there.
  sheet_rows <- tryCatch(sheet_read(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc), error = function(e) NULL)
  existing_rows <- if (!is.null(sheet_rows) && "TSid" %in% names(sheet_rows)) sheet_rows$TSid else sheet$tsid
  new_rows <- setdiff(wide$TSid, existing_rows)
  # Fields the store holds that the sheet has no column for. They are NOT written:
  # a column is added by a curator on the sheet, never by a run.
  no_column <- setdiff(names(wide), c("TSid", unique(lv_display_field(sheet$field, reg))))
  say(sprintf("\nsheet       : %d row%s, %d new row%s, %d stored field%s with no column (not written)\n",
              nrow(wide), if (nrow(wide) == 1) "" else "s",
              length(new_rows), if (length(new_rows) == 1) "" else "s",
              length(no_column), if (length(no_column) == 1) "" else "s"))
  # Name them while the list is short. A new row is a timeseries a curator has
  # not seen, so which ones matters more than how many.
  if (length(new_rows) && length(new_rows) <= 40) {
    nr <- idx$timeseries[idx$timeseries$TSid %in% new_rows, , drop = FALSE]
    say("  new rows:\n")
    for (i in seq_len(nrow(nr))) {
      say(sprintf("    %-34s %-16s %s\n", nr$dataSetName[i], nr$variableName[i], nr$TSid[i]))
    }
  }
  if (patch && length(new_rows)) {
    say(sprintf("  patch: %d new row%s will be appended at the bottom, unformatted;\n",
                length(new_rows), if (length(new_rows) == 1) "" else "s"))
    say("         existing rows and their colour coding are untouched.\n")
  }

  # The membership tab names datasets by name, so after a rename the old entry
  # matches nothing and the dataset drops out of the considered set on the next
  # run -- looking like the compilation losing a dataset for no reason.
  renamed_rows <- NULL
  if (nrow(renames)) {
    renamed_rows <- lv_rename_in_membership(renames, cfg, bk, dry_run = !commit)
    say(sprintf("membership  : %d renamed entr%s on %s%s\n", nrow(renamed_rows),
                if (nrow(renamed_rows) == 1) "y" else "ies", cfg$qc_tabs$datasets,
                if (commit) "" else " (would patch)"))
  }

  events <- qc_diff_to_events(base, state, source = "sheet")
  exported <- NULL
  if (commit) {
    qc_store_append(store, comp, events, run_id = run)
    say(sprintf("store       : appended %d event%s\n", nrow(events),
                if (nrow(events) == 1) "" else "s"))
    lv_version_append(store, comp, ver, run_id = run,
                      db_fingerprint = lv_scan(db)$fingerprint,
                      qc_state_hash = lv_dataset_set_hash(paste(state$tsid, state$field, state$value)))
    say(sprintf("version     : %s\n", ver$version))
    # Full rewrite only when rows are added, since a patch cannot add rows.
    # Columns never change the shape: the push writes the sheet's own column set.
    qc_sheet_push(push_state, bk, cfg$qc_sheet_id, cfg$qc_tabs$qc,
                  mode = if (length(new_rows) && !patch) "full" else "patch",
                  dry_run = FALSE)
    say("sheet       : pushed\n")
    # The export is the artefact anything downstream reads, and it records the
    # database fingerprint, vocabulary pin and QC state hash that produced it.
    # try(): a completed update should not be undone by an export failure, and the
    # export can be rebuilt from the state at any time.
    if (export) {
      exported <- try(lv_export(cfg, ver$version, datasets = ds, run_id = run,
                                store = store, dry_run = FALSE, progress = FALSE),
                      silent = TRUE)
      if (inherits(exported, "try-error")) {
        say(sprintf("export      : FAILED -- %s\n",
                    sub("\n.*", "", conditionMessage(attr(exported, "condition")))))
        say("              rerun: lv_export(cfg, ver$version, datasets = ds, dry_run = FALSE)\n")
      } else {
        say(sprintf("export      : %s\n", fs::path(lv_path("export"), comp, ver$version)))
      }
    }
    # The title is where most people read the version, so it is part of the update
    # rather than an optional extra. The try() remains only so a transient API
    # failure cannot fail an otherwise complete run, and it says so loudly.
    if (rename_sheet) {
      renamed <- try(lv_rename_qc_sheet(cfg, ver$version, dry_run = FALSE), silent = TRUE)
      if (inherits(renamed, "try-error")) {
        say(sprintf("title       : FAILED -- %s\n",
                    sub("\n.*", "", conditionMessage(attr(renamed, "condition")))))
      } else {
        say(sprintf("title       : %s\n", renamed))
      }
    }
  } else {
    say(sprintf("store       : would append %d event%s\n", nrow(events),
                if (nrow(events) == 1) "" else "s"))
    say("sheet       : would push\n")
    say(sprintf("version     : would record %s\n", ver$version))
    if (export) {
      ex <- lv_export(cfg, ver$version, datasets = ds, run_id = run, store = store,
                      dry_run = TRUE, progress = FALSE)
      say(sprintf("export      : would write %d dataset%s, %d timeseries, %d value%s\n",
                  ex$tables[["datasets"]], if (ex$tables[["datasets"]] == 1) "" else "s",
                  ex$tables[["timeseries"]], ex$tables[["values"]],
                  if (ex$tables[["values"]] == 1) "" else "s"))
    }
  }

  # ---- invariant: idempotence ----------------------------------------------

  idempotent <- NA
  if (commit) {
    say("\n-- second pass --\n")
    base2 <- qc_state_current(store, comp)
    frame2 <- qc_frame(db, datasets = ds, progress = FALSE)
    frame2 <- frame2[frame2$tsid %in% ts, , drop = FALSE]
    # Including csm, so a csm value that failed to reach its file shows up here as
    # a cell the run would write again rather than passing as idempotent.
    frame2 <- dplyr::bind_rows(frame2, lv_csm_frame(db, comp, datasets = ds, tsids = ts,
                                                    progress = FALSE))
    if (length(calcs)) {
      frame2 <- frame2[!frame2$field %in% calcs, , drop = FALSE]
      frame2 <- dplyr::bind_rows(frame2, lv_calculate(calcs, db, datasets = ds, tsids = ts,
                                                      index = lv_db_index(lv_scan(db), cache = TRUE),
                                                      progress = FALSE))
    }
    plan2 <- qc_merge(base2, qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc), frame2)
    n2 <- plan2$summary$n_changed
    idempotent <- n2 == 0
    say(sprintf("changes on a second run: %d\n", n2))
    if (!idempotent) {
      show(as.data.frame(utils::head(dplyr::count(tibble::as_tibble(plan2$changes),
                                                  .data$field, .data$resolution), 20)))
      cli::cli_abort("Not idempotent: a second run against unchanged inputs still changes cells.",
                     class = "lv_error_idempotence")
    }
    say("idempotent.\n")
  }

  structure(list(
    compilation = comp, run_id = run, commit = commit, stage = stage,
    considered = ds, timeseries = ts, members = members,
    plan = plan, state = state, events = events,
    write_cells = write_cells, csm_cells = csm_cells, calc_cells = calc_cells,
    calculations = calcs, computed = computed,
    renames = renames, renamed_rows = renamed_rows,
    membership = mres, staged = staged, changelog = changelog,
    receipt = receipt, version = ver, new_rows = new_rows,
    issues = lv_issues_bind(csm_issues, apply_issues),
    idempotent = idempotent
  ), class = "lv_update_result")
}

#' @export
print.lv_update_result <- function(x, ...) {
  cli::cli_h3("lv_update: {x$compilation} {x$version$version}")
  cli::cli_bullets(c(
    "*" = "{if (x$commit) 'committed' else 'dry run'}, run {x$run_id}",
    "*" = "{length(x$considered)} dataset{?s} considered, {length(x$members)} member timeseries",
    "*" = "{x$plan$summary$n_changed} change{?s} merged, {nrow(x$write_cells) + nrow(x$csm_cells)} cell{?s} to write",
    "*" = "{length(x$staged)} file{?s} staged",
    if (nrow(x$renames)) "!" = "{nrow(x$renames)} rename{?s}",
    if (!is.na(x$idempotent) && !x$idempotent) "x" = "not idempotent",
    if (lv_n_issues(x$issues, "warn")) "!" = "{lv_n_issues(x$issues, 'warn')} warning{?s}"
  ))
  invisible(x)
}
