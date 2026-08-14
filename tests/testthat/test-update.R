# The whole pipeline, end to end, against a local sheet and a temp database.
#
# Until lv_update() existed this was a script, so the most consequential code in
# the package -- merge, apply, promote, version, changelog, and both invariants
# -- was the only part no test ever ran.

local_update_env <- function(envir = parent.frame(), qc = NULL, members = NULL,
                             tsids = c("T1", "T2")) {
  state <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(LIPDVERSE_STATE = state, .local_envir = envir)
  db <- withr::local_tempdir(.local_envir = envir)
  write_lpd(db, "A.Author.2001", tsids = tsids)
  # Two variables, because validLipd() rejects a measurement table with fewer --
  # a one-column fixture is invalid as delivered and fails the promote's
  # verification for a reason that has nothing to do with the run.
  write_lpd(db, "B.Author.2002", tsids = c("T3", "T4"))

  sheets <- withr::local_tempdir(.local_envir = envir)
  bk <- sheet_backend_local(sheets)
  # The config validator insists on a Google-shaped id, and rightly: a short one
  # in the registry means a typo, not a local sheet. The local backend only uses
  # it as a directory name, so a conforming fake serves.
  sid <- "LocalTestSheet0000000000000000000000"
  qc <- qc %||% data.frame(TSid = c(tsids, "T3", "T4"),
                           dataSetName = c(rep("A.Author.2001", length(tsids)),
                                           "B.Author.2002", "B.Author.2002"),
                           archiveType = "LakeSediment",
                           units = "degC",
                           inThisCompilation = "FALSE",
                           stringsAsFactors = FALSE)
  members <- members %||% data.frame(dsn = c("A.Author.2001", "B.Author.2002"),
                                     inComp = "TRUE", stringsAsFactors = FALSE)
  sheet_write(bk, sid, "QC", qc)
  sheet_write(bk, sid, "datasetsInCompilation", members)

  cfg <- lv_config("lipdverseTest", qc_sheet_id = sid, lipd_dir = db)
  list(cfg = cfg, db = db, backend = bk, sid = sid,
       store = qc_store(file.path(state, "store")),
       stage = file.path(state, "stage"))
}

# A first run with the sheet and the files agreeing, which records the baseline.
# Without one the merge cannot tell a curator edit from a file change -- both
# sides differ from an absent base -- so every edit is a conflict and every
# dataSetName difference is an identifier error. That is the system being right;
# seeding is what a real compilation does once, via scripts/seed-baseline.R.
seed_baseline <- function(e) run_update(e, commit = TRUE)

run_update <- function(e, ...) {
  lv_update("lipdverseTest", cfg = e$cfg, dir = e$db, store = e$store,
            backend = e$backend, stage = e$stage, export = FALSE,
            snapshot = FALSE, rename_sheet = FALSE, progress = FALSE, ...)
}

test_that("a dry run reports a plan and writes nothing", {
  e <- local_update_env()
  before <- sort(fs::dir_ls(e$db, glob = "*.lpd", type = "file"))
  hashes <- vapply(before, function(p) digest::digest(file = p), character(1))

  r <- run_update(e)

  expect_s3_class(r, "lv_update_result")
  expect_false(r$commit)
  expect_setequal(r$considered, c("A.Author.2001", "B.Author.2002"))
  # The database is untouched, which is what makes a dry run safe to leave running.
  expect_equal(vapply(before, function(p) digest::digest(file = p), character(1)), hashes)
  # And the store has recorded nothing: a dry run leaves no trace anywhere.
  expect_equal(nrow(qc_state_current(e$store, "lipdverseTest")), 0)
})

test_that("a curator edit on the sheet becomes a cell to write", {
  e <- local_update_env()
  seed_baseline(e)
  # Against a recorded baseline the sheet moving alone is unambiguously a
  # curator edit, and the file keeping its old value is not a competing claim.
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  qc$units[qc$TSid == "T1"] <- "permil"
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)

  r <- run_update(e)
  hit <- r$write_cells[r$write_cells$tsid == "T1" & r$write_cells$field == "paleoData_units", ]
  expect_equal(nrow(hit), 1)
  expect_equal(hit$value, "permil")
})

test_that("membership is admitted from the sheet, not invented", {
  e <- local_update_env()
  seed_baseline(e)
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  qc$inThisCompilation[qc$TSid == "T1"] <- "TRUE"
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)

  r <- run_update(e)
  expect_true("T1" %in% r$membership$added)
  expect_length(r$membership$removed, 0)
  # A blank is "no opinion", so nothing else is admitted.
  expect_length(r$membership$added, 1)
})

test_that("a rename is declared, and the old file is named for deletion", {
  e <- local_update_env()
  seed_baseline(e)
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  qc$dataSetName[qc$dataSetName == "B.Author.2002"] <- "Better.Author.2002"
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)

  r <- run_update(e)
  expect_equal(nrow(r$renames), 1)
  expect_equal(r$renames$new_name, "Better.Author.2002")
  expect_true(is.na(r$renames$issue))
  # The staged file carries the new name and the promote plan retires the old
  # one, rather than leaving both.
  expect_true("Better.Author.2002.lpd" %in% r$staged)
  expect_true("B.Author.2002.lpd" %in% r$receipt$deletions)
})

test_that("a commit writes the files, the store and the version", {
  e <- local_update_env()
  seed_baseline(e)
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  qc$units[qc$TSid == "T1"] <- "permil"
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)

  r <- run_update(e, commit = TRUE)

  expect_true(r$commit)
  expect_true(r$receipt$committed)
  # The value reached the file.
  L <- suppressWarnings(lipdR::readLipd(fs::path(e$db, "A.Author.2001.lpd")))
  tb <- L$paleoData[[1]]$measurementTable[[1]]
  cols <- if (!is.null(tb$columns)) tb$columns else tb
  hit <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), cols)
  expect_equal(as.character(hit[[1]]$units), "permil")
  # And the store recorded it, so the next run has a baseline.
  st <- qc_state_current(e$store, "lipdverseTest")
  expect_equal(st$value[st$tsid == "T1" & st$field == "paleoData_units"], "permil")
  expect_true(!is.null(lv_version_current(e$store, "lipdverseTest")))
  # The idempotence invariant ran and passed, which is the assertion that a
  # second run against unchanged inputs would change nothing.
  expect_true(r$idempotent)
})

test_that("a declared rename is not mistaken for collateral damage", {
  # The collateral invariant asks whether the run touched a file it cannot
  # account for. A renamed dataset lands under a name the membership tab does
  # not know yet, so it has to be exempt -- but only because it was declared.
  # The undeclared case is what stopped the real hydroclimate2k run before
  # renaming was supported, and lv_planned_renames() is where that line is drawn.
  e <- local_update_env()
  seed_baseline(e)
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  qc$dataSetName[qc$dataSetName == "B.Author.2002"] <- "Better.Author.2002"
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)

  r <- run_update(e)
  expect_equal(r$renames$new_name, "Better.Author.2002")
  expect_true("Better.Author.2002.lpd" %in% r$staged)
  # The renamed file is outside the compilation's dataset list and the run still
  # completes, because it is accounted for.
  expect_false("Better.Author.2002" %in% r$considered)
})

test_that("an unsafe rename stops the run before anything is applied", {
  e <- local_update_env()
  seed_baseline(e)
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  # Rename B onto A's name: that would overwrite a different dataset.
  qc$dataSetName[qc$dataSetName == "B.Author.2002"] <- "A.Author.2001"
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)
  expect_error(run_update(e), class = "lv_error_rename")
})

test_that("the idempotence check asks the same question the run answered", {
  # The check pulled the raw sheet while the run merges a scoped one, so another
  # compilation's csm column read as changes the run had failed to make. It had
  # not failed to make them; it declined to, on purpose. On the first live
  # hydroclimate2k update this failed the assertion with 600 iso2kUI cells after
  # every write had already succeeded.
  e <- local_update_env()
  seed_baseline(e)
  qc <- sheet_read(e$backend, e$cfg$qc_sheet_id, "QC")
  # iso2k's certification column, on lipdverseTest's tab.
  qc[["Iso2k QC certification (INITIALS)"]] <- c("AAA", rep(NA, nrow(qc) - 1))
  sheet_write(e$backend, e$cfg$qc_sheet_id, "QC", qc)

  r <- run_update(e, commit = TRUE)
  # The foreign column is excluded from the merge and does not make the run
  # look non-idempotent afterwards.
  expect_true(r$idempotent)
  expect_false(any(r$write_cells$field == "paleoData_iso2kCertification"))
})
