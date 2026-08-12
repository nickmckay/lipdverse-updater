fingerprint <- function(dir) {
  f <- sort(fs::dir_ls(dir, glob = "*.lpd", type = "file"))
  paste(fs::path_file(f), vapply(f, function(p) digest::digest(file = p), character(1)),
        collapse = "|")
}

test_that("a dry run verifies and writes nothing", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9"))))
  before <- fingerprint(live)

  r <- lv_promote(stage, live, dry_run = TRUE)
  expect_false(r$committed)
  expect_equal(fingerprint(live), before)
  expect_equal(nrow(r$plan), 1)
  expect_equal(r$plan$action, "replace")
})

test_that("a commit replaces and adds, keeping the old copies", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9")),
                         list(dataSetName = "B.Author.2002")))
  old <- digest::digest(file = fs::path(live, "A.Author.2001.lpd"))

  r <- lv_promote(stage, live, run_id = "R1", dry_run = FALSE)
  expect_true(r$committed)
  expect_setequal(fs::path_file(fs::dir_ls(live, glob = "*.lpd")),
                  c("A.Author.2001.lpd", "B.Author.2002.lpd"))
  # The replaced copy is in the trash, not gone.
  expect_equal(digest::digest(file = fs::path(live, ".trash", "R1", "A.Author.2001.lpd")), old)
  expect_true(fs::file_exists(fs::path(live, ".runs", "R1.json")))
})

# The defect this replaces: lipdverseR deleted first and wrote second.
test_that("deletion requires explicit permission", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001")))

  expect_error(lv_promote(stage, live, dry_run = FALSE), class = "lv_error_write")
  # Nothing was removed by the refusal.
  expect_true(fs::file_exists(fs::path(live, "B.Author.2002.lpd")))
})

test_that("a permitted deletion moves the file to trash rather than removing it", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001")))

  lv_promote(stage, live, run_id = "R1", dry_run = FALSE, allow_delete = TRUE)
  expect_false(fs::file_exists(fs::path(live, "B.Author.2002.lpd")))
  expect_true(fs::file_exists(fs::path(live, ".trash", "R1", "B.Author.2002.lpd")))
})

# Verification is the gate. A file that cannot be read back must never replace
# a good one.
test_that("an unreadable staged file aborts before anything is touched", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001")))
  writeLines("not a zip", fs::path(stage, "A.Author.2001.lpd"))
  before <- fingerprint(live)

  expect_error(lv_promote(stage, live, dry_run = FALSE), class = "lv_error_issues")
  expect_equal(fingerprint(live), before)
})

test_that("a staged file with the wrong dataSetName is rejected", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- withr::local_tempdir()
  # Content says B, filename says A.
  write_lpd(stage, "B.Author.2002")
  fs::file_move(fs::path(stage, "B.Author.2002.lpd"), fs::path(stage, "A.Author.2001.lpd"))

  expect_error(lv_promote(stage, live, dry_run = FALSE), class = "lv_error_issues")
  expect_equal(nrow(lv_verify_file(fs::path(stage, "A.Author.2001.lpd"), "A.Author.2001")), 1)
})

test_that("a truncated staged file is rejected", {
  stage <- withr::local_tempdir()
  write_lpd(stage, "A.Author.2001")
  p <- fs::path(stage, "A.Author.2001.lpd")
  writeBin(raw(10), p)
  iss <- lv_verify_file(p)
  expect_equal(iss$check, "staged_too_small")
})

test_that("a good file passes verification", {
  stage <- withr::local_tempdir()
  write_lpd(stage, "A.Author.2001")
  expect_equal(nrow(lv_verify_file(fs::path(stage, "A.Author.2001.lpd"), "A.Author.2001")), 0)
})

# Fault injection: the database must be byte-identical after a failure at any
# point during the commit.
test_that("rollback restores the database exactly", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9")),
                         list(dataSetName = "B.Author.2002", tsids = c("T2", "T8")),
                         list(dataSetName = "C.Author.2003")))
  before <- fingerprint(live)

  lv_promote(stage, live, run_id = "R1", dry_run = FALSE)
  expect_false(identical(fingerprint(live), before))

  lv_write_rollback(live, "R1")
  expect_equal(fingerprint(live), before)
  # The file the run added is gone again.
  expect_false(fs::file_exists(fs::path(live, "C.Author.2003.lpd")))
})

test_that("rollback also restores permitted deletions", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001")))
  before <- fingerprint(live)

  lv_promote(stage, live, run_id = "R1", dry_run = FALSE, allow_delete = TRUE)
  expect_false(fs::file_exists(fs::path(live, "B.Author.2002.lpd")))

  lv_write_rollback(live, "R1")
  expect_equal(fingerprint(live), before)
})

test_that("the write log records committed runs", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9"))))

  expect_equal(nrow(lv_write_log(live)), 0)
  lv_promote(stage, live, run_id = "R1", dry_run = FALSE)
  log <- lv_write_log(live)
  expect_equal(nrow(log), 1)
  expect_equal(log$run_id, "R1")
  expect_equal(log$n_replaced, 1)
})

test_that("gc prunes old trash but keeps the most recent", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  for (i in 1:4) {
    stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", paste0("TX", i)))))
    lv_promote(stage, live, run_id = sprintf("R%02d", i), dry_run = FALSE)
  }
  expect_equal(length(fs::dir_ls(fs::path(live, ".trash"), type = "directory")), 4)
  lv_gc(live, keep = 2)
  gens <- fs::path_file(fs::dir_ls(fs::path(live, ".trash"), type = "directory"))
  expect_equal(length(gens), 2)
  expect_setequal(gens, c("R03", "R04"))   # the oldest go first
})

test_that("an empty staging directory is refused", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  expect_error(lv_promote(withr::local_tempdir(), live, dry_run = FALSE),
               class = "lv_error_write")
})

# The pipeline normally updates only the datasets that changed. Without a
# partial mode, promoting one changed file into a database of thousands reads
# as deleting all the others.
test_that("a partial staging does not treat untouched files as deletions", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002"),
                        list(dataSetName = "C.Author.2003")))
  stage <- local_db(list(list(dataSetName = "B.Author.2002", tsids = c("T1", "T9"))))

  # Whole-directory semantics would call the other two deletions and refuse.
  expect_error(lv_promote(stage, live, dry_run = FALSE), class = "lv_error_write")

  r <- lv_promote(stage, live, run_id = "P1", dry_run = FALSE, partial = TRUE)
  expect_equal(length(r$deletions), 0)
  expect_setequal(fs::path_file(fs::dir_ls(live, glob = "*.lpd")),
                  c("A.Author.2001.lpd", "B.Author.2002.lpd", "C.Author.2003.lpd"))
  expect_true(fs::file_exists(fs::path(live, ".trash", "P1", "B.Author.2002.lpd")))
})

test_that("a partial promotion still rolls back cleanly", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
  stage <- local_db(list(list(dataSetName = "B.Author.2002", tsids = c("T1", "T9"))))
  before <- fingerprint(live)

  lv_promote(stage, live, run_id = "P1", dry_run = FALSE, partial = TRUE)
  expect_false(identical(fingerprint(live), before))
  lv_write_rollback(live, "P1")
  expect_equal(fingerprint(live), before)
})

test_that("a decomposed filename on disk is replaced, not added alongside", {
  # macOS hands back filenames in NFD while a freshly written file takes its name
  # from the metadata, which is NFC. Compared as strings they differ, so a
  # replace read as an add: the live copy was never trashed and the run could not
  # be rolled back. The filesystem then resolved both spellings to one entry, so
  # the write landed correctly and nothing looked wrong.
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001")))
  nfd <- stringi::stri_trans_nfd("Büntgen.Test.2011.lpd")
  nfc <- stringi::stri_trans_nfc("Büntgen.Test.2011.lpd")
  expect_false(identical(nfd, nfc))
  fs::file_copy(fs::dir_ls(live, glob = "*.lpd")[1], fs::path(live, nfd))
  fs::file_copy(fs::dir_ls(stage, glob = "*.lpd")[1], fs::path(stage, nfc))
  fs::file_delete(fs::path(stage, "A.Author.2001.lpd"))

  r <- lv_promote(stage, live, run_id = "N1", dry_run = TRUE, verify = FALSE, partial = TRUE)
  expect_equal(r$plan$action, "replace")
  # And it targets the spelling already on disk.
  expect_equal(fs::path_file(r$plan$live_path), nfd)
})

test_that("the displaced copy of a decomposed name reaches the trash", {
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001")))
  nfd <- stringi::stri_trans_nfd("Büntgen.Test.2011.lpd")
  nfc <- stringi::stri_trans_nfc("Büntgen.Test.2011.lpd")
  fs::file_copy(fs::dir_ls(live, glob = "*.lpd")[1], fs::path(live, nfd))
  fs::file_copy(fs::dir_ls(stage, glob = "*.lpd")[1], fs::path(stage, nfc))
  fs::file_delete(fs::path(stage, "A.Author.2001.lpd"))

  lv_promote(stage, live, run_id = "N2", dry_run = FALSE, verify = FALSE, partial = TRUE)
  trashed <- fs::path_file(fs::dir_ls(fs::path(live, ".trash", "N2")))
  expect_length(trashed, 1)
  expect_equal(lv_nfc(trashed), lv_nfc(nfd))
})

test_that("verification accepts a decomposed filename with a composed dataSetName", {
  # The sibling NFC tests above both pass verify = FALSE, which is how this
  # survived: the staged check compares the dataSetName inside the file (NFC)
  # against a name derived from the filename (NFD on macOS) and read them as a
  # mismatch, failing the whole promote on any accented dataset.
  name <- stringi::stri_trans_nfc("Büntgen.Test.2011")
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = name)))
  written <- fs::dir_ls(stage, glob = "*.lpd")[1]
  nfd <- fs::path(stage, stringi::stri_trans_nfd(paste0(name, ".lpd")))
  if (!identical(fs::path_file(written), fs::path_file(nfd))) fs::file_move(written, nfd)

  r <- lv_promote(stage, live, run_id = "N3", dry_run = TRUE, verify = TRUE, partial = TRUE)
  expect_equal(lv_n_issues(r$issues, "error"), 0)
})

test_that("a run id cannot write twice", {
  # Reusing an id puts the second set of files into the first run's trash and
  # overwrites its receipt. That is how an ingest of 91 datasets came to be
  # recorded as an update of 412, leaving one surviving record, no pre-ingest
  # copy, and a rollback that would have undone both promotes at once.
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  one  <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9"))))
  two  <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T8"))))

  lv_promote(one, live, run_id = "R1", dry_run = FALSE, verify = FALSE, partial = TRUE)
  expect_error(
    lv_promote(two, live, run_id = "R1", dry_run = FALSE, verify = FALSE, partial = TRUE),
    class = "lv_error_write")

  # The first run's record and its trash survive intact.
  expect_true(fs::file_exists(fs::path(live, ".runs", "R1.json")))
  expect_length(fs::dir_ls(fs::path(live, ".trash", "R1")), 1)
})

test_that("a dry run does not consume the run id", {
  # Previewing must stay free: a dry run writes no receipt, so the same id can
  # still be used for the real promote that follows it.
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9"))))

  lv_promote(stage, live, run_id = "R2", dry_run = TRUE, verify = FALSE, partial = TRUE)
  expect_no_error(
    lv_promote(stage, live, run_id = "R2", dry_run = FALSE, verify = FALSE, partial = TRUE))
})
