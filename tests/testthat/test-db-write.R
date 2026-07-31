local_db <- function(specs, envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  for (s in specs) do.call(write_lpd, c(list(dir = d), s))
  d
}

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
