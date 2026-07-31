test_that("a lock can be taken, inspected and released", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_false(lv_lock_status("demo")$locked)

  lv_lock("demo", run_id = "R1")
  s <- lv_lock_status("demo")
  expect_true(s$locked)
  expect_equal(s$run_id, "R1")
  expect_equal(s$pid, Sys.getpid())
  expect_false(s$stale)

  lv_unlock("demo")
  expect_false(lv_lock_status("demo")$locked)
})

test_that("a second acquisition fails while the holder is alive", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_lock("demo")
  withr::defer(lv_unlock("demo"))
  expect_error(lv_lock("demo"), class = "lv_error_lock")
})

# The defect: lipdverseR released its update flag only on the final line of the
# pipeline, so any earlier failure left the database flagged forever.
test_that("lv_locked releases on error, abort and normal exit", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())

  boom <- function() {
    lv_locked("demo")
    stop("stage failed")
  }
  expect_error(boom(), "stage failed")
  expect_false(lv_lock_status("demo")$locked)

  aborter <- function() {
    lv_locked("demo")
    rlang::abort("classed failure", class = "lv_error_write")
  }
  expect_error(aborter(), class = "lv_error_write")
  expect_false(lv_lock_status("demo")$locked)

  fine <- function() { lv_locked("demo"); "done" }
  expect_equal(fine(), "done")
  expect_false(lv_lock_status("demo")$locked)
})

test_that("a lock held by a dead process is stale and gets broken", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_lock("demo", run_id = "OLD")

  # Rewrite the lock as if a long-gone pid on this host held it.
  f <- fs::path(lv_path("state"), "locks", "demo.lock", "lock.json")
  info <- jsonlite::read_json(f, simplifyVector = TRUE)
  info$pid <- 99999999L
  jsonlite::write_json(info, f, auto_unbox = TRUE)

  expect_true(lv_lock_status("demo")$stale)
  expect_silent(suppressMessages(lv_lock("demo", run_id = "NEW")))
  expect_equal(lv_lock_status("demo")$run_id, "NEW")
  lv_unlock("demo")
})

test_that("a lock past its timeout is stale even if the pid is alive", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_lock("demo", timeout_minutes = 60)

  # Backdate rather than sleep: a wall-clock race makes this test depend on
  # machine load, and it should be testing the timeout rule, not the clock.
  f <- fs::path(lv_path("state"), "locks", "demo.lock", "lock.json")
  info <- jsonlite::read_json(f, simplifyVector = TRUE)
  info$started_at <- format(Sys.time() - 3600 * 3, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  jsonlite::write_json(info, f, auto_unbox = TRUE)

  s <- lv_lock_status("demo")
  expect_true(s$stale)
  expect_gt(s$age_minutes, 60)
  lv_unlock("demo")
})

test_that("locks are per compilation", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_lock("a"); withr::defer(lv_unlock("a"))
  expect_no_error(lv_lock("b"))
  lv_unlock("b")
})

test_that("compilation names with slashes cannot escape the lock directory", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_lock("../evil")
  withr::defer(lv_unlock("../evil"))

  root <- fs::path(lv_path("state"), "locks")
  locks <- fs::dir_ls(root, all = TRUE, type = "directory")
  expect_length(locks, 1)
  # Contained, and not a dotfile: an operator hunting a stuck lock must be
  # able to see it in a plain listing.
  expect_equal(basename(locks), "__evil.lock")
  expect_true(fs::path_has_parent(locks, root))
})

test_that("sanitised names stay distinct", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_lock("../evil"); withr::defer(lv_unlock("../evil"))
  # "evil" must not be considered already locked by "../evil".
  expect_no_error(lv_lock("evil"))
  lv_unlock("evil")
})
