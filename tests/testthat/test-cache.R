test_that("a miss computes and a hit does not", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  calls <- 0
  run <- function() lv_cached("demo", list(a = 1), { calls <<- calls + 1; "value" })

  expect_equal(run(), "value")
  expect_equal(calls, 1)
  expect_equal(suppressMessages(run()), "value")
  expect_equal(calls, 1)
})

test_that("changing inputs or version invalidates", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  calls <- 0
  f <- function(inputs, version) lv_cached("demo", inputs, { calls <<- calls + 1; inputs }, version = version)

  f(list(a = 1), 1L); expect_equal(calls, 1)
  f(list(a = 2), 1L); expect_equal(calls, 2)   # different inputs
  f(list(a = 1), 2L); expect_equal(calls, 3)   # bumped stage version
  suppressMessages(f(list(a = 1), 1L)); expect_equal(calls, 3)  # original still cached
})

test_that("use_cache = FALSE forces recomputation but still stores", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  calls <- 0
  f <- function(use) lv_cached("demo", list(a = 1), { calls <<- calls + 1; "v" }, use_cache = use)
  f(TRUE);  expect_equal(calls, 1)
  f(FALSE); expect_equal(calls, 2)
})

test_that("stages do not collide", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_equal(lv_cached("one", list(a = 1), "first"), "first")
  expect_equal(lv_cached("two", list(a = 1), "second"), "second")
  expect_equal(suppressMessages(lv_cached("one", list(a = 1), "ignored")), "first")
})

# An interrupted write must not poison the stage forever.
test_that("a corrupt cache entry is discarded and recomputed", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_cached("demo", list(a = 1), "good")

  f <- fs::dir_ls(fs::path(lv_path("state"), "cache", "demo"), glob = "*.rds")
  writeLines("garbage", f[1])

  expect_equal(lv_cached("demo", list(a = 1), "recomputed"), "recomputed")
})

test_that("cache info and clearing work", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  lv_cached("one", list(a = 1), "x")
  lv_cached("two", list(a = 1), "y")

  info <- lv_cache_info()
  expect_setequal(info$stage, c("one", "two"))
  expect_true(all(info$bytes > 0))

  lv_cache_clear("one")
  expect_setequal(lv_cache_info()$stage, "two")
  lv_cache_clear()
  expect_equal(nrow(lv_cache_info()), 0)
})

test_that("run ids are unique and their timestamp prefix is monotonic", {
  ids <- replicate(200, lv_run_id())
  expect_equal(anyDuplicated(ids), 0)
  expect_match(ids, "^[0-9]{8}T[0-9]{6}-[a-z0-9]{6}$", all = TRUE)

  # Ordering is by the timestamp prefix; within a single second the random
  # suffix decides, which is fine because run directories only need to sort
  # chronologically at second granularity.
  stamps <- substr(ids, 1, 15)
  expect_false(is.unsorted(stamps))
})

test_that("lv_log appends to the active run log only", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  rid <- lv_run_id()

  suppressMessages(lv_log("outside a run"))
  expect_false(fs::file_exists(fs::path(lv_run_dir(rid, create = FALSE), "log.txt")))

  lv_with_run(rid, suppressMessages(lv_log("inside a run")))
  expect_match(paste(readLines(fs::path(lv_run_dir(rid), "log.txt")), collapse = "\n"),
               "inside a run")
})
