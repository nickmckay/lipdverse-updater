test_that("lv_issues always has the standard columns", {
  x <- lv_issues(check = "c", severity = "warn", message = "m")
  expect_s3_class(x, "lv_issues")
  expect_true(all(c("check", "severity", "message", "datasetId", "dataSetName",
                    "TSid", "field", "value", "path") %in% names(x)))
})

test_that("an invalid severity is rejected", {
  expect_error(lv_issues(check = "c", severity = "catastrophe"), "Invalid severity")
})

test_that("binding tolerates NULL and empty inputs", {
  x <- lv_issues_bind(NULL, lv_issues_empty(), lv_issues(check = "c", severity = "info"))
  expect_equal(nrow(x), 1)
  expect_equal(nrow(lv_issues_bind()), 0)
})

test_that("lv_n_issues counts by severity", {
  x <- lv_issues(check = c("a", "b", "c"), severity = c("error", "warn", "error"))
  expect_equal(lv_n_issues(x, "error"), 2)
  expect_equal(lv_n_issues(x, c("warn", "info")), 1)
})

test_that("lv_issues_check aborts on errors and writes the report first", {
  p <- withr::local_tempfile(fileext = ".csv")
  x <- lv_issues(check = "a", severity = "error", message = "bad")

  expect_error(lv_issues_check(x, path = p), class = "lv_error_issues")
  # The operator must have the full report even though the run stopped.
  expect_true(file.exists(p))
  expect_equal(nrow(readr::read_csv(p, show_col_types = FALSE)), 1)
})

test_that("lv_issues_check passes when only warnings are present", {
  x <- lv_issues(check = "a", severity = "warn")
  expect_silent(lv_issues_check(x))
})
