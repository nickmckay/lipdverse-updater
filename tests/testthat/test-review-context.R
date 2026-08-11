test_that("examples show non-missing values and how full the column is", {
  # Taking the first few values would show NA, NA, NA for a sparse column and
  # read as empty. Two real columns in the h2k round-2 batch look exactly like
  # that: "d13C code" is 114 populated values out of 580, and they are isotope
  # measurements, not the empty flag column the leading NAs suggested.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  # Same length as its sibling: a ragged table makes writeLipd empty every
  # column in it, which is its own trap.
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$values <- list(NA, -5.34, -6.62)
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$variableName <- "d13C code"
  fs::file_delete(fs::path(d, "A.Author.2001.lpd"))
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))

  x <- tibble::tibble(field = "paleoData_variableName", value = "d13C code",
                      dataSetName = "A.Author.2001", TSid = "T1")
  ctx <- lv_review_context(x, d)
  ex <- unlist(ctx$examples)
  expect_true(any(grepl("-5.34", ex)))
  expect_false(any(grepl("NA, NA", ex)))
  expect_true(any(grepl("2 of 3 values", ex)))
})

test_that("an empty column says so rather than looking like a lookup failure", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$values <- list(NA, NA, NA)
  fs::file_delete(fs::path(d, "A.Author.2001.lpd"))
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))

  x <- tibble::tibble(field = "paleoData_variableName", value = "temperature",
                      dataSetName = "A.Author.2001", TSid = "T1")
  ex <- unlist(lv_review_context(x, d)$examples)
  expect_true(any(grepl("empty", ex)))
  # Not a fixed total: lipdR does not preserve the length of an all-NA column
  # across a write, so the count is asserted as "none filled", not "none of 3".
  expect_true(any(grepl("0 of [0-9]+ values", ex)))
})

test_that("siblings list the other variables in the table", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  x <- tibble::tibble(field = "paleoData_variableName", value = "temperature",
                      dataSetName = "A.Author.2001", TSid = "T1")
  sib <- unlist(lv_review_context(x, d)$siblings)
  expect_true(length(sib) >= 1)
  expect_true(all(nzchar(sib)))
})

test_that("no directory means no context rather than an error", {
  # The review must still generate when the staged files are gone.
  x <- tibble::tibble(field = "paleoData_variableName", value = "x",
                      dataSetName = "A", TSid = "T1")
  expect_length(lv_review_context(x, NULL)$key, 0)
  expect_length(lv_review_context(x, "/nonexistent/path")$key, 0)
})

test_that("fields are ordered for working, not alphabetically", {
  # variableName decisions constrain units and proxy, and a decompose on
  # variableName creates the seasonality an interpretation row refers to.
  expect_lt(lv_field_rank("paleoData_variableName"), lv_field_rank("paleoData_units"))
  expect_lt(lv_field_rank("paleoData_units"), lv_field_rank("paleoData_proxy"))
  expect_lt(lv_field_rank("paleoData_proxy"), lv_field_rank("interpretation_variable"))
  # An unknown field sorts last rather than leading silently.
  expect_gt(lv_field_rank("something_new"), lv_field_rank("archiveType"))
})
