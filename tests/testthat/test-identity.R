local_index <- function(specs) {
  withr::local_envvar(.local_envir = parent.frame(),
                      LIPDVERSE_STATE = withr::local_tempdir(.local_envir = parent.frame()))
  d <- make_db(specs)
  lv_db_index(lv_scan(d, cache = FALSE), cache = FALSE)
}

test_that("lv_db_index extracts identity from real .lpd structure", {
  i <- local_index(list(
    list(dataSetName = "A.Author.2001", datasetId = "ID_A", tsids = c("T1", "T2")),
    list(dataSetName = "B.Author.2002", datasetId = "ID_B", tsids = "T3", version = "2.1.0")
  ))

  expect_s3_class(i, "lv_index")
  expect_equal(nrow(i$datasets), 2)
  expect_equal(i$datasets$datasetId, c("ID_A", "ID_B"))
  expect_equal(i$datasets$n_ts, c(2, 1))
  expect_setequal(i$timeseries$TSid, c("T1", "T2", "T3"))
  expect_true(all(is.na(i$datasets$parse_error)))
})

test_that("datasetVersion is the maximum changelog version, not the last", {
  i <- local_index(list(list(dataSetName = "A.Author.2001", version = "1.0.10")))
  # 1.0.10 must beat 1.0.0 numerically; string comparison would pick 1.0.0.
  expect_equal(i$datasets$datasetVersion, "1.0.10")
})

test_that("the legacy named-column layout is still parsed", {
  i <- local_index(list(list(dataSetName = "A.Author.2001",
                             tsids = c("T1", "T2"), legacy_columns = TRUE)))
  expect_setequal(i$timeseries$TSid, c("T1", "T2"))
})

test_that("an unreadable file is reported, not fatal", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- make_db(list(list(dataSetName = "A.Author.2001")))
  writeLines("not a zip", file.path(d, "Broken.Author.2003.lpd"))

  i <- lv_db_index(lv_scan(d, cache = FALSE), cache = FALSE)
  expect_equal(sum(!is.na(i$datasets$parse_error)), 1)

  iss <- lv_validate_identity(i)
  expect_true("file_unreadable" %in% iss$check)
  expect_equal(iss$severity[iss$check == "file_unreadable"], "error")
})

test_that("a clean database produces no issues", {
  i <- local_index(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", tsids = "T1"),
                        list(dataSetName = "B.Author.2002", datasetId = "ID_B", tsids = "T2")))
  expect_equal(nrow(lv_validate_identity(i)), 0)
})

# The negative control. Without these, "no issues found" on the real database
# is indistinguishable from a validator that never fires.
test_that("duplicate TSids are caught and never auto-renamed", {
  i <- local_index(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", tsids = "SHARED"),
                        list(dataSetName = "B.Author.2002", datasetId = "ID_B", tsids = "SHARED")))
  iss <- lv_validate_identity(i)

  expect_true("duplicate_TSid_across_datasets" %in% iss$check)
  expect_equal(iss$severity[iss$check == "duplicate_TSid_across_datasets"], "error")
  # lipdverseR appended "-dup" here; nothing may be silently rewritten.
  expect_false(any(grepl("-dup", i$timeseries$TSid)))
})

test_that("duplicate TSids within one dataset are distinguished from across", {
  i <- local_index(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T1"))))
  iss <- lv_validate_identity(i)
  expect_true("duplicate_TSid_within_dataset" %in% iss$check)
})

test_that("duplicate datasetIds are caught", {
  i <- local_index(list(list(dataSetName = "A.Author.2001", datasetId = "SAME"),
                        list(dataSetName = "B.Author.2002", datasetId = "SAME")))
  iss <- lv_validate_identity(i)
  expect_true("duplicate_datasetId" %in% iss$check)
})

test_that("strict = FALSE downgrades duplicates to warnings", {
  i <- local_index(list(list(dataSetName = "A.Author.2001", datasetId = "SAME"),
                        list(dataSetName = "B.Author.2002", datasetId = "SAME")))
  expect_equal(lv_validate_identity(i, strict = FALSE)$severity[
    lv_validate_identity(i, strict = FALSE)$check == "duplicate_datasetId"], "warn")
})

test_that("filename and metadata disagreement is reported", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- make_db(list(list(dataSetName = "A.Author.2001")))
  file.rename(file.path(d, "A.Author.2001.lpd"), file.path(d, "Renamed.lpd"))

  iss <- lv_validate_identity(lv_db_index(lv_scan(d, cache = FALSE), cache = FALSE))
  expect_true("filename_metadata_mismatch" %in% iss$check)
})
