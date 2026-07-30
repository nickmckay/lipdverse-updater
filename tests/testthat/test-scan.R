test_that("lv_scan indexes files and computes a fingerprint", {
  d <- make_db(list(list(dataSetName = "A.Author.2001"),
                    list(dataSetName = "B.Author.2002")))
  s <- lv_scan(d, cache = FALSE)

  expect_s3_class(s, "lv_scan")
  expect_equal(nrow(s$files), 2)
  expect_equal(s$files$dataSetName, c("A.Author.2001", "B.Author.2002"))
  expect_match(s$fingerprint, "^[0-9a-f]{32}$")
})

test_that("lv_scan refuses an empty directory", {
  d <- withr::local_tempdir()
  expect_error(lv_scan(d, cache = FALSE), class = "lv_error_scan")
})

# This is the defect in lipdverseR's directoryMD5(): it zipped the directory and
# hashed the zip, and zip stores mtimes, so touching an unchanged file forced a
# full rebuild.
test_that("fingerprint ignores mtime but tracks content", {
  d <- make_db(list(list(dataSetName = "A.Author.2001")))
  before <- lv_scan(d, cache = FALSE)$fingerprint

  Sys.setFileTime(file.path(d, "A.Author.2001.lpd"), Sys.time() + 60)
  expect_equal(lv_scan(d, cache = FALSE)$fingerprint, before)

  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2", "T3"))
  expect_false(identical(lv_scan(d, cache = FALSE)$fingerprint, before))
})

test_that("lv_scan_diff reports added, removed and changed", {
  d <- make_db(list(list(dataSetName = "A.Author.2001"),
                    list(dataSetName = "B.Author.2002")))
  old <- lv_scan(d, cache = FALSE)

  file.remove(file.path(d, "B.Author.2002.lpd"))
  write_lpd(d, "C.Author.2003")
  write_lpd(d, "A.Author.2001", tsids = c("T9"))
  new <- lv_scan(d, cache = FALSE)

  diff <- lv_scan_diff(old, new)
  expect_setequal(diff$status[diff$dataSetName == "C.Author.2003"], "added")
  expect_setequal(diff$status[diff$dataSetName == "B.Author.2002"], "removed")
  expect_setequal(diff$status[diff$dataSetName == "A.Author.2001"], "changed")
})

test_that("the hash cache does not change results", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- make_db(list(list(dataSetName = "A.Author.2001")))
  expect_equal(lv_scan(d, cache = TRUE)$fingerprint,
               lv_scan(d, cache = TRUE)$fingerprint)
})
