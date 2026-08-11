test_that("a column's createdBy is the compilation, not the file's writer", {
  # createdBy exists twice in the registry: at the dataset root it is the tool
  # that wrote the file, on a column it is the compilation that added the
  # variable. Resolving a column key to the root field put "hydroclimate2k"
  # into the dataset's field, where it fought the tool name and never reached
  # the paleoData_createdBy column the curators read.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"),
            col_extra = list(createdBy = "hydroclimate2k"))

  f <- qc_frame(d, progress = FALSE)
  cb <- f[grepl("createdBy", f$field), ]
  expect_true("paleoData_createdBy" %in% cb$field)
  expect_equal(unique(cb$value[cb$field == "paleoData_createdBy"]), "hydroclimate2k")
})

test_that("the root createdBy still reaches the dataset-level field", {
  # The two must not collapse into one: losing the tool that wrote a file is
  # its own loss.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  p <- fs::path(d, "A.Author.2001.lpd")
  L <- suppressWarnings(lipdR::readLipd(p))
  L$createdBy <- "matlab"
  fs::file_delete(p)
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))

  f <- qc_frame(d, progress = FALSE)
  expect_equal(unique(f$value[f$field == "createdBy"]), "matlab")
})

test_that("both survive together on one dataset", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1",
            col_extra = list(createdBy = "hydroclimate2k"))
  p <- fs::path(d, "A.Author.2001.lpd")
  L <- suppressWarnings(lipdR::readLipd(p))
  L$createdBy <- "matlab"
  fs::file_delete(p)
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))

  f <- qc_frame(d, progress = FALSE)
  expect_equal(unique(f$value[f$field == "createdBy"]), "matlab")
  expect_equal(unique(f$value[f$field == "paleoData_createdBy"]), "hydroclimate2k")
})
