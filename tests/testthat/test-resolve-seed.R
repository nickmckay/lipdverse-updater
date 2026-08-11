mk_index <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  write_lpd(d, "A.Author.2001", datasetId = "DS-A", tsids = c("T1", "T2"))
  write_lpd(d, "B.Author.2002", datasetId = "DS-B", tsids = "T3")
  lv_db_index(lv_scan(d))
}

test_that("a seed of TSids resolves to itself", {
  idx <- mk_index()
  r <- lv_resolve_seed(c("T1", "T3"), idx)
  expect_equal(r$by, "TSid")
  expect_setequal(r$tsids, c("T1", "T3"))
  expect_length(r$unmatched, 0)
})

test_that("a seed of dataset names resolves to that dataset's timeseries", {
  idx <- mk_index()
  r <- lv_resolve_seed("A.Author.2001", idx)
  expect_equal(r$by, "dataSetName")
  expect_setequal(r$tsids, c("T1", "T2"))
})

test_that("a seed of datasetIds resolves too", {
  idx <- mk_index()
  r <- lv_resolve_seed("DS-B", idx)
  expect_equal(r$by, "datasetId")
  expect_equal(r$tsids, "T3")
})

test_that("the kind is detected by matching, not by appearance", {
  # TSids, datasetIds and dataSetNames have no reliable shape between them, so
  # guessing from the look of a string would be wrong quietly. Forcing the wrong
  # interpretation must find nothing rather than invent something.
  idx <- mk_index()
  r <- lv_resolve_seed("T1", idx, by = "dataSetName")
  expect_length(r$tsids, 0)
  expect_equal(r$unmatched, "T1")
  expect_true(any(r$issues$check == "seed_not_found"))
})

test_that("values that match nothing are reported, not dropped silently", {
  # A list that half-matches is the dangerous case: the compilation comes out
  # smaller than intended and nothing says so.
  idx <- mk_index()
  r <- lv_resolve_seed(c("T1", "NOPE", "ALSO-NOPE"), idx)
  expect_equal(r$tsids, "T1")
  expect_setequal(r$unmatched, c("NOPE", "ALSO-NOPE"))
  expect_equal(nrow(r$issues), 2)
})

test_that("a seed matching nothing at all is an error", {
  idx <- mk_index()
  expect_error(lv_resolve_seed(c("X", "Y"), idx), class = "lv_error_compilation")
})

test_that("dataset seeds exclude axis columns unless asked", {
  # A compilation of year and depth columns is not meaningful, and including
  # them puts them in the QC sheet -- the thing the h2k curators asked to have
  # taken out.
  d <- withr::local_tempdir()
  write_lpd(d, "C.Author.2003", datasetId = "DS-C", tsids = c("T9", "T8"))
  p <- fs::path(d, "C.Author.2003.lpd")
  L <- suppressWarnings(lipdR::readLipd(p))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$variableName <- "year"
  fs::file_delete(p)
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))
  idx <- lv_db_index(lv_scan(d))

  qc  <- lv_resolve_seed("C.Author.2003", idx, scope = "qc")
  all <- lv_resolve_seed("C.Author.2003", idx, scope = "all")
  expect_lt(length(qc$tsids), length(all$tsids))
  expect_length(all$tsids, 2)
})
