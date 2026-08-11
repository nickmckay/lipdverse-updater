mk_index <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  write_lpd(d, "A.Author.2001", datasetId = "DS-A", tsids = c("T1", "T2"))
  write_lpd(d, "B.Author.2002", datasetId = "DS-B", tsids = "T3")
  lv_db_index(lv_scan(d))
}

test_that("a seed of TSids resolves to itself", {
  idx <- mk_index()
  r <- lv_resolve_seed(c("T1", "T3"), idx, by = "TSid")
  expect_equal(r$by, "TSid")
  expect_setequal(r$tsids, c("T1", "T3"))
  expect_length(r$unmatched, 0)
})

test_that("a seed of dataset names resolves to that dataset's timeseries", {
  idx <- mk_index()
  r <- lv_resolve_seed("A.Author.2001", idx, by = "dataSetName")
  expect_equal(r$by, "dataSetName")
  expect_setequal(r$tsids, c("T1", "T2"))
})

test_that("a seed of datasetIds resolves too", {
  idx <- mk_index()
  r <- lv_resolve_seed("DS-B", idx, by = "datasetId")
  expect_equal(r$by, "datasetId")
  expect_equal(r$tsids, "T3")
})

test_that("the seed kind must be stated, not inferred", {
  # A wrong guess does not fail: it builds a plausible compilation of the wrong
  # things. So the caller says which it is, and asking is a separate question.
  idx <- mk_index()
  expect_error(lv_resolve_seed(c("T1", "T3"), idx), class = "lv_error_compilation")
})

test_that("claiming the wrong kind fails instead of resolving to nothing", {
  # Saying dataSetName over a list of TSids is a mistake worth an error, not an
  # empty compilation that looks like the list simply was not in the database.
  idx <- mk_index()
  expect_error(lv_resolve_seed(c("T1", "T2"), idx, by = "dataSetName"),
               class = "lv_error_compilation")
})

test_that("lv_seed_kind reports what a list holds without acting on it", {
  idx <- mk_index()
  k <- lv_seed_kind(c("T1", "T2", "A.Author.2001"), idx)
  expect_setequal(k$kind, c("TSid", "datasetId", "dataSetName"))
  expect_equal(k$matched[k$kind == "TSid"], 2L)
  expect_equal(k$matched[k$kind == "dataSetName"], 1L)
  expect_equal(k$matched[k$kind == "datasetId"], 0L)
  # Ordered so the likeliest reading is first, but it decides nothing.
  expect_equal(k$kind[1], "TSid")
})

test_that("values that match nothing are reported, not dropped silently", {
  # A list that half-matches is the dangerous case: the compilation comes out
  # smaller than intended and nothing says so.
  idx <- mk_index()
  r <- lv_resolve_seed(c("T1", "NOPE", "ALSO-NOPE"), idx, by = "TSid")
  expect_equal(r$tsids, "T1")
  expect_setequal(r$unmatched, c("NOPE", "ALSO-NOPE"))
  expect_equal(nrow(r$issues), 2)
})

test_that("a seed matching nothing at all is an error", {
  idx <- mk_index()
  expect_error(lv_resolve_seed(c("X", "Y"), idx, by = "TSid"), class = "lv_error_compilation")
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

  qc  <- lv_resolve_seed("C.Author.2003", idx, by = "dataSetName", scope = "qc")
  all <- lv_resolve_seed("C.Author.2003", idx, by = "dataSetName", scope = "all")
  expect_lt(length(qc$tsids), length(all$tsids))
  expect_length(all$tsids, 2)
})
