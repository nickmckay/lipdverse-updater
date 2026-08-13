# An incoming directory holding one dataset.
#
# This used to copy a file out of the live database, which made the test depend
# on the operator's machine: it passed here and failed in CI with ENOENT on
# ~/Dropbox/lipdverse/database, because a runner has no Dropbox. A fixture is
# also the honest thing to test against -- what matters is the identity logic,
# not which real dataset happened to sort first.
local_incoming <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  d
}

test_that("createdBy marks timeseries new to LiPDverse, not merely newly minted", {
  inc <- local_incoming()
  scan <- lv_ingest_scan(inc, progress = FALSE)
  # An index that already knows the first column's TSid, so that column is an
  # update, and knows nothing of the rest.
  # The datasetId must be the file's own, or the file reads as a different
  # dataset carrying someone else's TSid and the column is re-minted as template
  # reuse rather than treated as an update.
  did <- scan$datasetId[1]
  idx <- list(
    datasets = tibble::tibble(fileDataSetName = scan$dataSetName[1],
                              dataSetName = scan$dataSetName[1],
                              datasetId = did, path = NA_character_),
    timeseries = tibble::tibble(TSid = scan$TSid[1], datasetId = did,
                                dataSetName = scan$dataSetName[1],
                                tableType = "paleo", variableName = scan$variableName[1],
                                compilations = NA_character_))
  plan <- lv_ingest_identity(scan, idx)
  expect_equal(plan$action[1], "keep")           # same dataset, same TSid: an update

  out <- withr::local_tempdir()
  lv_ingest_apply(plan, inc, out, idx, compilation = "hydroclimate2k", progress = FALSE)
  L <- suppressWarnings(lipdR::readLipd(fs::dir_ls(out, glob = "*.lpd")[1]))

  cols_of <- function(tb) if (!is.null(tb$columns)) tb$columns else
    tb[!names(tb) %in% c("filename", "tableName", "missingValue")]
  got <- list()
  for (p in L$paleoData) for (tb in p$measurementTable) for (cl in cols_of(tb)) {
    if (!is.list(cl) || is.null(cl$TSid)) next
    got[[as.character(cl$TSid)]] <- as.character(cl$createdBy %||% NA)
  }
  # The one the database already held is untouched; everything else is stamped.
  expect_true(is.na(got[[scan$TSid[1]]]) || got[[scan$TSid[1]]] != "hydroclimate2k")
  others <- got[setdiff(names(got), scan$TSid[1])]
  expect_true(length(others) > 0)
  expect_true(all(unlist(others) == "hydroclimate2k"))
})

test_that("without a compilation nothing is stamped", {
  inc <- local_incoming()
  scan <- lv_ingest_scan(inc, progress = FALSE)
  idx <- list(datasets = tibble::tibble(fileDataSetName = character(), dataSetName = character(),
                                        datasetId = character(), path = character()),
              timeseries = tibble::tibble(TSid = character(), datasetId = character(),
                                          dataSetName = character(), tableType = character(),
                                          variableName = character(), compilations = character()))
  plan <- lv_ingest_identity(scan, idx)
  out <- withr::local_tempdir()
  lv_ingest_apply(plan, inc, out, idx, progress = FALSE)
  L <- suppressWarnings(lipdR::readLipd(fs::dir_ls(out, glob = "*.lpd")[1]))
  cols_of <- function(tb) if (!is.null(tb$columns)) tb$columns else
    tb[!names(tb) %in% c("filename", "tableName", "missingValue")]
  any_stamped <- FALSE
  for (p in L$paleoData) for (tb in p$measurementTable) for (cl in cols_of(tb))
    if (is.list(cl) && !is.null(cl$createdBy)) any_stamped <- TRUE
  expect_false(any_stamped)
})
