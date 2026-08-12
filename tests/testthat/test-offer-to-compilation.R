test_that("a dataset ingested for a compilation is detected from its files", {
  # The files record which compilation minted a column, so the question survives
  # the batch that answered it. A run receipt would not.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1",
            col_extra = list(createdBy = "hydroclimate2k"))
  write_lpd(d, "B.Author.2002", tsids = "T2")
  write_lpd(d, "C.Author.2003", tsids = "T3",
            col_extra = list(createdBy = "iso2k"))

  got <- lv_datasets_created_by(c("A.Author.2001", "B.Author.2002", "C.Author.2003"),
                                "hydroclimate2k", d)
  expect_equal(got, "A.Author.2001")
})

test_that("a missing file is not a match", {
  d <- withr::local_tempdir()
  expect_length(lv_datasets_created_by("Nope.Author.2001", "hydroclimate2k", d), 0)
})

test_that("offering appends only the datasets not already listed", {
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "datasetsInCompilation",
              data.frame(dsn = "Old.Author.2000", dsid = "D0", inComp = TRUE,
                         stringsAsFactors = FALSE))
  idx <- list(datasets = tibble::tibble(
    dataSetName = c("Old.Author.2000", "New.Author.2001"),
    datasetId = c("D0", "D1")))
  cfg <- list(qc_sheet_id = "S1", qc_tabs = list(datasets = "datasetsInCompilation"))

  add <- lv_offer_to_compilation(c("Old.Author.2000", "New.Author.2001"), cfg, bk, idx,
                                 dry_run = FALSE)
  back <- sheet_read(bk, "S1", "datasetsInCompilation")
  expect_equal(nrow(back), 2)
  expect_equal(add$dsn, "New.Author.2001")
  # TRUE is what makes it visible in the QC tab for review.
  expect_true(as.logical(back$inComp[back$dsn == "New.Author.2001"]))
  # Same type as the column it was appended to.
  expect_type(back$inComp, typeof(back$inComp[back$dsn == "Old.Author.2000"]))
  # The existing row is untouched: this appends, it does not rewrite.
  expect_equal(back$dsid[back$dsn == "Old.Author.2000"], "D0")
})

test_that("a new compilation's sheet has somewhere to record QC", {
  # Certification columns come from the registry, not from the data: a brand new
  # compilation has no certification values, so building columns only from the
  # cells leaves a curator able to review but unable to record it.
  reg <- lv_qc_fields()
  cells <- tibble::tibble(tsid = "T1", field = "paleoData_variableName",
                          value = "temperature", present = TRUE)
  idx <- list(timeseries = tibble::tibble(TSid = "T1", dataSetName = "A.Author.2001"),
              datasets = tibble::tibble(fileDataSetName = "A.Author.2001",
                                        datasetId = "D1"))

  plain <- lv_compilation_sheet(cells, idx, reg)
  withcsm <- lv_compilation_sheet(cells, idx, reg, compilation = "hydroclimate2k")

  expect_false(any(grepl("Certification", names(plain$QC))))
  expect_true(any(grepl("Certification", names(withcsm$QC))))
  # Blank, not invented.
  cert <- grep("Certification", names(withcsm$QC), value = TRUE)[1]
  expect_true(all(is.na(withcsm$QC[[cert]])))
})
