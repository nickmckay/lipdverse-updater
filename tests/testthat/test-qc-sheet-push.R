test_that("a push never adds a column the sheet does not have", {
  # Columns are added by a curator, on the sheet. A run that adds one commits a
  # compilation to a field nobody asked for -- and this sheet grew from 77 to
  # 122 columns that way once already.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  tab <- data.frame(TSid = c("T1", "T2"), units = c("degC", "permil"),
                    stringsAsFactors = FALSE)
  sheet_write(bk, "S1", "QC", tab)

  cells <- tibble::tibble(
    tsid = c("T1", "T2", "T1"),
    field = c("paleoData_units", "paleoData_units", "paleoData_proxy"),
    value = c("degC", "per mil", "ring width"), present = TRUE,
    dataset_id = c("D1", "D2", "D1"))

  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "full", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")

  expect_setequal(names(back), names(tab))
  expect_true("paleoData_proxy" %in% r$skipped_fields | "proxy" %in% r$skipped_fields)
  # The value that does have a column is still written.
  expect_equal(back$units[back$TSid == "T2"], "per mil")
})

test_that("a column the run has nothing for keeps what the sheet holds", {
  # Restricting to the sheet's columns must not blank the ones this run did not
  # touch: the block is written positionally, so every column has to be present.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  tab <- data.frame(TSid = c("T1", "T2"), units = c("degC", "permil"),
                    notes = c("keep me", "and me"), stringsAsFactors = FALSE)
  sheet_write(bk, "S1", "QC", tab)

  cells <- tibble::tibble(tsid = "T1", field = "paleoData_units",
                          value = "K", present = TRUE, dataset_id = "D1")
  qc_sheet_push(cells, bk, "S1", "QC", mode = "full", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")

  expect_equal(back$notes[back$TSid == "T1"], "keep me")
  expect_setequal(names(back), names(tab))
})
