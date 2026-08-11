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

test_that("a patch writes only the changed cells and leaves the rest alone", {
  # A rewrite discards the colour coding the leads navigate by, so a run that
  # changes a handful of cells must not rewrite the tab. `mode` used to be
  # ignored entirely, so every push was a rewrite whatever was asked for.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(
    TSid = c("T1", "T2", "T3"), units = c("degC", "permil", "cm"),
    notes = c("a", "b", "c"), stringsAsFactors = FALSE))

  # The full state for the rows this run manages, with one value changed.
  cells <- tibble::tibble(
    tsid = c("T1", "T2", "T3"), field = "paleoData_units",
    value = c("degC", "per mil", "cm"), present = TRUE, dataset_id = "D1")
  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")

  expect_equal(back$units, c("degC", "per mil", "cm"))
  expect_equal(back$notes, c("a", "b", "c"))   # untouched
  expect_equal(nrow(back), 3)                  # no rows added or removed
  expect_equal(r$n_written, 1)
})

test_that("a patch skips a change with no cell rather than writing it elsewhere", {
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(TSid = c("T1"), units = "degC",
                                          stringsAsFactors = FALSE))
  cells <- tibble::tibble(
    tsid = c("T1", "T9"), field = "paleoData_units",
    value = c("K", "K"), present = TRUE, dataset_id = "D1")

  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")
  expect_equal(back$units, "K")
  expect_equal(nrow(back), 1)
  expect_equal(r$n_written, 1)
  expect_equal(r$skipped_cells$tsid, "T9")
})

test_that("a patch leaves rows this run does not cover alone", {
  # The hydroclimate2k case: the state covers 4,849 rows and the sheet holds
  # 7,525, the difference being axis rows that are to be removed deliberately
  # later. The diff calls them deletions; blanking them would be the run
  # destroying data it was not asked to touch.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(
    TSid = c("T1", "T2", "AXIS1"), units = c("degC", "permil", "yr"),
    stringsAsFactors = FALSE))

  cells <- tibble::tibble(tsid = c("T1", "T2"), field = "paleoData_units",
                          value = c("K", "permil"), present = TRUE, dataset_id = "D1")
  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")

  expect_equal(back$units[back$TSid == "AXIS1"], "yr")   # untouched
  expect_equal(back$units[back$TSid == "T1"], "K")
  expect_equal(nrow(back), 3)
  expect_equal(r$n_written, 1)
})
