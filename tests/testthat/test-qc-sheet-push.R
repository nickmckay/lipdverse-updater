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

test_that("a patch never writes a value into the wrong row", {
  # The hazard that makes address-based writing worth testing: a TSid with no
  # row must not have its value land on whichever row happens to be there.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(TSid = "T1", units = "degC",
                                          stringsAsFactors = FALSE))
  cells <- tibble::tibble(
    tsid = c("T1", "T9"), field = "paleoData_units",
    value = c("K", "cm"), present = TRUE, dataset_id = "D1")

  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")
  expect_equal(back$units[back$TSid == "T1"], "K")     # its own value, not T9's
  expect_equal(back$units[back$TSid == "T9"], "cm")    # appended, not merged
  expect_equal(r$n_written, 1)
  expect_equal(r$n_appended, 1)
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

test_that("a patch appends a new timeseries rather than skipping it", {
  # A new row has no cell to address, but appending adds one without touching
  # the existing rows or their formatting. Skipping means a curator never sees
  # the timeseries; rewriting costs the colour coding.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(
    TSid = c("T1", "T2"), units = c("degC", "permil"), stringsAsFactors = FALSE))

  cells <- tibble::tibble(
    tsid = c("T1", "T2", "T3"), field = "paleoData_units",
    value = c("K", "permil", "cm"), present = TRUE, dataset_id = "D1")
  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE)
  back <- sheet_read(bk, "S1", "QC")

  expect_equal(nrow(back), 3)
  expect_equal(r$n_appended, 1)
  expect_equal(back$units[back$TSid == "T3"], "cm")
  expect_equal(back$units[back$TSid == "T1"], "K")     # patched
  expect_equal(back$units[back$TSid == "T2"], "permil")
  expect_setequal(names(back), c("TSid", "units"))     # no new columns
})

test_that("appending can be declined", {
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(TSid = "T1", units = "degC",
                                          stringsAsFactors = FALSE))
  cells <- tibble::tibble(tsid = c("T1", "T3"), field = "paleoData_units",
                          value = c("K", "cm"), present = TRUE, dataset_id = "D1")
  r <- qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE,
                     add_rows = FALSE)
  expect_equal(nrow(sheet_read(bk, "S1", "QC")), 1)
  expect_equal(r$rows_not_added, "T3")
})

test_that("sheet rows are ordered by dataSetName, not TSid", {
  # A curator works a dataset at a time, so its variables belong together. TSid
  # order scatters them, and a TSid is not a name anyone recognises.
  cells <- tibble::tibble(
    tsid = c("Z1", "A1", "Z2"),
    field = rep(c("dataSetName", "paleoData_units"), length.out = 6)[1:3],
    value = c("Alpha.Author.2001", "degC", "Alpha.Author.2001"),
    present = TRUE)
  cells <- tibble::tibble(
    tsid = c("Z1", "A1", "Z2"),
    field = "dataSetName",
    value = c("Beta.Author.2002", "Zulu.Author.2003", "alpha.Author.2001"),
    present = TRUE)

  w <- qc_cells_to_sheet(cells, lv_qc_fields())
  expect_equal(w$dataSetName, c("alpha.Author.2001", "Beta.Author.2002", "Zulu.Author.2003"))
  # Case-insensitive: "alpha" leads "Beta", which a case-sensitive sort reverses.
})

test_that("rows of one dataset stay together and in TSid order", {
  cells <- tibble::tibble(
    tsid = c("T2", "T1", "T3"), field = "dataSetName",
    value = c("Same.Author.2001", "Same.Author.2001", "Other.Author.2002"),
    present = TRUE)
  w <- qc_cells_to_sheet(cells, lv_qc_fields())
  expect_equal(w$TSid, c("T3", "T1", "T2"))
})

test_that("a row with no dataSetName sorts last rather than leading the sheet", {
  # An unnamed row is an anomaly; putting it at the top hides it behind the
  # assumption that the first rows are the normal ones.
  cells <- tibble::tibble(
    tsid = c("T1", "T2"), field = "dataSetName",
    value = c(NA_character_, "Alpha.Author.2001"), present = TRUE)
  w <- qc_cells_to_sheet(cells, lv_qc_fields())
  expect_equal(w$TSid, c("T2", "T1"))
})

test_that("a patch refuses to write if the rows moved underneath it", {
  # Curators sort their sheets. Sorting between the read and the write would
  # send every value to the wrong row, silently, which is worse than failing.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(
    TSid = c("T1", "T2", "T3"), units = c("degC", "permil", "cm"),
    stringsAsFactors = FALSE))
  before <- sheet_read(bk, "S1", "QC")

  cells <- tibble::tibble(tsid = c("T1", "T2", "T3"), field = "paleoData_units",
                          value = c("K", "permil", "cm"), present = TRUE,
                          dataset_id = "D1")

  # Return the sheet in a different row order on the verification read, as a
  # sort landing mid-run would.
  calls <- 0L
  real <- sheet_read
  testthat::local_mocked_bindings(
    sheet_read = function(backend, id, tab, ...) {
      calls <<- calls + 1L
      x <- real(backend, id, tab, ...)
      if (calls >= 3L) x[c(3, 1, 2), , drop = FALSE] else x
    },
    .package = "lipdverseUpdater")

  expect_error(
    qc_sheet_push(cells, bk, "S1", "QC", mode = "patch", dry_run = FALSE),
    class = "lv_error_sheet")
  # Nothing was written: the values are as they were.
  expect_equal(real(bk, "S1", "QC")$units, before$units)
})

test_that("a patch dry run reports the write, not the whole sheet", {
  # The diff between a partial state and a full sheet calls every untouched cell
  # a deletion. A patch ignores those, so counting them in the receipt made the
  # dry run report 261,663 changes for a write of 60 cells -- a preview that
  # could not be used to decide whether to proceed.
  d <- withr::local_tempdir()
  bk <- sheet_backend_local(d)
  sheet_write(bk, "S1", "QC", data.frame(
    TSid = c("T1", "T2"), units = c("degC", "permil"),
    archiveType = c("Wood", "LakeSediment"), stringsAsFactors = FALSE))

  fill <- tibble::tibble(tsid = "T1", field = "paleoData_units", value = "mm",
                         present = TRUE, dataset_id = "D1")

  r <- qc_sheet_push(fill, bk, "S1", "QC", mode = "patch", dry_run = TRUE)
  expect_equal(r$n_changed, 1)
  expect_equal(r$changed$tsid, "T1")
  # Everything else is reported as untouched rather than as a change.
  expect_gt(r$unmanaged_cells, 0)

  # And the dry run's count is what the real write then does.
  w <- qc_sheet_push(fill, bk, "S1", "QC", mode = "patch", dry_run = FALSE,
                     add_rows = FALSE)
  expect_equal(w$n_written, r$n_changed)
  back <- sheet_read(bk, "S1", "QC")
  expect_equal(back$units, c("mm", "permil"))
  expect_equal(back$archiveType, c("Wood", "LakeSediment"))
})
