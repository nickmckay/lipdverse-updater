cells <- function(tsid, field, value) tibble::tibble(
  tsid = tsid, field = field, value = value, present = !is.na(value), dataset_id = "d")

test_that("a curator can clear a field they own", {
  # The LS12THAY01C case. She deleted climateVariableDetail and basis in the
  # sheet; they came back, because a blank was read as "unchanged" for every
  # field except four, so the clear never became an event and the file value was
  # pushed back into her empty cell.
  f <- "climateInterpretation1_variableDetail"
  b <- cells("T1", f, "a detail")
  s <- cells("T1", f, NA_character_); s$present <- TRUE   # present in the sheet, blank
  fr <- cells("T1", f, "a detail")
  p <- qc_merge(b, s, fr, lv_qc_fields())
  r <- p$cells[p$cells$field == f, ]
  expect_true(r$sheet_clears)
  expect_true(is.na(r$value))
})

test_that("a blank the sheet does not carry is still not a deletion", {
  # The distinction that keeps the old daff loss from returning: a cell absent
  # from the sheet means "no opinion", not "delete".
  f <- "climateInterpretation1_variableDetail"
  b <- cells("T1", f, "a detail")
  fr <- cells("T1", f, "a detail")
  p <- qc_merge(b, cells(character(), character(), character()), fr, lv_qc_fields())
  r <- p$cells[p$cells$field == f, ]
  expect_false(isTRUE(r$sheet_clears))
  expect_equal(r$value, "a detail")
})

test_that("a blank on a field the curator does not own never deletes", {
  f <- "archiveType"                      # shared
  b <- cells("T1", f, "Wood")
  s <- cells("T1", f, NA_character_); s$present <- TRUE
  fr <- cells("T1", f, "Wood")
  r <- qc_merge(b, s, fr, lv_qc_fields())$cells
  r <- r[r$field == f, ]
  expect_false(isTRUE(r$sheet_clears))
  expect_equal(r$value, "Wood")
})

test_that("membership is not clearable by blanking", {
  expect_false(lv_qc_fields()$nullable_by_curator[
    lv_qc_fields()$qc_name == "inThisCompilation"] %in% TRUE)
})

test_that("every nullable field is curator-owned", {
  r <- lv_qc_fields()
  expect_true(all(r$ownership[r$nullable_by_curator %in% TRUE] == "curator"))
})

test_that("a blank cell survives the sheet pull for a clearable field", {
  d <- withr::local_tempdir(); bk <- sheet_backend_local(d)
  sheet_write(bk, "s", "QC", tibble::tibble(
    TSid = c("T1", "T2"),
    climateInterpretation1_variableDetail = c("kept", "")))
  out <- qc_sheet_pull(bk, "s", "QC")
  r <- out[out$tsid == "T2" & out$field == "climateInterpretation1_variableDetail", ]
  expect_equal(nrow(r), 1)      # the blank is carried, not dropped
  expect_false(r$present)
})
