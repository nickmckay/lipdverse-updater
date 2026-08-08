test_that("axis columns are left out of the QC scope", {
  idx <- list(timeseries = tibble::tibble(
    TSid = paste0("T", 1:7),
    dataSetName = "D",
    tableType = "paleo",
    variableName = c("age", "year", "depth", "sampleID", "d18O", "trsgi", "Age")))
  keep <- lv_qc_timeseries(idx, datasets = "D")
  expect_setequal(keep, c("T5", "T6"))
  # Case does not save an axis column from being hidden.
  expect_false("T7" %in% keep)
  # And they can be asked for explicitly.
  expect_length(lv_qc_timeseries(idx, datasets = "D", axes = TRUE), 7)
})

test_that("chron is still excluded regardless of the axis rule", {
  idx <- list(timeseries = tibble::tibble(
    TSid = c("P1", "C1"), dataSetName = "D",
    tableType = c("paleo", "chron"), variableName = c("d18O", "d18O")))
  expect_equal(lv_qc_timeseries(idx, datasets = "D"), "P1")
  expect_equal(lv_qc_timeseries(idx, datasets = "D", axes = TRUE), "P1")
})

test_that("a field reaches the sheet column it is named by", {
  # `description` was filed as role=delete, so lv_display_field() -- which only
  # consults synonyms -- had no column for paleoData_description and 6,516
  # stored descriptions reached no column at all.
  r <- lv_qc_fields()
  expect_equal(lv_canonical_field("description", r), "paleoData_description")
  expect_equal(lv_display_field("paleoData_description", r), "description")
  expect_equal(lv_canonical_field(lv_display_field("paleoData_description", r), r),
               "paleoData_description")
})

test_that("the registry refuses a delete row that points at a live field", {
  x <- lv_qc_fields()
  x$role[x$qc_name == "description"] <- "delete"
  expect_error(validate_qc_fields(x), "pointing at a live field")
})

test_that("every merged field can round trip through its display name", {
  r <- lv_qc_fields()
  m <- r$qc_name[r$role == "merged"]
  back <- lv_canonical_field(lv_display_field(m, r), r)
  expect_equal(back, m)
})
