col <- function(...) c(list(TSid = "T1", variableName = "d13C_biweight_mean",
                            units = "permil", values = list(1, 2)), list(...))
rm1 <- function(...) tibble::tibble(field = "paleoData_variableName",
                                    value = "d13C_biweight_mean", map_to = "d13C", ...)

test_that("a second field is written even when it is on no QC sheet", {
  # summaryStatistic is on 87,810 columns and no QC sheet. Whether a curator
  # sees a field is decided in Google Sheets; whether the file can hold it is a
  # different question, and the file is what decompose writes to.
  r <- rm1(also_field = "paleoData_summaryStatistic", also_value = "biweight_mean")
  out <- lv_apply_remap(col(), r, "D", "T1")
  expect_equal(out$variableName, "d13C")
  expect_equal(out$summaryStatistic, "biweight_mean")
})

test_that("units decompose onto the column, which they previously did not", {
  # "% sand" -> sand + units = percent was accepted as a decision and silently
  # dropped the unit: only interpretation_* had a branch that wrote anything.
  r <- tibble::tibble(field = "paleoData_variableName", value = "% sand",
                      map_to = "sand", also_field = "paleoData_units",
                      also_value = "percent")
  c0 <- list(TSid = "T1", variableName = "% sand", values = list(1))
  out <- lv_apply_remap(c0, r, "D", "T1")
  expect_equal(out$variableName, "sand")
  expect_equal(out$units, "percent")
})

test_that("an existing value is not overwritten by the second field", {
  r <- rm1(also_field = "paleoData_summaryStatistic", also_value = "biweight_mean")
  out <- lv_apply_remap(col(summaryStatistic = "mean"), r, "D", "T1")
  expect_equal(out$summaryStatistic, "mean")
})

test_that("seasonality still lands in the interpretation", {
  r <- tibble::tibble(field = "paleoData_variableName", value = "Jan precip",
                      map_to = "precipitation",
                      also_field = "interpretation_seasonality", also_value = "Jan")
  c0 <- list(TSid = "T1", variableName = "Jan precip", values = list(1))
  out <- lv_apply_remap(c0, r, "D", "T1")
  expect_equal(out$variableName, "precipitation")
  expect_equal(out$interpretation[[1]]$seasonality, "Jan")
})

test_that("every write is logged, including the second field", {
  # The log is the changelog evidence. A silent write is as bad as a silent drop.
  seen <- list()
  r <- rm1(also_field = "paleoData_summaryStatistic", also_value = "biweight_mean")
  lv_apply_remap(col(), r, "D", "T1", log = function(e) seen[[length(seen) + 1L]] <<- e)
  fields <- vapply(seen, function(e) e$field, character(1))
  expect_setequal(fields, c("paleoData_variableName", "paleoData_summaryStatistic"))
})

test_that("a dataset-level field is not written onto a column", {
  # archiveType describes the dataset. Writing it per column is the flattening
  # artifact that put junk into the h2k QC sheet while the root was correct.
  expect_true(is.na(lv_col_key("archiveType")))
  r <- rm1(also_field = "archiveType", also_value = "Wood")
  out <- lv_apply_remap(col(), r, "D", "T1")
  expect_null(out$archiveType)
})

test_that("lv_remap_check reports targets that cannot be written", {
  ok  <- tibble::tibble(field = "paleoData_variableName", value = "x", map_to = "y",
                        also_field = "paleoData_summaryStatistic", also_value = "mean")
  bad <- tibble::tibble(field = "paleoData_variableName", value = "x", map_to = "y",
                        also_field = "archiveType", also_value = "Wood")
  expect_equal(nrow(lv_remap_check(ok)), 0)
  expect_true(any(lv_remap_check(bad)$check == "remap_also_field_not_writable"))
})
