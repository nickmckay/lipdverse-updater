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

test_that("remove clears the value instead of mapping it", {
  # paleoData_proxy = "Depth" is the case: depth is an axis, and no proxy term
  # is the right answer. Mapping it anywhere would put a wrong fact in the file.
  r <- tibble::tibble(field = "paleoData_proxy", value = "Depth",
                      map_to = NA_character_, also_field = NA_character_,
                      also_value = NA_character_, action = "remove")
  c0 <- list(TSid = "T1", variableName = "depth", proxy = "Depth", values = list(1))
  out <- lv_apply_remap(c0, r, "D", "T1")
  expect_null(out$proxy)
  expect_equal(out$variableName, "depth")
})

test_that("removing is logged as a removal, not a change to nothing", {
  seen <- list()
  r <- tibble::tibble(field = "paleoData_proxy", value = "Depth", map_to = NA_character_,
                      also_field = NA_character_, also_value = NA_character_,
                      action = "remove")
  c0 <- list(TSid = "T1", proxy = "Depth", values = list(1))
  lv_apply_remap(c0, r, "D", "T1", log = function(e) seen[[length(seen) + 1L]] <<- e)
  expect_length(seen, 1)
  expect_equal(seen[[1]]$rule, "remove")
  expect_equal(seen[[1]]$from, "Depth")
  expect_true(is.na(seen[[1]]$to))
})

test_that("a column whose proxy differs is left alone", {
  r <- tibble::tibble(field = "paleoData_proxy", value = "Depth", map_to = NA_character_,
                      also_field = NA_character_, also_value = NA_character_,
                      action = "remove")
  c0 <- list(TSid = "T1", proxy = "ring width", values = list(1))
  expect_equal(lv_apply_remap(c0, r, "D", "T1")$proxy, "ring width")
})

test_that("a remap table without an action column still decomposes", {
  # Decisions recorded before `remove` existed have no action; they must keep
  # working rather than being read as removals.
  r <- tibble::tibble(field = "paleoData_variableName", value = "Jan precip",
                      map_to = "precipitation",
                      also_field = "interpretation_seasonality", also_value = "Jan")
  c0 <- list(TSid = "T1", variableName = "Jan precip", values = list(1))
  out <- lv_apply_remap(c0, r, "D", "T1")
  expect_equal(out$variableName, "precipitation")
})

test_that("a field is read by its exact name, never by prefix", {
  # `$` on a list partial-matches when the exact name is absent. A column with
  # proxyObservationType and no proxy had its variableName read as a proxy, so
  # "Depth" and "% sand" were offered for review as proxy values. Deciding
  # either would have written a proxy onto a column that never had one.
  cl <- list(TSid = "T1", variableName = "Depth",
             proxyObservationType = "Depth", unitsOriginal = "cm")
  expect_null(cl[["proxy"]])
  expect_null(cl[["units"]])
  # This is what the old code saw:
  expect_equal(cl$proxy, "Depth")
  expect_equal(cl$units, "cm")
})

test_that("standardizing leaves a column with no real proxy alone", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1",
            col_extra = list(proxyObservationType = "Depth"))
  out <- withr::local_tempdir()
  std <- lv_ingest_standardize(d, out, progress = FALSE)

  # proxyObservationType is a stale variableName copy, not a proxy, so nothing
  # about it should be reported as an unknown proxy value.
  iss <- tibble::as_tibble(std$issues)
  expect_false(any(iss$field == "paleoData_proxy" & iss$value == "Depth"))
  L <- suppressWarnings(lipdR::readLipd(fs::path(out, "A.Author.2001.lpd")))
  cols <- lv_cols_of(L$paleoData[[1]]$measurementTable[[1]])
  expect_true(all(vapply(cols, function(c) is.null(c[["proxy"]]), logical(1))))
})
