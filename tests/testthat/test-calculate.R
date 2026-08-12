# Calculated fields: derived from the data on every update, never curated.

test_that("min/max year mask to timesteps that carry a measurement", {
  calc <- lv_calculators()
  year <- c(1900, 1901, 1902, 1903)
  vals <- c(NA, 5, 6, NA)
  # The axis spans 1900-1903, but data exists only for 1901-1902. Reporting the
  # axis would claim coverage the column does not have.
  expect_equal(calc$minYear(year, NULL, vals), 1901)
  expect_equal(calc$maxYear(year, NULL, vals), 1902)
})

test_that("a column with no finite measurement has no year range", {
  calc <- lv_calculators()
  expect_true(is.na(calc$minYear(c(1900, 1901), NULL, c(NA, NA))))
  expect_true(is.na(calc$maxYear(c(1900, 1901), NULL, c(NaN, NaN))))
})

test_that("an axis of a different length is used unmasked rather than mispaired", {
  # Pairing by position across mismatched lengths would mask by whichever
  # elements happened to line up, which is worse than not masking.
  calc <- lv_calculators()
  expect_equal(calc$minYear(c(1900, 1901, 1902), NULL, c(NA, 5)), 1900)
})

test_that("age converts to year with the no-year-zero correction", {
  # There is no year 0: 1950 - 5437 = -3487 is 3488 BCE, written -3488. This is
  # what geoChronR::convertBP2AD does and what every existing hydroclimate2k
  # minYear already carries; without it a run rewrites 4,715 cells by one.
  calc <- lv_calculators()
  vals <- rep(1, 3)
  expect_equal(calc$minYear(NULL, c(0, 100, 5437), vals), -3488)
  expect_equal(calc$maxYear(NULL, c(0, 100, 5437), vals), 1950)
  # Positive years are untouched.
  expect_equal(calc$minYear(NULL, c(50, 100), c(1, 1)), 1850)
})

test_that("year is preferred over age when both are present", {
  calc <- lv_calculators()
  expect_equal(calc$maxYear(c(1000, 1010), c(500, 490), c(1, 1)), 1010)
})

test_that("distinctYearsInCommonEra counts distinct whole years in the CE", {
  # Transcribed from lipdverseR/distinctYearsInCommonEra.R: floor, keep 0-2025,
  # count unique. Fractional samples within one year count once.
  f <- lv_calculators()$distinctYearsInCommonEra
  expect_equal(f(c(1000.2, 1000.8, 1001.1), NULL, NULL), 2)
  # Outside the common era is excluded, not clamped.
  expect_equal(f(c(-500, 1000, 3000), NULL, NULL), 1)
  # Falling back to age, whose window is the same span counted the other way.
  expect_equal(f(NULL, c(0, 1, 1950, 3000), NULL), 3)
  expect_equal(f(NULL, c(-75, -80), NULL), 1)
  expect_true(is.na(f(NULL, NULL, NULL)))
})

test_that("distinctYearsInCommonEra counts only years that carry a value", {
  # The question is how many distinct years have a measurement, so a year whose
  # value is blank does not count (Nick, 2026-08-12). The reference
  # implementation counted the axis alone, which makes every existing
  # hydroclimate2k value an upper bound.
  f <- lv_calculators()$distinctYearsInCommonEra
  expect_equal(f(c(1000, 1001, 1002), NULL, c(NA, NA, 5)), 1)
  expect_equal(f(c(1000, 1001, 1002), NULL, c(3, NA, 5)), 2)
  # Masking applies to the age fallback too.
  expect_equal(f(NULL, c(0, 1, 2), c(NA, 5, 6)), 2)
  # A column with no data at all has no distinct years, which clears the cell.
  expect_true(is.na(f(c(1000, 1001), NULL, c(NA, NA))))
})

test_that("a calculation runs only where the QC sheet has the column", {
  reg <- lv_qc_fields()
  # The header is the request: a lead adds the column to ask for the value.
  expect_equal(lv_calculations_for(c("TSid", "minYear", "maxYear"), reg),
               c("minYear", "maxYear"))
  expect_equal(lv_calculations_for(c("TSid", "archiveType"), reg), character())
  # Display names resolve through the registry like any other column.
  expect_true("distinctYearsInCommonEra" %in%
                lv_calculations_for(c("TSid", "distinctYearsInCommonEra"), reg))
  expect_equal(lv_calculations_for(character(), reg), character())
})

test_that("whole numbers are formatted without a decimal tail", {
  # "1980.0000" against the sheet's "1980" would read as a change on every run.
  expect_equal(lv_format_number(1980), "1980")
  expect_equal(lv_format_number(-3488), "-3488")
  expect_equal(lv_format_number(1883.583), "1883.583")
})
