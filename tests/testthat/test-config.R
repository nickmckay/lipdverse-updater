test_that("the registry covers every compilation exactly once", {
  reg <- lv_compilations()
  expect_gt(nrow(reg), 15)
  expect_equal(anyDuplicated(reg$compilation), 0)
  expect_true(all(c("compilation", "qc_sheet_id", "last_update_id",
                    "age_or_year", "lipd_dir") %in% names(reg)))
})

test_that("every compilation in the registry produces a valid config", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_no_error(lv_config_all())
})

test_that("defaults are layered under registry values", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  cfg <- lv_config("hydroclimate2k")
  expect_s3_class(cfg, "lv_config")
  expect_equal(cfg$age_or_year, "year")   # registry overrides the "age" default
  expect_equal(cfg$membership, "from_sheet")
  expect_true(cfg$strict)
})

test_that("... overrides win over everything", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  cfg <- lv_config("hydroclimate2k", cutover = "live", strict = FALSE)
  expect_equal(cfg$cutover, "live")
  expect_false(cfg$strict)
})

test_that("lipd_dir is per-compilation and resolves against the database root", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir(),
                      LIPDVERSE_DATABASE = "/data/lipdverse/database")
  expect_equal(lv_config("hydroclimate2k")$lipd_dir, "/data/lipdverse/database")
  # GBRCD is the one compilation with its own database.
  expect_equal(lv_config("GBRCD")$lipd_dir, "/data/lipdverse/GBRCD")
})

test_that("an unknown compilation errors and lists the known ones", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_error(lv_config("NotACompilation"), class = "lv_error_config")
})

test_that("invalid enum values are rejected", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_error(lv_config("hydroclimate2k", cutover = "whenever"), class = "lv_error_config")
  expect_error(lv_config("hydroclimate2k", age_or_year = "epoch"), class = "lv_error_config")
  expect_error(lv_config("hydroclimate2k", membership = "vibes"), class = "lv_error_config")
  expect_error(lv_config("hydroclimate2k", strict = "yes"), class = "lv_error_config")
})

test_that("a malformed sheet id is rejected", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_error(lv_config("hydroclimate2k", qc_sheet_id = "nope"), class = "lv_error_config")
})

test_that("each config carries a unique run id", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  expect_false(identical(lv_config("hydroclimate2k")$run_id,
                         lv_config("hydroclimate2k")$run_id))
})
