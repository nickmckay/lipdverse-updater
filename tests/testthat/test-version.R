test_that("version strings round-trip", {
  expect_equal(unname(lv_version_parse("1_0_3")), c(1L, 0L, 3L))
  expect_equal(lv_version_string(1, 0, 3), "1_0_3")
  expect_error(lv_version_parse("1.0.3"), class = "lv_error_version")
  expect_error(lv_version_parse("v1_0_3"), class = "lv_error_version")
})

test_that("an unchanged dataset set ticks metadata only", {
  v <- lv_tick_version("1_2_5", c("a", "b"), c("b", "a"))
  expect_equal(v$version, "1_2_6")
  expect_match(v$reason, "unchanged")
})

test_that("a changed dataset set ticks dataset and resets metadata", {
  v <- lv_tick_version("1_2_5", c("a", "b"), c("a", "b", "c"))
  expect_equal(v$version, "1_3_0")
  expect_equal(v$added, "c")
  expect_length(v$removed, 0)
})

test_that("a removal counts as a dataset change", {
  v <- lv_tick_version("1_2_5", c("a", "b"), "a")
  expect_equal(v$version, "1_3_0")
  expect_equal(v$removed, "b")
})

# lipdverseR decided this with `all(lastUdsn == thisUdsn)` on two sorted
# character vectors. `==` recycles when the lengths differ, so a set that grew
# by a whole multiple could compare equal element-by-element and tick metadata
# for a run that had actually changed the dataset set.
test_that("a dataset set that grew by a multiple is not mistaken for unchanged", {
  before <- c("a", "b")
  now <- c("a", "b", "a2", "b2")
  # The bug: recycling makes the element-wise comparison look plausible.
  expect_length(sort(before) == sort(now), 4)
  v <- lv_tick_version("1_2_5", before, now)
  expect_equal(v$version, "1_3_0")
})

test_that("publishing bumps publication and resets the rest", {
  v <- lv_tick_version("1_4_9", c("a"), c("a"), publish = TRUE)
  expect_equal(v$version, "2_0_0")
  expect_equal(v$reason, "published")
})

test_that("the first version of a compilation is 1_0_0", {
  v <- lv_tick_version(NULL, character(), c("a", "b"))
  expect_equal(v$version, "1_0_0")
  expect_true(is.na(v$previous))
  expect_equal(v$n_datasets, 2)
})

test_that("the dataset set hash ignores order and duplicates", {
  expect_equal(lv_dataset_set_hash(c("b", "a")), lv_dataset_set_hash(c("a", "b", "a")))
  expect_false(lv_dataset_set_hash(c("a", "b")) == lv_dataset_set_hash(c("a", "c")))
  expect_true(is.na(lv_dataset_set_hash(character())))
})

# Two runs can both hold 700 datasets and not be the same 700.
test_that("same-size different-content sets hash differently", {
  expect_false(lv_dataset_set_hash(c("a", "b")) == lv_dataset_set_hash(c("a", "z")))
})

test_that("the ledger appends and reads back", {
  st <- qc_store(withr::local_tempdir())
  expect_equal(nrow(lv_versions(st)), 0)
  expect_null(lv_version_current(st, "iso2k"))

  v1 <- lv_tick_version(NULL, character(), c("a", "b"))
  lv_version_append(st, "iso2k", v1, run_id = "r1")
  expect_equal(lv_version_current(st, "iso2k"), "1_0_0")

  v2 <- lv_tick_version(lv_version_current(st, "iso2k"), c("a", "b"), c("a", "b"))
  lv_version_append(st, "iso2k", v2, run_id = "r2")
  expect_equal(lv_version_current(st, "iso2k"), "1_0_1")
  expect_equal(nrow(lv_versions(st)), 2)

  # Another compilation does not disturb this one.
  lv_version_append(st, "Temp12k", lv_tick_version(NULL, character(), "z"), run_id = "r3")
  expect_equal(lv_version_current(st, "iso2k"), "1_0_1")
  expect_equal(lv_version_current(st, "Temp12k"), "1_0_0")
})

test_that("membership is recorded per version, not as a joined string", {
  st <- qc_store(withr::local_tempdir())
  lv_version_append(st, "iso2k", lv_tick_version(NULL, character(), c("a", "b")), run_id = "r1")
  lv_version_append(st, "iso2k", lv_tick_version("1_0_0", c("a", "b"), c("a", "b", "c")),
                    run_id = "r2")
  m <- readr::read_csv(fs::path(st$path, "version_datasets.csv"),
                       col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE)
  expect_setequal(m$dataset[m$version == "1_0_0"], c("a", "b"))
  expect_setequal(m$dataset[m$version == "1_1_0"], c("a", "b", "c"))
})

test_that("a version carries provenance for reproducing the run", {
  st <- qc_store(withr::local_tempdir())
  v <- lv_tick_version(NULL, character(), "a")
  lv_version_append(st, "iso2k", v, run_id = "r1", db_fingerprint = "abc", notes = "hi")
  x <- lv_versions(st)
  expect_equal(x$db_fingerprint, "abc")
  expect_equal(x$run_id, "r1")
  expect_equal(x$notes, "hi")
  expect_false(is.na(x$lipdr_version))
  expect_equal(x$dataset_set_hash, lv_dataset_set_hash("a"))
})
