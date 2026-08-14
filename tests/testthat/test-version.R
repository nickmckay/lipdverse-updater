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
  v <- lv_tick_version("0_4_9", c("a"), c("a"), publish = TRUE)
  expect_equal(v$version, "1_0_0")
  expect_equal(v$reason, "published")
})

# Every compilation in LiPDverse begins at 0_0_1 and stays at publication 0
# until it is actually published. hydroclimate2k reached 0_4_0 unpublished.
test_that("the first version of a compilation is 0_0_1", {
  v <- lv_tick_version(NULL, character(), c("a", "b"))
  expect_equal(v$version, "0_0_1")
  expect_true(is.na(v$previous))
  expect_equal(v$n_datasets, 2)
})

# The case Nick corrected: hydroclimate2k at 0_4_0, this run changes which
# datasets are in the compilation, so the dataset component ticks and metadata
# resets.
test_that("a dataset change at 0_4_0 gives 0_5_0", {
  v <- lv_tick_version("0_4_0", c("a", "b"), c("a", "b", "c"))
  expect_equal(v$version, "0_5_0")
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
  expect_equal(lv_version_current(st, "iso2k"), "0_0_1")

  v2 <- lv_tick_version(lv_version_current(st, "iso2k"), c("a", "b"), c("a", "b"))
  lv_version_append(st, "iso2k", v2, run_id = "r2")
  expect_equal(lv_version_current(st, "iso2k"), "0_0_2")
  expect_equal(nrow(lv_versions(st)), 2)

  # Another compilation does not disturb this one.
  lv_version_append(st, "Temp12k", lv_tick_version(NULL, character(), "z"), run_id = "r3")
  expect_equal(lv_version_current(st, "iso2k"), "0_0_2")
  expect_equal(lv_version_current(st, "Temp12k"), "0_0_1")
})

test_that("membership is recorded per version, not as a joined string", {
  st <- qc_store(withr::local_tempdir())
  lv_version_append(st, "iso2k", lv_tick_version(NULL, character(), c("a", "b")), run_id = "r1")
  lv_version_append(st, "iso2k", lv_tick_version("0_0_1", c("a", "b"), c("a", "b", "c")),
                    run_id = "r2")
  m <- readr::read_csv(fs::path(st$path, "version_datasets.csv"),
                       col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE)
  expect_setequal(m$dataset[m$version == "0_0_1"], c("a", "b"))
  expect_setequal(m$dataset[m$version == "0_1_0"], c("a", "b", "c"))
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

test_that("the ledger records which version of each dataset a release contained", {
  # A compilation page for a published version has to embed the dataset pages as
  # they were, and those live at /data/<datasetId>/<version>. The dataset name
  # alone cannot say which version to embed.
  withr::local_envvar(LIPDVERSE_QCSTORE = withr::local_tempdir())
  st <- qc_store()
  v <- lv_tick_version(NULL, character(), c("A.Author.2001", "B.Author.2002"))
  detail <- tibble::tibble(dataset = c("A.Author.2001", "B.Author.2002"),
                           datasetId = c("ID1", "ID2"),
                           datasetVersion = c("1.0.5", "2.1.0"))
  lv_version_append(st, "c1", v, run_id = "r1", members = detail)

  m <- lv_version_members(st, compilation = "c1")
  expect_equal(nrow(m), 2)
  expect_equal(m$datasetId[m$dataset == "A.Author.2001"], "ID1")
  expect_equal(m$datasetVersion[m$dataset == "B.Author.2002"], "2.1.0")
})

test_that("a release recorded without that detail still reads back", {
  # Every version published before the ledger carried it is in this state, and
  # always will be: it was not written down and cannot be reconstructed.
  withr::local_envvar(LIPDVERSE_QCSTORE = withr::local_tempdir())
  st <- qc_store()
  v <- lv_tick_version(NULL, character(), "A.Author.2001")
  lv_version_append(st, "c1", v, run_id = "r1")
  m <- lv_version_members(st, compilation = "c1")
  expect_equal(m$dataset, "A.Author.2001")
  expect_true(is.na(m$datasetId))
  expect_true(is.na(m$datasetVersion))
})

test_that("the ledger can be asked what one version held", {
  withr::local_envvar(LIPDVERSE_QCSTORE = withr::local_tempdir())
  st <- qc_store()
  v1 <- lv_tick_version(NULL, character(), c("A.Author.2001", "B.Author.2002"))
  lv_version_append(st, "c1", v1, run_id = "r1")
  v2 <- lv_tick_version(v1$version, c("A.Author.2001", "B.Author.2002"), "A.Author.2001")
  lv_version_append(st, "c1", v2, run_id = "r2")

  expect_equal(nrow(lv_version_members(st, "c1", v1$version)), 2)
  expect_equal(lv_version_members(st, "c1", v2$version)$dataset, "A.Author.2001")
})

test_that("recording a version twice replaces its membership rather than doubling it", {
  # A run that produces an unchanged version used to append every membership row
  # again: NAm21k-noPollen 0_1_0 was in the ledger three times, 1,152 duplicate
  # keys, and the export contract refused the table.
  withr::local_envvar(LIPDVERSE_QCSTORE = withr::local_tempdir())
  st <- qc_store()
  v <- lv_tick_version(NULL, character(), c("A.Author.2001", "B.Author.2002"))
  lv_version_append(st, "c1", v, run_id = "r1")
  lv_version_append(st, "c1", v, run_id = "r2")

  m <- lv_version_members(st, "c1", v$version)
  expect_equal(nrow(m), 2)
  expect_equal(sum(duplicated(paste(m$compilation, m$version, m$dataset))), 0)
})

test_that("re-recording one version leaves the others alone", {
  withr::local_envvar(LIPDVERSE_QCSTORE = withr::local_tempdir())
  st <- qc_store()
  v1 <- lv_tick_version(NULL, character(), "A.Author.2001")
  lv_version_append(st, "c1", v1, run_id = "r1")
  v2 <- lv_tick_version(v1$version, "A.Author.2001", c("A.Author.2001", "B.Author.2002"))
  lv_version_append(st, "c1", v2, run_id = "r2")
  lv_version_append(st, "c1", v2, run_id = "r3")

  expect_equal(nrow(lv_version_members(st, "c1", v1$version)), 1)
  expect_equal(nrow(lv_version_members(st, "c1", v2$version)), 2)
})
