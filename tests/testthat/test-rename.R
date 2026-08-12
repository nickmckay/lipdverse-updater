# Renaming a dataset from the QC sheet.
#
# The file takes its name from its metadata, so applying a corrected dataSetName
# writes a file under the new name. Without the rest of a rename that is a fork:
# the old file stays and a promote adds a duplicate.

rename_index <- function(names = c("DSNF.Bregy.222", "Other.Author.2001"),
                         ids = c("ID1", "ID2")) {
  ts <- tibble::tibble(
    TSid = paste0("T", seq_along(names)),
    dataSetName = names,
    datasetId = ids,
    tableType = "paleo",
    variableName = "temperature",
    compilations = replicate(length(names), character(), simplify = FALSE))
  structure(list(datasets = tibble::tibble(fileDataSetName = names, dataSetName = names,
                                           datasetId = ids, path = paste0(names, ".lpd")),
                 timeseries = ts), class = "lv_index")
}

ren_cells <- function(tsid, value) {
  tibble::tibble(tsid = tsid, field = "dataSetName", value = value,
                 present = TRUE, dataset_id = "ID1")
}

test_that("a corrected dataSetName is recognised as a rename", {
  r <- lv_planned_renames(ren_cells("T1", "DeSotoNationalForest.Bregy.2022"), rename_index())
  expect_equal(nrow(r), 1)
  expect_equal(r$dataSetName, "DSNF.Bregy.222")
  expect_equal(r$new_name, "DeSotoNationalForest.Bregy.2022")
  expect_equal(r$file_old, "DSNF.Bregy.222.lpd")
  expect_equal(r$file_new, "DeSotoNationalForest.Bregy.2022.lpd")
  # The identity travels with the rename; the filename is not the identity.
  expect_equal(r$datasetId, "ID1")
  expect_true(is.na(r$issue))
})

test_that("a name that has not changed is not a rename", {
  r <- lv_planned_renames(ren_cells("T1", "DSNF.Bregy.222"), rename_index())
  expect_equal(nrow(r), 0)
})

test_that("the two spellings of an accented name are not a rename", {
  # A filename is decomposed and a sheet value composed. Comparing raw would
  # report a rename on every run forever, and rename the file to itself.
  nfc <- "CentralEurope.Büntgen.2011"
  idx <- rename_index(names = c(stringi::stri_trans_nfd(nfc), "Other.Author.2001"))
  expect_equal(nrow(lv_planned_renames(ren_cells("T1", nfc), idx)), 0)
})

test_that("rows disagreeing on the new name are refused, not guessed", {
  cells <- dplyr::bind_rows(ren_cells("T1", "One.Author.2001"),
                            ren_cells("T1", "Two.Author.2001"))
  r <- lv_planned_renames(cells, rename_index())
  expect_equal(nrow(r), 1)
  expect_match(r$issue, "disagree")
  expect_equal(lv_rename_issues(r)$severity, "error")
})

test_that("renaming onto an existing dataset is refused", {
  # Otherwise the promote would overwrite a different dataset that happens to
  # hold the target name.
  r <- lv_planned_renames(ren_cells("T1", "Other.Author.2001"), rename_index())
  expect_equal(nrow(r), 1)
  expect_match(r$issue, "already exists")
  expect_equal(lv_rename_issues(r)$severity, "error")
})

test_that("a name that cannot be a filename is refused", {
  r <- lv_planned_renames(ren_cells("T1", "bad/name.2001"), rename_index())
  expect_match(r$issue, "filename")
})

test_that("a safe rename reports as info, not as a problem", {
  r <- lv_planned_renames(ren_cells("T1", "DeSotoNationalForest.Bregy.2022"), rename_index())
  i <- lv_rename_issues(r)
  expect_equal(i$severity, "info")
  expect_equal(lv_n_issues(i, "error"), 0)
})

# ---- the promote side ------------------------------------------------------

test_that("a named deletion retires the old file without allow_delete", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "DSNF.Bregy.222"),
                        list(dataSetName = "Keep.Author.2001")))
  stage <- local_db(list(list(dataSetName = "DeSotoNationalForest.Bregy.2022")))

  r <- lv_promote(stage, live, dry_run = FALSE, partial = TRUE,
                  delete = "DSNF.Bregy.222.lpd")
  expect_true(r$committed)
  expect_true(fs::file_exists(fs::path(live, "DeSotoNationalForest.Bregy.2022.lpd")))
  # The old name is gone from the database but recoverable.
  expect_false(fs::file_exists(fs::path(live, "DSNF.Bregy.222.lpd")))
  expect_true(any(grepl("DSNF.Bregy.222", fs::dir_ls(fs::path(live, ".trash"), recurse = TRUE))))
  # Everything else is untouched, which is what `partial` is for.
  expect_true(fs::file_exists(fs::path(live, "Keep.Author.2001.lpd")))
})

test_that("a file cannot be both written and deleted by one run", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9"))))
  expect_error(lv_promote(stage, live, dry_run = TRUE, partial = TRUE,
                          delete = "A.Author.2001.lpd"),
               class = "lv_error_write")
})

test_that("a named deletion that is not in the database is ignored", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001")))
  stage <- local_db(list(list(dataSetName = "B.Author.2002")))
  r <- lv_promote(stage, live, dry_run = TRUE, partial = TRUE,
                  delete = "NeverExisted.lpd")
  expect_equal(length(r$deletions), 0)
})

test_that("an unnamed deletion still needs allow_delete", {
  # The named-deletion path must not become a way to delete by accident: a live
  # file simply absent from a full staging is still a candidate deletion that
  # has to be authorised.
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  live <- local_db(list(list(dataSetName = "A.Author.2001"),
                        list(dataSetName = "B.Author.2002")))
  stage <- local_db(list(list(dataSetName = "A.Author.2001", tsids = c("T1", "T9"))))
  expect_error(lv_promote(stage, live, dry_run = TRUE, partial = FALSE),
               class = "lv_error_write")
})
