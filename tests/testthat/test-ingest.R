# A TSid already in LiPDverse means one of two opposite things, and the
# difference decides whether it is preserved or replaced. Nick's rule: if the
# file is the dataset that holds it -- matching datasetId, or no datasetId yet
# and a matching dataSetName -- it is an update. Otherwise someone used an
# existing file as a template and left its identifiers behind.

fake_index <- function() {
  structure(list(
    datasets = tibble::tibble(
      path = c("a.lpd", "b.lpd"), file = c("a.lpd", "b.lpd"), md5 = c("1", "2"),
      dataSetName = c("Existing.Author.2020", "Other.Author.2019"),
      datasetId = c("DSID_EXIST", "DSID_OTHER"),
      datasetVersion = NA_character_, archiveType = NA_character_,
      n_ts = c(2L, 1L), parse_error = NA_character_,
      fileDataSetName = c("Existing.Author.2020", "Other.Author.2019")),
    timeseries = tibble::tibble(
      TSid = c("T_EXIST1", "T_EXIST2", "T_OTHER"),
      datasetId = c("DSID_EXIST", "DSID_EXIST", "DSID_OTHER"),
      dataSetName = c("Existing.Author.2020", "Existing.Author.2020", "Other.Author.2019"),
      tableType = "paleo", tableKind = "measurement",
      variableName = c("d18O", "age", "d13C"),
      compilations = list(character(), character(), character()))
  ), class = "lv_index")
}

incoming <- function(...) {
  x <- tibble::tribble(~file, ~dataSetName, ~datasetId, ~variableName, ~TSid, ...)
  x$block <- "paleoData"; x$paleo <- 1L; x$table <- 1L
  x$column <- seq_len(nrow(x)); x$error <- NA_character_
  x
}

act <- function(p, tsid) p$action[!is.na(p$TSid) & p$TSid == tsid]

test_that("a matching datasetId makes an existing TSid an update", {
  p <- lv_ingest_identity(
    incoming("f.lpd", "Existing.Author.2020", "DSID_EXIST", "d18O", "T_EXIST1"),
    fake_index())
  expect_equal(p$action, "keep")
  expect_equal(p$new_TSid, "T_EXIST1")
  expect_match(p$reason, "update")
})

test_that("a matching dataSetName with no datasetId is also an update", {
  p <- lv_ingest_identity(
    incoming("f.lpd", "Existing.Author.2020", NA_character_, "d18O", "T_EXIST1"),
    fake_index())
  expect_equal(p$action, "keep")
  expect_equal(p$new_TSid, "T_EXIST1")
})

# The template case: the TSid belongs to someone else's dataset.
test_that("an existing TSid in an unrelated file is re-minted", {
  p <- lv_ingest_identity(
    incoming("new.lpd", "Brand.New.2026", NA_character_, "d18O", "T_EXIST1"),
    fake_index())
  expect_equal(p$action, "remint")
  expect_false(p$new_TSid == "T_EXIST1")
  expect_match(p$reason, "template reuse")
})

test_that("a non-matching datasetId is template reuse even if the name matches", {
  p <- lv_ingest_identity(
    incoming("f.lpd", "Existing.Author.2020", "DSID_SOMETHING_ELSE", "d18O", "T_EXIST1"),
    fake_index())
  expect_equal(p$action, "remint")
})

test_that("a TSid repeated across submissions is re-minted everywhere", {
  p <- lv_ingest_identity(
    incoming("a.lpd", "A.Author.2026", NA_character_, "d18O", "WEB-shared",
             "b.lpd", "B.Author.2026", NA_character_, "d18O", "WEB-shared"),
    fake_index())
  expect_equal(p$action, c("remint", "remint"))
  # No arbitrary blessing: neither keeps the copied id, and they differ.
  expect_false(any(p$new_TSid == "WEB-shared"))
  expect_equal(dplyr::n_distinct(p$new_TSid), 2)
})

test_that("a missing TSid is minted", {
  p <- lv_ingest_identity(
    incoming("f.lpd", "Brand.New.2026", NA_character_, "d18O", NA_character_),
    fake_index())
  expect_equal(p$action, "mint")
  expect_true(nzchar(p$new_TSid))
  expect_false(p$new_TSid %in% fake_index()$timeseries$TSid)
})

test_that("a genuinely new unique TSid is kept", {
  p <- lv_ingest_identity(
    incoming("f.lpd", "Brand.New.2026", NA_character_, "d18O", "T_BRANDNEW"),
    fake_index())
  expect_equal(p$action, "keep")
  expect_equal(p$new_TSid, "T_BRANDNEW")
})

test_that("minted ids collide with nothing", {
  p <- lv_ingest_identity(
    incoming("a.lpd", "A.2026", NA_character_, "v1", NA_character_,
             "a.lpd", "A.2026", NA_character_, "v2", NA_character_,
             "b.lpd", "B.2026", NA_character_, "v3", "T_EXIST2"),
    fake_index())
  ids <- p$new_TSid
  expect_equal(anyDuplicated(ids), 0)
  expect_false(any(ids %in% fake_index()$timeseries$TSid))
})

# An update and a template can name the same TSid in one batch: the real owner
# keeps it, the impostor does not.
test_that("an update keeps its TSid while a template copy of it is re-minted", {
  p <- lv_ingest_identity(
    incoming("real.lpd", "Existing.Author.2020", "DSID_EXIST", "d18O", "T_EXIST1",
             "fake.lpd", "Copied.Author.2026", NA_character_, "d18O", "T_EXIST1"),
    fake_index())
  expect_equal(p$action[p$file == "real.lpd"], "keep")
  expect_equal(p$new_TSid[p$file == "real.lpd"], "T_EXIST1")
  expect_equal(p$action[p$file == "fake.lpd"], "remint")
  expect_false(p$new_TSid[p$file == "fake.lpd"] == "T_EXIST1")
})

test_that("issues report the re-mints and not the routine mints", {
  p <- lv_ingest_identity(
    incoming("new.lpd", "Brand.New.2026", NA_character_, "d18O", "T_EXIST1",
             "f2.lpd", "Another.2026", NA_character_, "d18O", NA_character_),
    fake_index())
  iss <- lv_ingest_issues(p)
  expect_equal(nrow(iss), 1)
  expect_equal(iss$check, "tsid_template_reuse")
  expect_equal(iss$severity, "warn")
})

test_that("scanning finds columns with and without TSids", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  s <- lv_ingest_scan(d, progress = FALSE)
  expect_true(all(c("file", "dataSetName", "TSid", "variableName") %in% names(s)))
  expect_true(all(c("T1", "T2") %in% s$TSid))
})

# ---- applying the plan -----------------------------------------------------

test_that("applying the plan writes the resolved TSids and mints a datasetId", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = c("T1", "T2"))
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)

  s <- lv_ingest_scan(src, progress = FALSE)
  # Pretend T1 belongs to a different dataset already in LiPDverse.
  fake <- idx
  fake$timeseries <- tibble::tibble(TSid = "T1", datasetId = "OTHER_ID",
                                    dataSetName = "Someone.Else.1999",
                                    tableType = "paleo", tableKind = "measurement",
                                    variableName = "x", compilations = list(character()))
  fake$datasets <- tibble::tibble(path = "x.lpd", file = "x.lpd", md5 = "1",
                                  dataSetName = "Someone.Else.1999", datasetId = "OTHER_ID",
                                  datasetVersion = NA_character_, archiveType = NA_character_,
                                  n_ts = 1L, parse_error = NA_character_,
                                  fileDataSetName = "Someone.Else.1999")
  p <- lv_ingest_identity(s, fake)
  expect_true("remint" %in% p$action)

  r <- lv_ingest_apply(p, src, out, fake, progress = FALSE)
  expect_length(r$staged, 1)
  expect_length(r$skipped, 0)

  L <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  got <- vapply(lv_ingest_walk(L), function(w) w$variableName, character(1))
  expect_equal(length(got), nrow(p))
  tab <- L$paleoData[[1]]$measurementTable[[1]]
  tsids <- unlist(lapply(tab, function(c) if (is.list(c)) as.character(c$TSid)[1] else NULL))
  # The re-minted one is gone; nothing collides with the database.
  expect_false("T1" %in% tsids)
  expect_false(any(tsids %in% fake$timeseries$TSid))
  expect_true(!is.null(L$datasetId) && nzchar(L$datasetId))
})

# One real submission declares 13 columns but leaves the measurement table's
# filename empty, so readLipd returns none of them. Assigning positionally there
# would write TSids onto the wrong columns.
test_that("a file whose structure does not match the plan is skipped, not mis-assigned", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = c("T1", "T2"))
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  p <- lv_ingest_identity(lv_ingest_scan(src, progress = FALSE), idx)
  # Claim a column the file does not have.
  p <- dplyr::bind_rows(p, dplyr::mutate(p[1, ], column = 99L, variableName = "ghost",
                                         TSid = NA_character_, new_TSid = "T_GHOST"))

  r <- lv_ingest_apply(p, src, out, idx, progress = FALSE)
  expect_length(r$staged, 0)
  expect_equal(r$skipped, "A.Author.2001.lpd")
  expect_equal(r$issues$check, "structure_mismatch")
  expect_equal(r$issues$severity, "error")
  expect_length(fs::dir_ls(out, glob = "*.lpd"), 0)
})

test_that("an existing datasetId is left alone", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); mid <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = "T1")
  L <- lipdR::readLipd(fs::path(src, "A.Author.2001.lpd"))
  L$datasetId <- "KEEP_ME"
  lipdR::writeLipd(L, path = mid, removeNamesFromLists = TRUE)

  idx <- lv_db_index(lv_scan(mid, cache = FALSE), cache = FALSE)
  p <- lv_ingest_identity(lv_ingest_scan(mid, progress = FALSE), idx)
  lv_ingest_apply(p, mid, out, idx, progress = FALSE)
  expect_equal(lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))$datasetId, "KEEP_ME")
})

test_that("apply refuses to write in place", {
  expect_error(lv_ingest_apply(data.frame(), ".", index = list()), "never writes in place")
})

# The loss can happen on write, not read, so the check has to be on the staged
# file. Verified against the real corpus rather than a fixture: the submission
# Switzerland.Pfister.1992 leaves its measurement table's filename empty, so
# lipdR never processes the table -- readLipd still reports all 13 columns, but
# writeLipd emits a dataset with no columns and no CSV member at all, 2 KB where
# a dataset should be. lv_ingest_apply catches it and skips the file.
#
# Not reproduced synthetically: lipdR infers the CSV by convention when the
# filename is blank, so a doctored fixture round-trips cleanly. Recreating the
# failure means matching lipdR's inference internals, which would test those
# rather than this guard. The positive case is asserted here.
test_that("staged output is verified against the plan after writing", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = c("T1", "T2"))
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  plan <- lv_ingest_identity(lv_ingest_scan(src, progress = FALSE), idx)

  r <- lv_ingest_apply(plan, src, out, idx, progress = FALSE)
  expect_length(r$staged, 1)
  expect_equal(nrow(r$issues), 0)

  # The staged file really carries the columns and the data, not just metadata.
  f <- fs::path(out, "A.Author.2001.lpd")
  expect_equal(length(lv_ingest_walk(lipdR::readLipd(f))), nrow(plan))
  expect_true(any(grepl("[.]csv$", utils::unzip(f, list = TRUE)$Name)))
})
