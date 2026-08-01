setup_db <- function(envir = parent.frame()) {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir(.local_envir = envir),
                      .local_envir = envir)
  d <- withr::local_tempdir(.local_envir = envir)
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  write_lpd(d, "B.Author.2002", tsids = c("T3", "T4"))
  d
}

memberships <- function(L, tsid) {
  tab <- L$paleoData[[1]]$measurementTable[[1]]
  col <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), tsid), tab)[[1]]
  vapply(col$inCompilation, function(e) as.character(unlist(e$compilationName))[1], character(1))
}

test_that("membership is added to exactly the named TSids", {
  src <- setup_db(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  r <- lv_create_compilation(c("T1", "T3"), "lipdverseTest", "1_0_0", src, out,
                             index = idx, progress = FALSE)

  expect_equal(r$placed, 2)
  expect_setequal(r$datasets, c("A.Author.2001", "B.Author.2002"))

  A <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  expect_equal(memberships(A, "T1"), "lipdverseTest")
  # The column that was not named stays out of the compilation.
  tab <- A$paleoData[[1]]$measurementTable[[1]]
  t2 <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T2"), tab)[[1]]
  expect_null(t2$inCompilation)
})

test_that("an existing membership for another compilation is preserved", {
  src <- setup_db(); out1 <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "iso2k", "1_0_0", src, out1, index = idx, progress = FALSE)

  idx2 <- lv_db_index(lv_scan(out1, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", out1, out2, index = idx2, progress = FALSE)

  A <- lipdR::readLipd(fs::path(out2, "A.Author.2001.lpd"))
  expect_setequal(memberships(A, "T1"), c("iso2k", "lipdverseTest"))
})

test_that("re-running adds a version rather than a duplicate membership", {
  src <- setup_db(); out1 <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", src, out1, index = idx, progress = FALSE)

  idx2 <- lv_db_index(lv_scan(out1, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_1", out1, out2, index = idx2, progress = FALSE)

  A <- lipdR::readLipd(fs::path(out2, "A.Author.2001.lpd"))
  expect_equal(memberships(A, "T1"), "lipdverseTest")
  tab <- A$paleoData[[1]]$measurementTable[[1]]
  col <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tab)[[1]]
  expect_setequal(unlist(col$inCompilation[[1]]$compilationVersion), c("1_0_0", "1_0_1"))
})

test_that("re-running the same version is idempotent", {
  src <- setup_db(); out1 <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", src, out1, index = idx, progress = FALSE)
  idx2 <- lv_db_index(lv_scan(out1, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", out1, out2, index = idx2, progress = FALSE)

  A <- lipdR::readLipd(fs::path(out2, "A.Author.2001.lpd"))
  col <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"),
                A$paleoData[[1]]$measurementTable[[1]])[[1]]
  expect_equal(unlist(col$inCompilation[[1]]$compilationVersion), "1_0_0")
})

# Two columns in the real database hold `inCompilation: "Tverse"` -- a bare
# string where a list of entries belongs. The obvious guard, `if (!is.list(ic))
# ic <- list()`, throws that membership away.
test_that("a bare-string inCompilation is converted, not discarded", {
  src <- setup_db(); mid <- withr::local_tempdir(); out <- withr::local_tempdir()
  L <- lipdR::readLipd(fs::path(src, "A.Author.2001.lpd"))
  tab <- L$paleoData[[1]]$measurementTable[[1]]
  cn <- names(Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tab))[1]
  L$paleoData[[1]]$measurementTable[[1]][[cn]]$inCompilation <- "Tverse"
  lipdR::writeLipd(L, path = mid, removeNamesFromLists = TRUE)

  idx <- lv_db_index(lv_scan(mid, cache = FALSE), cache = FALSE)
  r <- lv_create_compilation("T1", "lipdverseTest", "1_0_0", mid, out,
                             index = idx, progress = FALSE)

  A <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  expect_setequal(memberships(A, "T1"), c("Tverse", "lipdverseTest"))
  expect_true("malformed_inCompilation" %in% r$issues$check)
})

test_that("an unknown TSid is reported, not silently dropped", {
  src <- setup_db(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  r <- lv_create_compilation(c("T1", "NOPE"), "lipdverseTest", "1_0_0", src, out,
                             index = idx, progress = FALSE)
  expect_true("tsid_not_in_database" %in% r$issues$check)
  expect_equal(r$placed, 1)
})

test_that("output passes the writer's verification", {
  src <- setup_db(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation(c("T1", "T3"), "lipdverseTest", "1_0_0", src, out,
                        index = idx, progress = FALSE)
  for (f in fs::dir_ls(out, glob = "*.lpd")) {
    expect_equal(nrow(lv_verify_file(f, sub("\\.lpd$", "", fs::path_file(f)))), 0)
  }
})

test_that("it refuses to write in place or to run with no TSids", {
  src <- setup_db()
  expect_error(lv_create_compilation("T1", "x", dir = src), "never writes in place")
  expect_error(lv_create_compilation(character(), "x", dir = src, out = withr::local_tempdir()),
               class = "lv_error_compilation")
})

test_that("lv_compilation_sheet builds a QC tab and a membership tab", {
  src <- setup_db(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  cells <- qc_frame(src, progress = FALSE)
  cells <- cells[cells$tsid %in% c("T1", "T3"), , drop = FALSE]

  tabs <- lv_compilation_sheet(cells, idx)
  expect_setequal(names(tabs), c("QC", "datasetsInCompilation"))
  expect_equal(names(tabs$QC)[1], "TSid")
  expect_setequal(tabs$QC$TSid, c("T1", "T3"))
  expect_setequal(tabs$datasetsInCompilation$dsn, c("A.Author.2001", "B.Author.2002"))
  expect_true(all(tabs$datasetsInCompilation$inComp == "TRUE"))
})

# ---- membership growth -----------------------------------------------------
#
# How a compilation actually grows: a curator sets a dataset TRUE in
# datasetsInCompilation, all its timeseries appear in the QC tab with
# inThisCompilation FALSE, and they flag individual ones TRUE to admit them.

test_that("the file-side membership frame marks non-members FALSE", {
  src <- setup_db(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", src, out, index = idx, progress = FALSE)

  idx2 <- lv_db_index(lv_scan(out, cache = FALSE), cache = FALSE)
  f <- lv_membership_frame(idx2, "lipdverseTest", c("T1", "T2"))
  expect_equal(f$value[f$tsid == "T1"], "TRUE")
  # A candidate, which is what makes it visible to the curator.
  expect_equal(f$value[f$tsid == "T2"], "FALSE")
})

test_that("flagging TRUE admits a timeseries", {
  src <- setup_db(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  cells <- tibble::tibble(tsid = c("T1", "T2"), field = "inThisCompilation",
                          value = c("TRUE", "FALSE"))
  r <- lv_apply_membership(cells, idx, "lipdverseTest", dir = src, out = out, progress = FALSE)

  expect_equal(r$added, "T1")
  expect_length(r$removed, 0)
  A <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  expect_equal(memberships(A, "T1"), "lipdverseTest")
})

test_that("flagging FALSE removes a timeseries, leaving other compilations", {
  src <- setup_db(); mid <- withr::local_tempdir(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "iso2k", "1_0_0", src, mid, index = idx, progress = FALSE)
  idx2 <- lv_db_index(lv_scan(mid, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", mid, mid, index = idx2, progress = FALSE)

  idx3 <- lv_db_index(lv_scan(mid, cache = FALSE), cache = FALSE)
  r <- lv_apply_membership(tibble::tibble(tsid = "T1", field = "inThisCompilation",
                                          value = "FALSE"),
                           idx3, "lipdverseTest", dir = mid, out = out, progress = FALSE)
  expect_equal(r$removed, "T1")
  A <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  expect_equal(memberships(A, "T1"), "iso2k")
})

# Most cells in a real QC tab are blank. If blank meant "remove", the first run
# against a real sheet would empty the compilation.
test_that("a blank never removes a member", {
  src <- setup_db(); mid <- withr::local_tempdir(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", src, mid, index = idx, progress = FALSE)

  idx2 <- lv_db_index(lv_scan(mid, cache = FALSE), cache = FALSE)
  r <- lv_apply_membership(tibble::tibble(tsid = "T1", field = "inThisCompilation",
                                          value = NA_character_),
                           idx2, "lipdverseTest", dir = mid, out = out, progress = FALSE)
  expect_length(r$removed, 0)
  expect_length(fs::dir_ls(out, glob = "*.lpd"), 0)
})

test_that("removing the only membership drops the key rather than leaving it empty", {
  src <- setup_db(); mid <- withr::local_tempdir(); out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_create_compilation("T1", "lipdverseTest", "1_0_0", src, mid, index = idx, progress = FALSE)

  idx2 <- lv_db_index(lv_scan(mid, cache = FALSE), cache = FALSE)
  lv_apply_membership(tibble::tibble(tsid = "T1", field = "inThisCompilation", value = "FALSE"),
                      idx2, "lipdverseTest", dir = mid, out = out, progress = FALSE)
  A <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  col <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"),
                A$paleoData[[1]]$measurementTable[[1]])[[1]]
  expect_null(col$inCompilation)
})

test_that("the membership tab catalogues the whole database, not just members", {
  src <- setup_db()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  cells <- tibble::tibble(tsid = "T1", field = "archiveType", value = "Coral",
                          present = TRUE, dataset_id = "DS1")
  tabs <- lv_compilation_sheet(cells, idx)

  # B.Author.2002 contributes no cells, but must still be listed so a curator
  # can flip it to TRUE.
  expect_setequal(tabs$datasetsInCompilation$dsn, c("A.Author.2001", "B.Author.2002"))
  inc <- tabs$datasetsInCompilation
  expect_equal(inc$inComp[inc$dsn == "A.Author.2001"], "TRUE")
  expect_equal(inc$inComp[inc$dsn == "B.Author.2002"], "FALSE")
})
