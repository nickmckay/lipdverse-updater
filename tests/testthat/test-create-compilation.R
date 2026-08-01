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
