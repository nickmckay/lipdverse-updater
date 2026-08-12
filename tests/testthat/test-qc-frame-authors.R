test_that("an author record flattens to a name, not R source text", {
  # Authors arrive as an array of records. as.character() on that returns the
  # code that would rebuild it, so the QC sheet showed
  # list(name = "Castaneda, Isla S.") where a curator expected a name -- and an
  # edit to a neighbouring cell would have written that string back as the author.
  expect_equal(lv_scalar_value(list(list(name = "Castaneda, Isla S."))),
               "Castaneda, Isla S.")
  expect_equal(lv_scalar_value(list(list(name = "A, B"), list(name = "C, D"))),
               "A, B; C, D")
})

test_that("a plain value is untouched", {
  expect_equal(lv_scalar_value("Smith, J."), "Smith, J.")
  expect_equal(lv_scalar_value(1850), "1850")
  expect_equal(lv_scalar_value(TRUE), "TRUE")
})

test_that("a list with no name field still yields something readable", {
  expect_equal(lv_scalar_value(list("A", "B")), "A; B")
})

test_that("publication authors reach the frame as names", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  f <- qc_frame(d, progress = FALSE)
  au <- f$value[grepl("author", f$field, ignore.case = TRUE)]
  expect_true(length(au) > 0)
  expect_false(any(grepl("^list\\(", au)))
  expect_true(any(grepl("Author, A.", au, fixed = TRUE)))
})

test_that("an author written back becomes a record array again", {
  # The read flattens an author array to one string for the sheet. Writing that
  # string straight back makes the pub section invalid -- validLipd reports
  # "author field should be a list" -- so the write has to unflatten it.
  expect_equal(lv_author_records("Smith, J."), list(list(name = "Smith, J.")))
  expect_equal(lv_author_records("A, B; C, D"),
               list(list(name = "A, B"), list(name = "C, D")))
  expect_null(lv_author_records(NA_character_))
  expect_null(lv_author_records(""))
})

test_that("a dataset survives the author round trip and stays valid", {
  # The failure this prevents: five files refused by the write gate because a
  # flattened author string had replaced the array.
  d <- withr::local_tempdir()
  # Two variables: validLipd rejects a measurement table with fewer.
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  out <- withr::local_tempdir()

  cells <- tibble::tibble(tsid = "T1", dataSetName = "A.Author.2001",
                          field = "pub1_author",
                          value = "Sp\u00f6tl, C.; Schr\u00f6der-Ritzrau, A.",
                          present = TRUE)
  iss <- lv_apply_qc(cells, dir = d, out = out, progress = FALSE)
  expect_equal(lv_n_issues(iss, "error"), 0)

  L2 <- suppressWarnings(lipdR::readLipd(fs::path(out, "A.Author.2001.lpd")))
  au <- L2$pub[[1]]$author
  expect_true(is.list(au))
  expect_equal(length(au), 2)
  expect_equal(au[[1]]$name, "Sp\u00f6tl, C.")
  # And the file it produced is valid LiPD, which is what the gate checks.
  expect_equal(nrow(lv_verify_file(fs::path(out, "A.Author.2001.lpd"), "A.Author.2001")), 0)
})
