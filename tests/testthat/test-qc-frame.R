test_that("a dataset-level field is read from the root, not from a column copy", {
  # 1,131 files carry archiveType on their columns as well as at the root, and
  # 4,086 of those copies disagree with their own root -- mostly stale spellings
  # from before standardisation ("tree" for Wood, "marine sediment" for
  # MarineSediment). The column value used to overwrite the root, so the QC sheet
  # showed the junk while the file's real archiveType was correct. Read that way
  # it is not a change to anything, so no merge ever corrects it.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"),
            archiveType = "Wood", col_extra = list(archiveType = "tree"))

  f <- qc_frame(d, progress = FALSE)
  at <- f[f$field == "archiveType", ]
  expect_equal(unique(at$value), "Wood")
  expect_setequal(at$tsid, c("T1", "T2"))
})

test_that("a column still supplies a dataset-level field the root lacks", {
  # The root wins only when it has something to say. Dropping the column value
  # unconditionally would lose the field for any file that only carries it there.
  d <- withr::local_tempdir()
  write_lpd(d, "B.Author.2002", tsids = "T1", archiveType = NULL,
            col_extra = list(archiveType = "Coral"))

  f <- qc_frame(d, progress = FALSE)
  expect_equal(f$value[f$field == "archiveType"], "Coral")
})

test_that("a timeseries-level field is still read from the column", {
  # The rule is scoped by cardinality, not by "root beats column" generally:
  # units vary per column and must keep coming from there.
  d <- withr::local_tempdir()
  write_lpd(d, "C.Author.2003", tsids = c("T1", "T2"))

  f <- qc_frame(d, progress = FALSE)
  expect_equal(unique(f$value[f$field == "paleoData_units"]), "degC")
})

test_that("an accented dataset name is read despite a decomposed filename", {
  # The live database sits in Dropbox, which stores filenames decomposed (NFD)
  # while the name inside the file is composed (NFC). The two render identically
  # and compare unequal, so `basename(paths) %in% datasets` skipped every
  # accented dataset: five hydroclimate2k rows reached the QC sheet carrying
  # nothing but a TSid and a membership flag, which is what the curators saw.
  #
  # The fixture forces the decomposition rather than relying on the filesystem
  # to supply it: a temp dir on APFS preserves whatever name it is given, so a
  # plain accented name here would not reproduce the mismatch at all.
  nfc <- "CentralEurope.Büntgen.2011"
  nfd <- stringi::stri_trans_nfd(nfc)
  d <- withr::local_tempdir()
  write_lpd(d, nfd, tsids = "T1")
  expect_false(identical(nfc, nfd))

  f <- qc_frame(d, datasets = nfc, progress = FALSE)
  expect_gt(nrow(f), 0)
  expect_true("archiveType" %in% f$field)

  # And the other direction, since which side is decomposed depends on where the
  # name came from.
  g <- qc_frame(d, datasets = nfd, progress = FALSE)
  expect_equal(nrow(g), nrow(f))
})
