norm <- function(d) shadow_normalize(d, progress = FALSE)

test_that("normalize emits dataset-level and column-level rows", {
  d <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", tsids = c("T1", "T2"))))
  n <- norm(d)

  expect_true(all(c("dataSetName", "datasetId", "TSid", "field_path", "value") %in% names(n)))
  expect_true(any(is.na(n$TSid)))                       # dataset level
  expect_setequal(stats::na.omit(unique(n$TSid)), c("T1", "T2"))
  expect_true("archiveType" %in% n$field_path)
  expect_true("paleoData.variableName" %in% n$field_path)
})

test_that("normalize is deterministic", {
  d <- make_db(list(list(dataSetName = "A.Author.2001"), list(dataSetName = "B.Author.2002")))
  expect_identical(norm(d), norm(d))
})

test_that("identical directories produce an empty diff", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A")))
  b <- withr::local_tempdir()
  file.copy(fs::dir_ls(a, glob = "*.lpd"), b)
  expect_equal(nrow(shadow_diff(norm(a), norm(b))), 0)
})

# Columns are keyed by TSid, so a writer that emits them in a different order
# must not manufacture thousands of spurious differences.
test_that("column order does not create differences", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", tsids = c("T1", "T2", "T3"))))
  b <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", tsids = c("T3", "T1", "T2"))))
  expect_equal(nrow(shadow_diff(norm(a), norm(b))), 0)
})

test_that("diff classifies changed, added and removed fields", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         tsids = c("T1", "T2"), archiveType = "coral")))
  b <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         tsids = c("T1", "T3"), archiveType = "Coral")))
  d <- shadow_diff(norm(a), norm(b))

  arch <- d[d$field_path == "archiveType", ]
  expect_equal(arch$class, "differs")
  expect_equal(arch$old_value, "coral")
  expect_equal(arch$new_value, "Coral")

  expect_true(all(d$class[d$TSid %in% "T2"] == "only_old"))
  expect_true(all(d$class[d$TSid %in% "T3"] == "only_new"))
})

test_that("the ignore list removes volatile fields", {
  d <- make_db(list(list(dataSetName = "A.Author.2001")))
  expect_equal(sum(grepl("^changelog", norm(d)$field_path)), 0)
  expect_gt(sum(grepl("^changelog", shadow_normalize(d, ignore = NULL, progress = FALSE)$field_path)), 0)
})

test_that("numeric formatting differences are not differences", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", version = "1.0.0")))
  n <- shadow_normalize(a, ignore = NULL, progress = FALSE)
  # 1.10 and 1.1 must canonicalise to the same text.
  expect_equal(scalar_chr(1.10), scalar_chr(1.1))
  expect_equal(scalar_chr(0.5), "0.5")
})

# The regression that matters. lipdverseR quotes every CSV field; the new
# writer does not. Hashing raw bytes marked all 179 CoralHydro2k datasets as
# having changed data when the measurements were byte-for-byte equivalent.
test_that("CSV quoting differences are not data differences", {
  vals <- data.frame(depth = c(1.5, 2.5, 3.5), temp = c(-5.46, -5.52, -5.55))
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         csv = vals, csv_quote = TRUE)))
  b <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         csv = vals, csv_quote = FALSE)))

  na <- norm(a); nb <- norm(b)
  expect_gt(sum(grepl("^data\\.", na$field_path)), 0)   # the data row exists
  expect_equal(nrow(shadow_diff(na, nb)[grepl("^data\\.", shadow_diff(na, nb)$field_path), ]), 0)
})

test_that("a real data change is still detected", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         csv = data.frame(depth = c(1.5, 2.5), temp = c(-5.46, -5.52)))))
  b <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         csv = data.frame(depth = c(1.5, 2.5), temp = c(-5.46, -9.99)))))
  d <- shadow_diff(norm(a), norm(b))
  expect_equal(nrow(d[grepl("^data\\.", d$field_path), ]), 1)
})

test_that("a row-count change is detected", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         csv = data.frame(x = 1:3))))
  b <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A",
                         csv = data.frame(x = 1:4))))
  d <- shadow_diff(norm(a), norm(b))
  expect_equal(nrow(d[grepl("^data\\.", d$field_path), ]), 1)
})

test_that("an unreadable file is surfaced rather than skipped", {
  d <- make_db(list(list(dataSetName = "A.Author.2001")))
  writeLines("not a zip", file.path(d, "Broken.Author.2003.lpd"))
  n <- norm(d)
  expect_true("<unreadable>" %in% n$field_path)
})

test_that("shadow_report writes the full diff and summarises", {
  a <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", archiveType = "coral")))
  b <- make_db(list(list(dataSetName = "A.Author.2001", datasetId = "ID_A", archiveType = "Coral")))
  p <- withr::local_tempfile(fileext = ".csv")

  d <- shadow_diff(norm(a), norm(b))
  # cli headers go to the message stream; the per-field tables go to stdout.
  expect_output(suppressMessages(shadow_report(d, p)), "archiveType")
  expect_true(file.exists(p))
  expect_equal(nrow(readr::read_csv(p, show_col_types = FALSE)), nrow(d))
  expect_identical(suppressMessages(shadow_report(d)), d)
})

test_that("shadow_snapshot copies a database and refuses to clobber", {
  src <- make_db(list(list(dataSetName = "A.Author.2001")))
  dest <- fs::path(withr::local_tempdir(), "snap")
  shadow_snapshot(src, dest)
  expect_equal(length(fs::dir_ls(fs::path(dest, "db"), glob = "*.lpd")), 1)
  expect_error(shadow_snapshot(src, dest), class = "lv_error_shadow")
})
