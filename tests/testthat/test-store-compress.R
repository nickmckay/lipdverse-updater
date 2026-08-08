ev <- function(n, tsid, val, seq_from = 1) {
  tibble::tibble(tsid = tsid, field = "f", old_value = NA_character_, old_present = FALSE,
                 new_value = val, new_present = TRUE, dataset_id = "d",
                 source = "migration", actor = "t", reason = "seed")
}

test_that("compressing does not change what the events mean", {
  st <- qc_store(withr::local_tempdir())
  qc_store_append(st, "x", ev(1, c("a", "b"), c("1", "2")), run_id = "r1")
  qc_store_append(st, "x", ev(1, "a", "3"), run_id = "r2")
  before <- qc_state_current(st, "x")

  d <- fs::path(st$path, "compilations", "x", "events")
  # Force the pre-gzip layout, which is what the migration has to handle.
  for (f in fs::dir_ls(d, regexp = "gz$")) {
    x <- readr::read_csv(f, col_types = readr::cols(.default = "c"), na = "", progress = FALSE)
    readr::write_csv(x, sub("[.]gz$", "", f), na = "")
    fs::file_delete(f)
  }
  expect_length(fs::dir_ls(d, regexp = "[.]csv$"), 2)

  qc_store_compress(st, "x", dry_run = FALSE)
  expect_length(fs::dir_ls(d, regexp = "[.]csv$"), 0)
  expect_length(fs::dir_ls(d, regexp = "[.]csv[.]gz$"), 2)
  expect_equal(qc_state_current(st, "x"), before)
})

test_that("a mixed directory reads in the right order", {
  st <- qc_store(withr::local_tempdir())
  qc_store_append(st, "x", ev(1, "a", "first"), run_id = "r1")
  d <- fs::path(st$path, "compilations", "x", "events")
  # Leave the first as plain .csv, so the second append meets a mixed directory.
  f <- fs::dir_ls(d, regexp = "gz$")
  x <- readr::read_csv(f, col_types = readr::cols(.default = "c"), na = "", progress = FALSE)
  readr::write_csv(x, sub("[.]gz$", "", f), na = ""); fs::file_delete(f)

  qc_store_append(st, "x", ev(1, "a", "second"), run_id = "r2")
  expect_length(fs::dir_ls(d), 2)
  # Latest wins, and the gzipped second append is the later one.
  s <- qc_state_current(st, "x")
  expect_equal(s$value[s$tsid == "a"], "second")
})

test_that("appends are gzipped and keep their sequence", {
  st <- qc_store(withr::local_tempdir())
  for (i in 1:3) qc_store_append(st, "x", ev(1, "a", as.character(i)), run_id = paste0("r", i))
  d <- fs::path(st$path, "compilations", "x", "events")
  f <- fs::dir_ls(d)
  expect_true(all(grepl("[.]csv[.]gz$", f)))
  expect_equal(substr(basename(f), 1, 6), c("000001", "000002", "000003"))
  s <- qc_state_current(st, "x")
  expect_equal(s$value[s$tsid == "a"], "3")
})

test_that("compression actually saves space on a realistic log", {
  st <- qc_store(withr::local_tempdir())
  big <- ev(1, paste0("T", 1:5000), rep("some repeated citation text", 5000))
  qc_store_append(st, "x", big, run_id = "r1")
  d <- fs::path(st$path, "compilations", "x", "events")
  gz <- fs::file_size(fs::dir_ls(d, regexp = "gz$"))
  x <- readr::read_csv(fs::dir_ls(d, regexp = "gz$"), col_types = readr::cols(.default = "c"),
                       na = "", progress = FALSE)
  plain_path <- fs::path(withr::local_tempdir(), "p.csv")
  readr::write_csv(x, plain_path, na = "")
  expect_lt(as.numeric(gz), as.numeric(fs::file_size(plain_path)) / 5)
})
