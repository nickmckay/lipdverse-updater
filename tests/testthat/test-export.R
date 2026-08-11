test_that("a text column survives export instead of becoming NaN", {
  # The reason values.parquet is long. A wide table forces one type per row
  # across every column, which is how lipdR turned string and logical chron
  # columns into NaN on write. Splitting into value_num and value_chr makes the
  # coercion impossible to express, so this is the defining test for the table.
  s <- lv_split_values(list("reject", "accept", "reject"))
  expect_true(all(is.na(s$num)))
  expect_equal(s$chr, c("reject", "accept", "reject"))

  n <- lv_split_values(list(1, 2.5, -3))
  expect_equal(n$num, c(1, 2.5, -3))
  expect_true(all(is.na(n$chr)))
})

test_that("absent values are absent in both columns, not text", {
  # "NA" arriving as a string must not become the literal word in value_chr,
  # or every consumer has to know to filter it back out.
  s <- lv_split_values(list("NA", "", NA, "NaN"))
  expect_true(all(is.na(s$num)))
  expect_true(all(is.na(s$chr)))
})

test_that("a mixed column keeps the numbers as numbers and the text as text", {
  s <- lv_split_values(list("1.5", "below detection", "2.5"))
  expect_equal(s$num, c(1.5, NA, 2.5))
  expect_equal(s$chr, c(NA, "below detection", NA))
})

test_that("export of one dataset matches the contract", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  x <- lv_export_one(L, file_md5 = "abc")

  expect_equal(nrow(x$datasets), 1)
  expect_equal(x$datasets$dataSetName, "A.Author.2001")
  expect_equal(x$datasets$file_md5, "abc")
  expect_setequal(x$timeseries$TSid, c("T1", "T2"))
  expect_equal(x$datasets$n_timeseries, 2L)
  # Three values per column, long.
  expect_equal(nrow(x$values), 6)
  expect_setequal(x$values$row_index, 1:3)

  expect_equal(nrow(lv_export_validate(x)), 0)
})

test_that("the validator rejects a table that breaks the contract", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  x <- lv_export_one(L)

  # Wrong type where the contract says double.
  bad <- x; bad$values$value_num <- as.character(bad$values$value_num)
  iss <- lv_export_validate(bad)
  expect_true(any(iss$check == "export_wrong_type"))

  # A duplicated key must not pass: the site generator joins on it.
  dup <- x; dup$values <- dplyr::bind_rows(dup$values, dup$values[1, ])
  expect_true(any(lv_export_validate(dup)$check == "export_duplicate_key"))

  # A required column holding NA is an error, not a warning.
  na <- x; na$timeseries$TSid[1] <- NA_character_
  expect_true(any(lv_export_validate(na)$check == "export_required_na"))

  # And a missing table is caught rather than silently exported short.
  expect_true(any(lv_export_validate(x[setdiff(names(x), "values")])$check ==
                    "export_missing_table"))
})

test_that("axis columns are marked rather than dropped", {
  # The site generator needs the age axis; the QC sheet does not. Exporting it
  # with a flag serves both, where filtering it out here would not.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  # readLipd stores each column as a named entry of the table, not under a
  # `columns` key, so reach for it the way lv_cols_of() does.
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$variableName <- "year"
  x <- lv_export_one(L)

  expect_true(any(x$timeseries$isAxis))
  expect_false(all(x$timeseries$isAxis))
  expect_equal(nrow(x$timeseries), 2)
})

test_that("interpretations are numbered within their scope", {
  # environmentInterpretation1 is the first environment one, not the first entry
  # in the list. Numbering by position produces ranks that match nothing.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$interpretation <- list(
    list(scope = "climate", variable = "T"),
    list(scope = "environment", variable = "E"),
    list(scope = "climate", variable = "P"))
  x <- lv_export_one(L)

  cl <- x$interpretations[x$interpretations$scope == "climate", ]
  expect_equal(cl$rank, c(1L, 2L))
  expect_equal(cl$variable, c("T", "P"))
  expect_equal(x$interpretations$rank[x$interpretations$scope == "environment"], 1L)
})

test_that("authors stay a list rather than a joined string", {
  # Joining them means every consumer has to guess the separator back, and
  # author names contain commas.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  x <- lv_export_one(L)

  expect_true(is.list(x$publications$authors))
  expect_equal(nrow(x$publications), 1)
  expect_equal(lv_export_validate(x) |> nrow(), 0)
})

test_that("the export round-trips through parquet with types intact", {
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  write_lpd(d, "B.Author.2002", tsids = "T3")

  tbl <- lv_export_tables(d, progress = FALSE)
  out <- withr::local_tempdir()
  man <- lv_export_write(tbl, out, meta = list(compilation = "test", version = "1_0_0"))

  expect_true(fs::file_exists(fs::path(out, "export_manifest.json")))
  back <- arrow::read_parquet(fs::path(out, "values.parquet"))
  expect_equal(nrow(back), nrow(tbl$values))
  expect_type(back$value_num, "double")
  expect_type(back$value_chr, "character")
  # The list column must survive as a list, not a joined string.
  pubs <- arrow::read_parquet(fs::path(out, "publications.parquet"))
  expect_true(is.list(pubs$authors))
  # And the written tables still satisfy the contract after the round trip.
  local_tables <- setdiff(names(tbl), "values_ensemble")   # external, a directory
  rt <- stats::setNames(lapply(local_tables, function(n)
    tibble::as_tibble(arrow::read_parquet(fs::path(out, paste0(n, ".parquet"))))), local_tables)
  rt$values_ensemble <- tbl$values_ensemble
  expect_equal(nrow(lv_export_validate(rt)), 0)
})

test_that("the manifest detects a file changed after it was written", {
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  out <- withr::local_tempdir()
  lv_export_write(lv_export_tables(d, progress = FALSE), out)

  expect_equal(nrow(lv_export_verify(fs::path(out, "export_manifest.json"))), 0)
  # An export the consumer cannot trust must not look fine.
  arrow::write_parquet(data.frame(x = 1), fs::path(out, "values.parquet"))
  iss <- lv_export_verify(fs::path(out, "export_manifest.json"))
  expect_true(any(iss$check == "export_hash_mismatch"))
})

test_that("the duckdb build reproduces the parquet files and their joins", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  write_lpd(d, "B.Author.2002", tsids = "T3")
  out <- withr::local_tempdir()
  tbl <- lv_export_tables(d, progress = FALSE)
  lv_export_write(tbl, out)

  db <- lv_export_duckdb(out)
  expect_true(fs::file_exists(db))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Every contract table is reachable, `values` included -- it is a reserved SQL
  # keyword, so it is the one most likely to be silently missing. Ensembles are
  # a view over the shared store rather than a copied table.
  expect_true(all(setdiff(names(tbl), "values_ensemble") %in% DBI::dbListTables(con)))
  n <- DBI::dbGetQuery(con, 'SELECT count(*) AS n FROM "values"')$n
  expect_equal(n, nrow(tbl$values))

  # The join view is the reason the database exists.
  v <- DBI::dbGetQuery(con, "SELECT TSid, dataSetName FROM v_timeseries_full ORDER BY TSid")
  expect_equal(nrow(v), nrow(tbl$timeseries))
  expect_false(anyNA(v$dataSetName))

  # And the axis filter is applied for the consumer rather than by them.
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM v_measurements")$n,
               sum(tbl$timeseries$tableKind == "measurement" & !tbl$timeseries$isAxis))
})

test_that("an incomplete export is refused rather than half-built", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  out <- withr::local_tempdir()
  lv_export_write(lv_export_tables(d, progress = FALSE), out)
  fs::file_delete(fs::path(out, "interpretations.parquet"))

  expect_error(lv_export_duckdb(out), class = "lv_error_export")
  # Nothing left behind for the next run to trip over.
  expect_length(fs::dir_ls(out, glob = "*.building"), 0)
})

test_that("an interpretation with no scope is labelled, not left NA", {
  # scope is part of the key. A null there does not join and does not dedupe,
  # so an unscoped interpretation would silently vanish from any query that
  # went through it. Real datasets have them.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$interpretation <- list(
    list(variable = "P"), list(scope = "climate", variable = "T"))
  x <- lv_export_one(L)

  expect_false(anyNA(x$interpretations$scope))
  expect_true("unscoped" %in% x$interpretations$scope)
  expect_equal(nrow(lv_export_validate(x)), 0)
})

test_that("ensembles are split out of values", {
  # Measured over the whole database, ensembles are 88.9% of 437M value rows and
  # cost 8.82 bytes each against 3.93 for measurements. Together they are ~3.6 GB;
  # apart, the table nearly everyone reads is ~189 MB.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  # An ensemble table of its own, with its own TSid: reusing the measurement
  # table would put one TSid in two tables, which the key check rejects.
  L$chronData <- list(list(model = list(list(ensembleTable = list(
    make_ensemble_table("E1"))))))
  x <- lv_export_one(L)

  expect_true(all(x$values_ensemble$TSid == "E1"))
  expect_gt(nrow(x$values_ensemble), 0)
  # The ensemble rows are not also in values.
  expect_equal(nrow(x$values), 3)
  # And the partition key travels with them, since a reader needs it before
  # opening anything.
  expect_true("datasetId" %in% names(x$values_ensemble))
  expect_false(anyNA(x$values_ensemble$datasetId))
  expect_equal(nrow(lv_export_validate(x)), 0)
})

test_that("the ensemble store is written once and referenced, not copied", {
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  L$chronData <- list(list(model = list(list(
    ensembleTable = list(make_ensemble_table("E1"))))))
  x <- lv_export_one(L)

  shared <- withr::local_tempdir()
  out <- withr::local_tempdir()
  man <- lv_export_write(x, out, ensemble_dir = shared)

  # Not a parquet file in the compilation directory.
  expect_false(fs::file_exists(fs::path(out, "values_ensemble.parquet")))
  # Hive layout in the shared store, so a reader can prune by dataset.
  parts <- fs::dir_ls(shared, glob = "*.parquet", recurse = TRUE)
  expect_gt(length(parts), 0)
  expect_true(any(grepl("datasetId=", fs::path_dir(parts))))
  # The manifest points at it and records enough to check it later.
  expect_equal(as.character(man$external[[1]]$name), "values_ensemble")
  expect_equal(nrow(lv_export_verify(fs::path(out, "export_manifest.json"))), 0)
})

test_that("verification notices the shared ensemble store changing underneath", {
  # The failure mode the split introduces: an export is no longer self-contained,
  # so the store can move on without the compilation directory knowing.
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  L$chronData <- list(list(model = list(list(
    ensembleTable = list(make_ensemble_table("E1"))))))
  shared <- withr::local_tempdir(); out <- withr::local_tempdir()
  lv_export_write(lv_export_one(L), out, ensemble_dir = shared)

  parts <- fs::dir_ls(shared, glob = "*.parquet", recurse = TRUE)
  arrow::write_parquet(data.frame(x = 1), parts[1])
  iss <- lv_export_verify(fs::path(out, "export_manifest.json"))
  expect_true(any(iss$check == "export_hash_mismatch"))

  fs::file_delete(parts[1])
  iss2 <- lv_export_verify(fs::path(out, "export_manifest.json"))
  expect_true(any(iss2$check == "export_external_file_count"))
})
