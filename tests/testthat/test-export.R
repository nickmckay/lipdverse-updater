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

test_that("the changelog is flattened per dataset and keeps its attribution", {
  # compilation and run_id are the fields that make a cross-compilation
  # overwrite attributable. Exporting the changelog without them would keep the
  # history and lose the only evidence of who changed what.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1", version = "1.0.3")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  L$changelog <- list(
    list(version = "1.0.0", curator = "a"),
    list(version = "1.0.3", lastVersion = "1.0.0", curator = "b",
         compilation = "hydroclimate2k", run_id = "R9",
         changes = list(list(field = "units"), list(field = "proxy"))))
  x <- lv_export_one(L)

  expect_equal(nrow(x$changelog), 2)
  expect_equal(x$changelog$seq, c(1L, 2L))
  last <- x$changelog[x$changelog$version == "1.0.3", ]
  expect_equal(last$compilation, "hydroclimate2k")
  expect_equal(last$run_id, "R9")
  expect_equal(last$n_changes, 2L)
  expect_equal(nrow(lv_export_validate(x)), 0)
})

test_that("a changelog entry with no version is dropped rather than keyed on null", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  L$changelog <- list(list(curator = "a"), list(version = "1.0.1", curator = "b"))
  x <- lv_export_one(L)

  expect_equal(nrow(x$changelog), 1)
  expect_equal(x$changelog$version, "1.0.1")
  expect_equal(nrow(lv_export_validate(x)), 0)
})

test_that("context tables carry the curated state, versions and vocabulary", {
  # An export read years later needs the vocabulary that applied then: checking
  # a value against today's terms answers a different question.
  store <- qc_store(withr::local_tempdir())
  ev <- tibble::tibble(
    run_id = "R1", event_seq = 1:2, ts = "2026-01-01T00:00:00Z",
    compilation = "testcomp", tsid = c("T1", "T2"), dataset_id = "D1",
    field = "paleoData_units", old_value = NA_character_, old_present = FALSE,
    new_value = c("degC", "permil"), new_present = TRUE,
    source = "sheet", actor = "nick", reason = NA_character_)
  qc_store_append(store, "testcomp", ev)

  ctx <- lv_export_context("testcomp", store = store,
                           vocab = list(paleoData_units = tibble::tibble(
                             lipdName = c("degC", "permil"), synonym = c("deg C", "per mil"))))
  expect_equal(nrow(ctx$qc_state), 2)
  expect_setequal(ctx$qc_state$tsid, c("T1", "T2"))
  # present is carried explicitly: absent and blank are different facts.
  expect_true(all(ctx$qc_state$present))
  expect_equal(nrow(ctx$vocab), 2)
  expect_setequal(ctx$vocab$synonym, c("deg C", "per mil"))

  full <- c(lv_export_tables(withr::local_tempdir(), progress = FALSE)[
    setdiff(names(lv_export_schema()$tables), names(ctx))], ctx)
  expect_equal(nrow(lv_export_validate(full)), 0)
})

test_that("an export records what produced it", {
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  write_lpd(d, "B.Author.2002", tsids = "T3")
  cfg <- list(compilation = "testcomp", lipd_dir = d)
  root <- withr::local_tempdir()
  store <- qc_store(withr::local_tempdir())

  man <- lv_export(cfg, "1_0_0", datasets = c("A.Author.2001", "B.Author.2002"),
                   export_dir = root, store = store, duckdb = FALSE,
                   dry_run = FALSE, progress = FALSE)

  # The three fingerprints are the reason the manifest exists: without them an
  # export is a snapshot of an unknown thing.
  for (k in c("db_fingerprint", "vocab_pin", "qc_state_hash", "run_id",
              "compilation", "version")) {
    expect_true(k %in% names(man), info = k)
    expect_false(is.na(as.character(man[[k]])[1]), info = k)
  }
  expect_equal(as.character(man$version), "1_0_0")

  out <- fs::path(root, "testcomp", "1_0_0")
  expect_true(fs::file_exists(fs::path(out, "datasets.parquet")))
  expect_equal(nrow(lv_export_verify(fs::path(out, "export_manifest.json"))), 0)
})

test_that("ensembles are shared across versions, not copied into each", {
  # They are 88.9% of the value rows and change far less often than the metadata
  # around them. Copying them per version is how an export becomes tens of GB.
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  cfg <- list(compilation = "testcomp", lipd_dir = d)
  root <- withr::local_tempdir()
  store <- qc_store(withr::local_tempdir())

  lv_export(cfg, "1_0_0", datasets = "A.Author.2001", export_dir = root,
            store = store, duckdb = FALSE, dry_run = FALSE, progress = FALSE)
  lv_export(cfg, "1_0_1", datasets = "A.Author.2001", export_dir = root,
            store = store, duckdb = FALSE, dry_run = FALSE, progress = FALSE)

  # Two version directories, one ensemble store beside them.
  expect_true(fs::dir_exists(fs::path(root, "testcomp", "1_0_0")))
  expect_true(fs::dir_exists(fs::path(root, "testcomp", "1_0_1")))
  expect_true(fs::dir_exists(fs::path(root, "ensembles")))
  expect_false(fs::file_exists(fs::path(root, "testcomp", "1_0_0", "values_ensemble.parquet")))
})

test_that("a dry run writes nothing but still reports the shape", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  cfg <- list(compilation = "testcomp", lipd_dir = d)
  root <- withr::local_tempdir()
  store <- qc_store(withr::local_tempdir())

  r <- lv_export(cfg, "1_0_0", datasets = "A.Author.2001", export_dir = root,
                 store = store, dry_run = TRUE, progress = FALSE)
  expect_length(fs::dir_ls(root), 0)
  expect_true("tables" %in% names(r))
  expect_true(r$tables[["datasets"]] >= 1)
})

test_that("a compilation with no curated state still gets a state hash", {
  # NA in a manifest cannot be told apart from "never recorded", so a consumer
  # cannot distinguish an export with no curated state from a broken one.
  skip_if_not_installed("arrow")
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  cfg <- list(compilation = "testcomp", lipd_dir = d)
  man <- lv_export(cfg, "1_0_0", datasets = "A.Author.2001",
                   export_dir = withr::local_tempdir(),
                   store = qc_store(withr::local_tempdir()),
                   duckdb = FALSE, dry_run = FALSE, progress = FALSE)
  expect_false(is.na(as.character(man$qc_state_hash)[1]))
  expect_true(nchar(as.character(man$qc_state_hash)[1]) > 8)
})

test_that("the versions table is built when the ledger actually has rows", {
  # The empty-store path worked and the populated one did not: assigning an
  # n-length column into a zero-row tibble is a recycling error, so this only
  # failed on a compilation that had been versioned. Every test using a fresh
  # store missed it.
  store <- qc_store(withr::local_tempdir())
  ver <- lv_tick_version(NULL, before = character(), now = c("D1", "D2"))
  lv_version_append(store, "testcomp", ver, run_id = "R1")

  ctx <- lv_export_context("testcomp", store = store, vocab = list())
  expect_gte(nrow(ctx$versions), 1)
  expect_equal(ctx$versions$compilation[1], "testcomp")
  expect_type(ctx$versions$n_datasets, "integer")
  expect_equal(nrow(lv_export_validate(
    c(stats::setNames(lapply(setdiff(names(lv_export_schema()$tables), names(ctx)),
                             lv_export_empty),
                      setdiff(names(lv_export_schema()$tables), names(ctx))), ctx))), 0)
})

test_that("an ensemble's shape is recorded, so the flat values can be rebuilt", {
  # values_ensemble stores one row per value, which loses the matrix. Without
  # n_rows and n_members a consumer gets 2,413,000 values in a vector and cannot
  # tell 2,413 depths x 1,000 members from any other factorisation -- and a
  # single-column ensemble table has no sibling column to infer it from.
  #
  # Built as a LiPD object rather than through a file, because lipdR's writer
  # does not round-trip a hand-made ensemble table and the shape is lost before
  # the export is even reached.
  L <- list(
    dataSetName = "A.Author.2001", datasetId = "ID1", archiveType = "LakeSediment",
    geo = list(latitude = 40, longitude = -105),
    chronData = list(list(model = list(list(ensembleTable = list(list(
      tableName = "chron1model1ensemble1",
      columns = list(list(TSid = "E1", variableName = "ageEnsemble",
                          units = "yr BP",
                          values = matrix(seq_len(5 * 3), nrow = 5, ncol = 3)))
    )))))))

  one <- lv_export_one(L)
  row <- one$timeseries[one$timeseries$TSid == "E1", ]
  expect_equal(row$n_rows, 5L)
  expect_equal(row$n_members, 3L)
  expect_equal(row$n_values, 15L)

  # And the recorded shape decomposes row_index the way the schema says: column
  # major, so member m is row_index ((m-1)*n_rows+1) to (m*n_rows).
  e <- one$values_ensemble[one$values_ensemble$TSid == "E1", ]
  expect_equal(nrow(e), 15L)
  expect_equal(e$value_num[e$row_index > 5 & e$row_index <= 10], as.double(6:10))
})

test_that("a plain column records no ensemble shape", {
  L <- list(
    dataSetName = "A.Author.2001", datasetId = "ID1", archiveType = "LakeSediment",
    geo = list(latitude = 40, longitude = -105),
    paleoData = list(list(measurementTable = list(list(
      tableName = "paleo1measurement1",
      columns = list(list(TSid = "T1", variableName = "temperature",
                          units = "degC", values = list(1, 2, 3))))))))
  one <- lv_export_one(L)
  row <- one$timeseries[one$timeseries$TSid == "T1", ]
  expect_true(is.na(row$n_rows))
  expect_true(is.na(row$n_members))
  expect_equal(row$n_values, 3L)
})

test_that("an accented dataset name is exported, not silently dropped", {
  # The fifth site of the NFD/NFC defect: a filename is decomposed, a name from
  # the sheet or the index is composed, and the raw comparison drops the dataset
  # from the artefact everything downstream reads.
  nfc <- "CentralEurope.Büntgen.2011"
  d <- withr::local_tempdir()
  write_lpd(d, stringi::stri_trans_nfd(nfc), tsids = c("T1", "T2"))
  tabs <- lv_export_tables(d, datasets = nfc, progress = FALSE)
  expect_equal(nrow(tabs$datasets), 1)
  expect_equal(nrow(tabs$timeseries), 2)
})

test_that("the database export reaches datasets no compilation holds", {
  # 221 of 7,293 datasets belong to no compilation, so a site assembled only
  # from per-compilation exports would silently omit them.
  d <- withr::local_tempdir()
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  write_lpd(d, "InComp.Author.2001", tsids = c("T1", "T2"),
            col_extra = list(inCompilation = list(list(compilationName = "someComp",
                                                       compilationVersion = list("1_0_0")))))
  write_lpd(d, "Orphan.Author.2002", tsids = c("T3", "T4"))

  out <- withr::local_tempdir()
  man <- lv_export_database("test-label", dir = d, export_dir = out,
                            store = qc_store(file.path(out, "store")),
                            duckdb = FALSE, dry_run = FALSE, progress = FALSE)
  tabs <- arrow::read_parquet(fs::path(out, "_database", "test-label", "datasets.parquet"))
  expect_setequal(tabs$dataSetName, c("InComp.Author.2001", "Orphan.Author.2002"))
  # The orphan is present in datasets and absent from compilations, which is
  # exactly the gap this exists to close.
  comps <- arrow::read_parquet(fs::path(out, "_database", "test-label", "compilations.parquet"))
  expect_true("someComp" %in% comps$compilation)
  expect_false(any(comps$datasetId %in% tabs$datasetId[tabs$dataSetName == "Orphan.Author.2002"]))
})

test_that("the database export leaves qc_state empty, on purpose", {
  # The store is keyed by compilation and the table's key is [tsid, field], with
  # no room for two compilations holding different values for one cell. Unioning
  # them would collide silently, so this one is deliberately not attempted.
  d <- withr::local_tempdir()
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  out <- withr::local_tempdir()
  st <- qc_store(file.path(out, "store"))
  qc_store_append(st, "someComp", qc_diff_to_events(
    qc_cells_empty(),
    tibble::tibble(tsid = "T1", field = "paleoData_units", value = "permil",
                   present = TRUE, dataset_id = "IDA.Author.2001")))

  man <- lv_export_database("test-label", dir = d, export_dir = out, store = st,
                            duckdb = FALSE, dry_run = FALSE, progress = FALSE)
  qc <- arrow::read_parquet(fs::path(out, "_database", "test-label", "qc_state.parquet"))
  expect_equal(nrow(qc), 0)
  # But every compilation's version ledger does come through.
  expect_true(fs::file_exists(fs::path(out, "_database", "test-label", "versions.parquet")))
})
