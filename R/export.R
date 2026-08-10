#' The canonical export
#'
#' Writes the tables the site generator consumes, and nothing it does not. The
#' shape is frozen in `inst/extdata/export_schema.yml` and asserted by tests,
#' because the consumer lives in another repository and cannot be fixed in the
#' same commit as a change here.
#'
#' The work is split in two on purpose. [lv_export_tables()] turns LiPD into a
#' named list of tibbles and depends on nothing but the files, so the shaping
#' logic - which is where the judgement lives - is testable without arrow.
#' [lv_export_write()] is a thin writer over the result.
#'
#' @name export
NULL

#' @rdname export
#' @param path Schema file; defaults to the packaged contract.
#' @return The parsed schema.
#' @export
lv_export_schema <- function(path = NULL) {
  path <- path %||% system.file("extdata", "export_schema.yml", package = "lipdverseUpdater")
  if (!nzchar(path) || !fs::file_exists(path)) {
    # Under devtools::load_all() the installed path does not exist.
    alt <- fs::path(lv_pkg_root(), "inst", "extdata", "export_schema.yml")
    if (fs::file_exists(alt)) path <- alt else
      cli::cli_abort("Export schema not found.", class = "lv_error_export")
  }
  yaml::read_yaml(path)
}

# Whether a bare future worker could attach this package. Not the same question
# as whether it is loaded: devtools::load_all() loads it without installing it.
lv_pkg_installed <- function() {
  length(find.package("lipdverseUpdater", lib.loc = .libPaths(), quiet = TRUE)) > 0
}

lv_pkg_root <- function() {
  p <- tryCatch(system.file(package = "lipdverseUpdater"), error = function(e) "")
  if (nzchar(p) && fs::file_exists(fs::path(p, "DESCRIPTION"))) return(p)
  path.expand("~/GitHub/lipdverse-updater")
}

# Columns whose job is to carry the axis rather than a measurement.
lv_is_axis <- function(x) tolower(trimws(x %||% "")) %in% LV_AXIS_QC_VARIABLES

# A value is numeric or it is text, never both, and never coerced from one to
# the other. Anything that does not parse as a number stays as text with
# value_num NA, which is what keeps a logical or string column from becoming
# NaN on the way through a numeric matrix.
lv_split_values <- function(v) {
  chr <- as.character(unlist(v))
  num <- suppressWarnings(as.numeric(chr))
  # "NA" and blank are absent, not text.
  blank <- is.na(chr) | !nzchar(trimws(chr)) | trimws(chr) %in% c("NA", "NaN")
  list(num = ifelse(blank, NA_real_, num),
       chr = ifelse(blank | !is.na(num), NA_character_, chr))
}

lv_cols_of <- function(tb) {
  if (!is.null(tb$columns)) return(tb$columns)
  tb[!names(tb) %in% c("filename", "tableName", "missingValue", "googWorkSheetKey")]
}

# Every table in a file, tagged with where it came from, since tableType and
# tableKind are columns in the export rather than something a consumer should
# have to infer from a name.
lv_tables_of <- function(L) {
  out <- list()
  for (blk in c("paleoData", "chronData")) {
    tt <- sub("Data$", "", blk)
    for (pd in L[[blk]]) {
      for (tb in pd$measurementTable)
        out[[length(out) + 1L]] <- list(tb = tb, type = tt, kind = "measurement")
      for (md in pd$model) {
        for (k in c("summaryTable", "ensembleTable", "distributionTable")) {
          for (tb in md[[k]])
            out[[length(out) + 1L]] <- list(tb = tb, type = tt,
                                            kind = sub("Table$", "", k))
        }
      }
    }
  }
  out
}

#' @rdname export
#' @param L One LiPD object, as returned by `lipdR::readLipd()`.
#' @param file_md5 Recorded on the dataset row so an export can be traced to the
#'   exact file it came from.
#' @return A named list of tibbles for the single dataset.
#' @export
lv_export_one <- function(L, file_md5 = NA_character_) {
  dsid <- as_chr1(L$datasetId) %||% NA_character_
  dsn  <- as_chr1(L$dataSetName) %||% NA_character_
  s1 <- function(x) { v <- as_chr1(x); if (is.null(v)) NA_character_ else v }
  n1 <- function(x) { v <- suppressWarnings(as.numeric(as_chr1(x))); if (length(v) != 1) NA_real_ else v }

  geo <- lv_geo_of_object(L)
  tabs <- lv_tables_of(L)

  ts <- list(); vals <- list(); interp <- list(); comp <- list()
  has_chron_ens <- FALSE; has_paleo_ens <- FALSE; has_chron <- FALSE

  for (t in tabs) {
    if (identical(t$type, "chron")) has_chron <- TRUE
    if (identical(t$kind, "ensemble")) {
      if (identical(t$type, "chron")) has_chron_ens <- TRUE else has_paleo_ens <- TRUE
    }
    for (cl in lv_cols_of(t$tb)) {
      if (!is.list(cl) || is.null(cl$TSid)) next
      tsid <- as_chr1(cl$TSid)
      if (is.null(tsid) || !nzchar(tsid)) next
      vn <- s1(cl$variableName)
      sv <- lv_split_values(cl$values)
      n <- length(sv$num)

      ts[[length(ts) + 1L]] <- tibble::tibble(
        TSid = tsid, datasetId = dsid, tableType = t$type, tableKind = t$kind,
        variableName = vn, standardName = s1(cl$standardVariableName),
        units = s1(cl$units), proxy = s1(cl$proxy),
        proxyGeneral = s1(cl$proxyGeneral), description = s1(cl$description),
        primaryTimeseries = isTRUE(as.logical(as_chr1(cl$primaryTimeseries))),
        isAxis = lv_is_axis(vn), createdBy = s1(cl$createdBy),
        minYear = NA_real_, maxYear = NA_real_,
        medianResolution = NA_real_, n_values = as.integer(n))

      if (n) vals[[length(vals) + 1L]] <- tibble::tibble(
        TSid = tsid, row_index = seq_len(n), value_num = sv$num, value_chr = sv$chr)

      # Interpretations are numbered within their scope, not by list position:
      # environmentInterpretation1 is the first environment one. Numbering them
      # positionally produces names that match nothing downstream.
      if (!is.null(cl$interpretation)) {
        seen <- integer()
        for (it in cl$interpretation) {
          if (!is.list(it)) next
          sc <- tolower(s1(it$scope) %||% "")
          # Unscoped interpretations exist in the database, and scope is part of
          # the key. NA in a key is worse than a label: SQL joins do not match on
          # NULL, and the duplicate check cannot see them. So they get a name.
          if (is.na(sc) || !nzchar(sc)) sc <- "unscoped"
          k <- if (sc %in% names(seen)) seen[[sc]] + 1L else 1L
          seen[[sc]] <- k
          interp[[length(interp) + 1L]] <- tibble::tibble(
            TSid = tsid, scope = sc, rank = k,
            variable = s1(it$variable), variableDetail = s1(it$variableDetail),
            direction = s1(it$direction), seasonality = s1(it$seasonality),
            basis = s1(it$basis),
            isAnnual = {
              a <- s1(it$isAnnual); if (is.na(a)) NA else isTRUE(as.logical(a))
            })
        }
      }

      for (nm in grep("^inCompilation", names(cl), value = TRUE)) {
        for (ic in cl[[nm]]) {
          if (!is.list(ic)) next
          cn <- s1(ic$compilationName)
          if (is.na(cn)) next
          comp[[length(comp) + 1L]] <- tibble::tibble(
            datasetId = dsid, TSid = tsid, compilation = cn,
            compilationVersion = s1(ic$compilationVersion),
            inThisCompilation = {
              v <- s1(ic$inThisCompilation); if (is.na(v)) TRUE else isTRUE(as.logical(v))
            })
        }
      }
    }
  }

  ts_t <- if (length(ts)) dplyr::bind_rows(ts) else lv_export_empty("timeseries")
  # Year range per timeseries comes from the axis of its own table, so it is
  # derived after the columns are collected rather than inside the loop.
  ts_t <- lv_add_year_range(ts_t, tabs)

  pubs <- list()
  for (i in seq_along(L$pub)) {
    p <- L$pub[[i]]
    au <- p$author %||% p$authors
    aus <- if (is.list(au)) vapply(au, function(a)
      s1(if (is.list(a)) a$name else a), character(1)) else as.character(unlist(au))
    aus <- aus[!is.na(aus) & nzchar(aus)]
    pubs[[length(pubs) + 1L]] <- tibble::tibble(
      datasetId = dsid, pubIndex = i, pubKind = s1(p$pubDataType %||% p$type),
      authors = list(aus), year = as.integer(n1(p$year)), title = s1(p$title),
      journal = s1(p$journal), doi = s1(p$doi %||% p$DOI), citeKey = s1(p$citeKey))
  }

  list(
    datasets = tibble::tibble(
      datasetId = dsid, dataSetName = dsn, archiveType = s1(L$archiveType),
      version = s1(L$dataSetVersion %||% L$version),
      geo_latitude = geo$lat, geo_longitude = geo$lon, geo_elevation = geo$elev,
      geo_siteName = geo$site,
      minYear = suppressWarnings(min(ts_t$minYear, na.rm = TRUE)) |> lv_finite(),
      maxYear = suppressWarnings(max(ts_t$maxYear, na.rm = TRUE)) |> lv_finite(),
      hasChron = has_chron, hasChronEnsemble = has_chron_ens,
      hasPaleoEnsemble = has_paleo_ens,
      n_timeseries = nrow(ts_t), file_md5 = file_md5),
    timeseries = ts_t,
    values = if (length(vals)) dplyr::bind_rows(vals) else lv_export_empty("values"),
    interpretations = if (length(interp)) dplyr::bind_rows(interp) else lv_export_empty("interpretations"),
    publications = if (length(pubs)) dplyr::bind_rows(pubs) else lv_export_empty("publications"),
    compilations = if (length(comp)) dplyr::bind_rows(comp) else lv_export_empty("compilations"))
}

lv_finite <- function(x) if (length(x) != 1 || !is.finite(x)) NA_real_ else x

# minYear/maxYear describe the span a timeseries covers, which is a property of
# its table's axis rather than of the column itself.
lv_add_year_range <- function(ts_t, tabs) {
  if (!nrow(ts_t)) return(ts_t)
  for (t in tabs) {
    cols <- lv_cols_of(t$tb)
    ax <- NULL
    for (cl in cols) {
      if (!is.list(cl) || is.null(cl$values)) next
      vn <- as_chr1(cl$variableName) %||% ""
      if (tolower(trimws(vn)) %in% c("year", "yearad", "year ad")) {
        ax <- suppressWarnings(as.numeric(unlist(cl$values))); break
      }
    }
    if (is.null(ax)) next
    ax <- ax[is.finite(ax)]
    if (!length(ax)) next
    ids <- vapply(Filter(function(c) is.list(c) && !is.null(c$TSid), cols),
                  function(c) as.character(c$TSid), character(1))
    hit <- ts_t$TSid %in% ids
    ts_t$minYear[hit] <- min(ax); ts_t$maxYear[hit] <- max(ax)
    if (length(ax) > 1) ts_t$medianResolution[hit] <- stats::median(abs(diff(sort(ax))))
  }
  ts_t
}

lv_geo_of_object <- function(L) {
  g <- L$geo
  if (!is.list(g)) g <- list()
  num <- function(x) { v <- suppressWarnings(as.numeric(as_chr1(x))); if (length(v) != 1) NA_real_ else v }
  site <- as_chr1(g$siteName) %||% NA_character_
  # Two shapes in the wild: flat latitude/longitude, and GeoJSON where the
  # coordinates are an array. In GeoJSON the order is [longitude, latitude,
  # elevation] -- reading it the other way round swaps every coordinate in the
  # database without erroring anywhere.
  geom <- g$geometry
  if (is.list(geom) && !is.null(geom$coordinates)) {
    co <- geom$coordinates
    if (is.list(co) || is.numeric(co)) {
      return(list(lon = if (length(co) >= 1) num(co[[1]]) else NA_real_,
                  lat = if (length(co) >= 2) num(co[[2]]) else NA_real_,
                  elev = if (length(co) >= 3) num(co[[3]]) else NA_real_,
                  site = site))
    }
  }
  list(lat = num(g$latitude), lon = num(g$longitude), elev = num(g$elevation),
       site = site)
}

#' @rdname export
#' @param name Table name from the schema.
#' @export
lv_export_empty <- function(name, schema = lv_export_schema()) {
  spec <- schema$tables[[name]]
  if (is.null(spec)) cli::cli_abort("Unknown export table {.val {name}}.", class = "lv_error_export")
  proto <- list(chr = character(), dbl = double(), int = integer(),
                lgl = logical(), list_chr = list())
  tibble::as_tibble(stats::setNames(
    lapply(spec$columns, function(c) proto[[c$type]]), names(spec$columns)))
}

#' @rdname export
#' @param tables Named list of tibbles.
#' @param schema Parsed schema.
#' @return An `lv_issues` tibble; empty when the tables match the contract.
#' @export
lv_export_validate <- function(tables, schema = lv_export_schema()) {
  iss <- lv_issues_empty()
  for (nm in names(schema$tables)) {
    spec <- schema$tables[[nm]]
    if (!nm %in% names(tables)) {
      iss <- lv_issues_bind(iss, lv_issues(check = "export_missing_table",
        severity = "error", message = sprintf("Table '%s' is missing.", nm)))
      next
    }
    x <- tables[[nm]]
    missing <- setdiff(names(spec$columns), names(x))
    if (length(missing)) iss <- lv_issues_bind(iss, lv_issues(
      check = "export_missing_column", severity = "error",
      message = sprintf("Table '%s' is missing column%s: %s", nm,
                        if (length(missing) > 1) "s" else "", paste(missing, collapse = ", "))))
    extra <- setdiff(names(x), names(spec$columns))
    if (length(extra)) iss <- lv_issues_bind(iss, lv_issues(
      check = "export_extra_column", severity = "warn",
      message = sprintf("Table '%s' has undeclared column%s: %s", nm,
                        if (length(extra) > 1) "s" else "", paste(extra, collapse = ", "))))
    for (cn in intersect(names(spec$columns), names(x))) {
      want <- spec$columns[[cn]]$type
      got <- if (is.list(x[[cn]]) && !inherits(x[[cn]], "POSIXct")) "list_chr" else
        switch(class(x[[cn]])[1], character = "chr", numeric = "dbl", double = "dbl",
               integer = "int", logical = "lgl", class(x[[cn]])[1])
      if (!identical(want, got)) iss <- lv_issues_bind(iss, lv_issues(
        check = "export_wrong_type", severity = "error", field = cn,
        message = sprintf("Table '%s' column '%s' is %s, contract says %s.", nm, cn, got, want)))
      if (isTRUE(spec$columns[[cn]]$required) && nrow(x) && anyNA(x[[cn]]))
        iss <- lv_issues_bind(iss, lv_issues(check = "export_required_na",
          severity = "error", field = cn,
          message = sprintf("Table '%s' column '%s' is required but holds NA.", nm, cn)))
    }
    if (!is.null(spec$key) && nrow(x) && all(spec$key %in% names(x))) {
      k <- do.call(paste, c(unname(as.list(x[, spec$key])), sep = "\r"))
      if (anyDuplicated(k)) iss <- lv_issues_bind(iss, lv_issues(
        check = "export_duplicate_key", severity = "error",
        message = sprintf("Table '%s' has %d duplicate key%s on (%s).", nm,
                          sum(duplicated(k)), if (sum(duplicated(k)) > 1) "s" else "",
                          paste(spec$key, collapse = ", "))))
    }
  }
  iss
}

#' @rdname export
#' @param dir Database directory, or an `lv_scan`.
#' @param datasets Restrict to these dataSetNames.
#' @param progress Show a progress message.
#' @return A named list of tibbles covering every dataset read.
#' @export
lv_export_tables <- function(dir = lv_path("database"), datasets = NULL,
                             progress = TRUE, parallel = NA) {
  paths <- if (inherits(dir, "lv_scan")) dir$files$path else
    fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (!is.null(datasets))
    paths <- paths[sub("\\.lpd$", "", fs::path_file(paths)) %in% datasets]
  nms <- names(lv_export_schema()$tables)
  if (!length(paths)) return(stats::setNames(lapply(nms, lv_export_empty), nms))

  if (progress) cli::cli_alert_info("Exporting {length(paths)} file{?s}")
  one <- function(p) {
    L <- tryCatch(suppressWarnings(lipdR::readLipd(p)), error = function(e) NULL)
    if (is.null(L)) return(NULL)
    tryCatch(lv_export_one(L, file_md5 = unname(tools::md5sum(p))),
             error = function(e) NULL)
  }
  # The worker needs this package, and a future worker can only attach what is
  # actually installed -- under devtools::load_all() there is nothing to attach,
  # so parallelising there fails with a misleading "no package called" error.
  # Decide from the library rather than from whether the package is loaded.
  if (is.na(parallel)) parallel <- lv_pkg_installed()
  parts <- if (parallel) {
    furrr::future_map(unname(paths), one, .options = furrr::furrr_options(
      seed = TRUE, packages = c("lipdR", "lipdverseUpdater")))
  } else {
    purrr::map(unname(paths), one)
  }
  ok <- !vapply(parts, is.null, logical(1))
  if (any(!ok)) cli::cli_alert_warning("{sum(!ok)} file{?s} could not be exported.")
  parts <- parts[ok]
  out <- stats::setNames(lapply(nms, function(n)
    dplyr::bind_rows(lapply(parts, `[[`, n))), nms)
  out
}

#' @rdname export
#' @param tables From [lv_export_tables()].
#' @param path Output directory; created if absent.
#' @param meta Named list folded into the manifest (compilation, version, run_id, ...).
#' @param strict Abort on a contract violation rather than warn.
#' @return The manifest, invisibly.
#' @export
lv_export_write <- function(tables, path, meta = list(), strict = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE))
    cli::cli_abort("The {.pkg arrow} package is required to write the export.",
                   class = "lv_error_export")
  iss <- lv_export_validate(tables)
  if (lv_n_issues(iss, "error") > 0) {
    p <- fs::path(lv_run_dir(meta$run_id %||% lv_run_id()), "export-issues.csv")
    if (strict) lv_issues_check(iss, p, what = "Export validation")
    cli::cli_alert_warning("Export has {lv_n_issues(iss, 'error')} contract violation{?s}.")
  }
  fs::dir_create(path)

  files <- list()
  for (nm in names(tables)) {
    f <- fs::path(path, paste0(nm, ".parquet"))
    arrow::write_parquet(tables[[nm]], f)
    files[[length(files) + 1L]] <- list(
      name = jsonlite::unbox(paste0(nm, ".parquet")),
      n_rows = jsonlite::unbox(nrow(tables[[nm]])),
      bytes = jsonlite::unbox(as.numeric(fs::file_size(f))),
      sha256 = jsonlite::unbox(digest::digest(file = f, algo = "sha256")))
  }

  # The manifest is what makes an export reproducible after the fact: it records
  # which database, which vocabulary and which code produced it.
  man <- c(
    list(schema_version = jsonlite::unbox(lv_export_schema()$version)),
    lapply(meta, function(x) if (length(x) == 1 && !is.list(x)) jsonlite::unbox(x) else x),
    list(generated_at = jsonlite::unbox(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
         generator_version = jsonlite::unbox(as.character(utils::packageVersion("lipdverseUpdater"))),
         lipdr_version = jsonlite::unbox(as.character(utils::packageVersion("lipdR"))),
         files = files))
  jsonlite::write_json(man, fs::path(path, "export_manifest.json"),
                       pretty = TRUE, auto_unbox = FALSE, null = "null")
  cli::cli_alert_success("Wrote {length(files)} table{?s} to {.path {path}}")
  invisible(man)
}

#' @rdname export
#' @param manifest_path Path to an `export_manifest.json`.
#' @return An `lv_issues` tibble; empty when every file matches its recorded hash.
#' @export
lv_export_verify <- function(manifest_path) {
  man <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  dir <- fs::path_dir(manifest_path)
  iss <- lv_issues_empty()
  for (f in man$files) {
    p <- fs::path(dir, f$name)
    if (!fs::file_exists(p)) {
      iss <- lv_issues_bind(iss, lv_issues(check = "export_file_missing",
        severity = "error", message = sprintf("%s is listed but absent.", f$name)))
      next
    }
    if (!identical(digest::digest(file = p, algo = "sha256"), f$sha256))
      iss <- lv_issues_bind(iss, lv_issues(check = "export_hash_mismatch",
        severity = "error", message = sprintf("%s does not match its recorded sha256.", f$name)))
  }
  iss
}

#' @rdname export
#' @details
#' `lv_export_duckdb()` builds a database from the parquet files already
#' written. The parquet files remain the contract: the database is derived,
#' disposable, and rebuildable from them at any time, so nothing should read
#' from it that cannot also be answered from the files.
#'
#' Its value is the joins. Every consumer otherwise re-derives the same
#' timeseries-to-dataset join, and they will not all get the axis filter or the
#' compilation membership right.
#'
#' @param dir An export directory holding the parquet files.
#' @param file Database filename, written inside `dir`.
#' @param overwrite Replace an existing database.
#' @return Path to the database, invisibly.
#' @export
lv_export_duckdb <- function(dir, file = "lipdverse.duckdb", overwrite = TRUE) {
  if (!requireNamespace("duckdb", quietly = TRUE))
    cli::cli_abort("The {.pkg duckdb} package is required.", class = "lv_error_export")
  nms <- names(lv_export_schema()$tables)
  missing <- nms[!fs::file_exists(fs::path(dir, paste0(nms, ".parquet")))]
  if (length(missing))
    cli::cli_abort(c("Export at {.path {dir}} is incomplete.",
                     i = "Missing: {.val {missing}}"), class = "lv_error_export")

  path <- fs::path(dir, file)
  # Built into a temporary file and moved into place, so an interrupted build
  # never leaves a half-populated database where a complete one used to be.
  if (fs::file_exists(path)) {
    if (!overwrite) cli::cli_abort("{.path {path}} exists.", class = "lv_error_export")
  }
  tmp <- fs::path(dir, paste0(".", file, ".building"))
  if (fs::file_exists(tmp)) fs::file_delete(tmp)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmp)
  on.exit({ DBI::dbDisconnect(con, shutdown = TRUE) }, add = TRUE)

  # Identifiers are quoted throughout: `values` is a reserved SQL keyword, so an
  # unquoted CREATE TABLE values fails outright.
  for (n in nms) {
    src <- fs::path(dir, paste0(n, ".parquet"))
    DBI::dbExecute(con, sprintf(
      'CREATE TABLE "%s" AS SELECT * FROM read_parquet(\'%s\')', n, src))
  }

  # One row per timeseries with its dataset alongside: the join every consumer
  # would otherwise write, including the axis flag they would forget.
  DBI::dbExecute(con, "
    CREATE VIEW v_timeseries_full AS
    SELECT t.*, d.dataSetName, d.archiveType, d.version AS datasetVersion,
           d.geo_latitude, d.geo_longitude, d.geo_elevation, d.geo_siteName
    FROM timeseries t LEFT JOIN datasets d USING (datasetId)")

  # Measurements only, axes excluded. The common case, and the one most likely
  # to be got wrong by hand.
  DBI::dbExecute(con, "
    CREATE VIEW v_measurements AS
    SELECT * FROM v_timeseries_full
    WHERE tableKind = 'measurement' AND NOT isAxis")

  DBI::dbExecute(con, "
    CREATE VIEW v_compilation_members AS
    SELECT c.compilation, c.compilationVersion, c.inThisCompilation,
           t.TSid, t.variableName, t.units, t.tableType,
           d.datasetId, d.dataSetName, d.archiveType
    FROM compilations c
    JOIN timeseries t USING (TSid)
    LEFT JOIN datasets d ON d.datasetId = t.datasetId")

  # Values with enough context to be readable on their own.
  DBI::dbExecute(con, '
    CREATE VIEW v_values AS
    SELECT v.TSid, v.row_index, v.value_num, v.value_chr,
           t.variableName, t.units, t.datasetId, d.dataSetName
    FROM "values" v
    JOIN timeseries t USING (TSid)
    LEFT JOIN datasets d ON d.datasetId = t.datasetId')

  DBI::dbDisconnect(con, shutdown = TRUE)
  on.exit(NULL)
  if (fs::file_exists(path)) fs::file_delete(path)
  fs::file_move(tmp, path)
  cli::cli_alert_success("Built {.path {path}}")
  invisible(path)
}
