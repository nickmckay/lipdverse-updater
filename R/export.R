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

# Where parsed per-file exports are kept. Keyed by the file's md5 inside a
# directory named for the schema version and a format epoch, so a schema change
# or a fix to lv_export_one() invalidates everything at once rather than leaving
# stale results to be served silently. Bump LV_EXPORT_CACHE_EPOCH when the shape
# of what lv_export_one() returns changes for reasons the schema version does not
# capture.
# e2 (2026-08-17): lv_add_year_range() went through lv_table_axes(), so
# minYear/maxYear/medianResolution change for most files. The values, not the
# shape, so the schema version does not move -- exactly the case this epoch
# exists for. Without the bump the 2.8 GB cache serves the old empty ranges and
# a code fix looks like it did nothing, which is how this was nearly missed.
# e3 (2026-08-18): dataset-level minYear/maxYear now come from paleo
# measurements only, so the datasets table changes for 1,744 datasets.
LV_EXPORT_CACHE_EPOCH <- 3L

#' Where the per-file export cache lives
#' @return A path, created if absent.
#' @export
lv_export_cache_dir <- function() {
  d <- fs::path(lv_path("state"), "cache", "export",
                sprintf("v%s-e%d", lv_export_schema()$version, LV_EXPORT_CACHE_EPOCH))
  fs::dir_create(d)
  d
}

lv_pkg_root <- function() {
  p <- tryCatch(system.file(package = "lipdverseUpdater"), error = function(e) "")
  if (nzchar(p) && fs::file_exists(fs::path(p, "DESCRIPTION"))) return(p)
  # Loaded rather than installed, so ask pkgload where it was loaded from. The
  # fallback below is right on exactly one machine, which is the whole reason
  # lv_path() exists.
  if (requireNamespace("pkgload", quietly = TRUE)) {
    q <- tryCatch(pkgload::pkg_path(), error = function(e) "")
    if (nzchar(q)) return(q)
  }
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

  ts <- list(); vals <- list(); ens <- list(); interp <- list(); comp <- list()
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
      # An ensemble column is a matrix, and flattening it loses its shape. Keep
      # the dimensions: without them the export hands a consumer 2,413,000
      # values in one vector with no way to tell 2,413 depths x 1,000 members
      # from any other factorisation, and ensembles are 3.43 GB of the 3.6 GB
      # total. The flattening is R's own, column major, so member m occupies
      # row_index ((m - 1) * n_rows + 1) to (m * n_rows).
      dims <- if (is.matrix(cl$values)) dim(cl$values) else NULL
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
        medianResolution = NA_real_, n_values = as.integer(n),
        n_rows = if (is.null(dims)) NA_integer_ else as.integer(dims[1]),
        n_members = if (is.null(dims)) NA_integer_ else as.integer(dims[2]))

      # Ensembles go to their own table. They are 88.9% of all value rows and
      # the most expensive per row, so keeping them here would make the table
      # everyone reads twenty times bigger than it needs to be.
      if (n) {
        if (t$kind %in% c("ensemble", "distribution")) {
          ens[[length(ens) + 1L]] <- tibble::tibble(
            datasetId = dsid, TSid = tsid, row_index = seq_len(n),
            value_num = sv$num, value_chr = sv$chr)
        } else {
          vals[[length(vals) + 1L]] <- tibble::tibble(
            TSid = tsid, row_index = seq_len(n), value_num = sv$num, value_chr = sv$chr)
        }
      }

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
      journal = s1(p$journal), doi = s1(p$doi %||% p$DOI), citeKey = s1(p$citeKey),
      # Filled by lv_resolve_references() against the store; carried here as
      # empty so the table matches its contract whether or not it is resolved.
      citekey = NA_character_, ref_source = NA_character_)
  }

  # Flattened from the file's own changelog. seq preserves the order entries
  # appear in, since versions are strings and do not sort reliably.
  chg <- list()
  for (i in seq_along(L$changelog)) {
    e <- L$changelog[[i]]
    if (!is.list(e)) next
    chg[[length(chg) + 1L]] <- tibble::tibble(
      datasetId = dsid, version = s1(e$version) %|NA|% NA_character_,
      seq = i, lastVersion = s1(e$lastVersion), curator = s1(e$curator),
      timestamp = s1(e$timestamp), compilation = s1(e$compilation),
      run_id = s1(e$run_id),
      n_changes = as.integer(length(e$changes %||% list())))
  }
  chg <- if (length(chg)) dplyr::bind_rows(chg) else lv_export_empty("changelog")
  # A changelog entry with no version cannot be keyed on, and a handful of old
  # files have one. Dropped rather than exported with a null key.
  chg <- chg[!is.na(chg$version), , drop = FALSE]
  chg <- chg[!duplicated(paste(chg$datasetId, chg$version, chg$seq)), , drop = FALSE]

  list(
    datasets = tibble::tibble(
      datasetId = dsid, dataSetName = dsn, archiveType = s1(L$archiveType),
      # `datasetVersion`, with a lowercase s. It was spelled dataSetVersion here,
      # which matches nothing, so every dataset exported with an empty version --
      # and the version is half of the canonical URL a page lives at,
      # /data/<datasetId>/<version>. The changelog is the fallback, since it
      # records the same number and is what lv_tick_version() advances.
      version = s1(L$datasetVersion %||% L$dataSetVersion %||% L$version) %|NA|%
        lv_changelog_last_version(L),
      geo_latitude = geo$lat, geo_longitude = geo$lon, geo_elevation = geo$elev,
      geo_siteName = geo$site,
      # From the PALEO measurements only. A dataset's temporal coverage is the
      # coverage of what was measured, not of its age model: a chron table keeps
      # its own axis, in its own units, and often unlabelled.
      # `108_658.Tiedemann.2006` is the case that showed it -- a 137 kyr marine
      # core whose paleo axis is `yr ka` (unplaceable, so no range) and whose
      # chron `age` column carries no units at all, so the fallback read 0.5-137
      # as years BP and advertised the record as spanning 1813-1949 AD.
      #
      # Measured 2026-08-18: 608 datasets took their entire range from chron
      # this way, and another 1,136 disagreed with their own paleo range. 24% of
      # the database, on the most visible field a dataset page has.
      minYear = lv_paleo_year(ts_t, "minYear", min),
      maxYear = lv_paleo_year(ts_t, "maxYear", max),
      hasChron = has_chron, hasChronEnsemble = has_chron_ens,
      hasPaleoEnsemble = has_paleo_ens,
      n_timeseries = nrow(ts_t), file_md5 = file_md5),
    timeseries = ts_t,
    values = if (length(vals)) dplyr::bind_rows(vals) else lv_export_empty("values"),
    values_ensemble = if (length(ens)) dplyr::bind_rows(ens) else lv_export_empty("values_ensemble"),
    interpretations = if (length(interp)) dplyr::bind_rows(interp) else lv_export_empty("interpretations"),
    publications = if (length(pubs)) dplyr::bind_rows(pubs) else lv_export_empty("publications"),
    compilations = if (length(comp)) dplyr::bind_rows(comp) else lv_export_empty("compilations"),
    changelog = chg,
    # A single dataset has no version ledger and no curated state of its own.
    # They are returned empty rather than omitted so that one dataset's output
    # is still a complete export, and the contract check stays strict about
    # missing tables everywhere.
    # Context tables belong to the store, not to a file, so one dataset's worth
    # of export carries them empty. Listed from the schema rather than by hand:
    # naming them individually meant that declaring compilation_versions broke
    # every test that validates this function's output, because the new table
    # was the one nobody remembered to add here.
    qc_state = lv_export_empty("qc_state"),
    versions = lv_export_empty("versions"),
    vocab = lv_export_empty("vocab"),
    compilation_versions = lv_export_empty("compilation_versions"))
}

lv_finite <- function(x) if (length(x) != 1 || !is.finite(x)) NA_real_ else x

`%|NA|%` <- function(x, y) if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) y else x

# The dataset's own span, taken from its paleo measurement rows alone. Falls
# back to nothing rather than to chron: a range that describes the age model
# instead of the data is worse than an absent one, because it looks answerable.
lv_paleo_year <- function(ts_t, field, fun) {
  if (!nrow(ts_t)) return(NA_real_)
  keep <- ts_t$tableType %in% "paleo" & ts_t$tableKind %in% "measurement"
  v <- ts_t[[field]][keep]
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_real_)
  fun(v)
}

# minYear/maxYear describe the span a timeseries covers.
#
# This used to be a THIRD implementation of axis detection, and the weakest:
# it matched only a column literally named `year`, ignored units, did not mask
# to timesteps carrying a measurement, and did not apply the no-year-zero
# correction. Every record whose axis is called `age` -- most non-annual data --
# came out empty, so **180,444 of 210,723 timeseries (85.6%) had no year range
# at all**, including files whose own metadata carried the right one.
#
# It now goes through lv_table_axes() and lv_calculators(), the same code the QC
# pipeline uses, so a fix to axis handling reaches all of them at once. That was
# the lesson of lv_interp_numberer(): the same concept implemented separately in
# two places crashed iso2k twice, and fixing one copy only moved the crash.
#
# Per column rather than per table, because the mask is per column: an empty
# column in a populated table has no year range, which is what the QC sheet now
# says too (see the clears in hydroclimate2k 0_6_3).
lv_add_year_range <- function(ts_t, tabs) {
  if (!nrow(ts_t)) return(ts_t)
  calc <- lv_calculators()
  for (t in tabs) {
    cols <- lv_cols_of(t$tb)
    cols <- Filter(function(c) is.list(c) && !is.null(c$TSid), cols)
    if (!length(cols)) next

    axes <- lv_table_axes(cols)
    year <- if (!is.null(axes$year)) axes$year else
      if (!is.null(axes$age)) lv_year_from_age(axes$age) else NULL
    if (is.null(year)) next

    for (cl in cols) {
      tsid <- as_chr1(cl$TSid)
      if (is.null(tsid)) next
      hit <- ts_t$TSid == tsid
      if (!any(hit)) next
      vals <- suppressWarnings(as.numeric(unlist(cl$values)))
      mn <- calc$minYear(axes$year, axes$age, vals)
      mx <- calc$maxYear(axes$year, axes$age, vals)
      if (is.finite(mn)) ts_t$minYear[hit] <- mn
      if (is.finite(mx)) ts_t$maxYear[hit] <- mx
      # Resolution over the same timesteps the range describes, so a column
      # padded to the table's length does not report the table's spacing.
      ax <- lv_mask_to_data(year, vals)
      ax <- ax[is.finite(ax)]
      if (length(ax) > 1) {
        ts_t$medianResolution[hit] <- stats::median(abs(diff(sort(ax))))
      }
    }
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
                             progress = TRUE, parallel = NA, workers = NULL,
                             cache = TRUE, compilation = NULL, store = NULL) {
  paths <- if (inherits(dir, "lv_scan")) dir$files$path else
    fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (!is.null(datasets)) {
    # Normalised on both sides. A filename in the database is decomposed and a
    # name from the sheet or the index is composed, so the raw comparison drops
    # every accented dataset -- silently, from the artefact everything
    # downstream reads. Same defect as qc_frame() and lv_csm_frame() had.
    paths <- paths[lv_nfc(sub("\\.lpd$", "", fs::path_file(paths))) %in% lv_nfc(datasets)]
  }
  nms <- names(lv_export_schema()$tables)
  if (!length(paths)) return(stats::setNames(lapply(nms, lv_export_empty), nms))


  if (progress) cli::cli_alert_info("Exporting {length(paths)} file{?s}")

  cache_dir <- if (isTRUE(cache)) lv_export_cache_dir() else NULL
  one <- function(p) {
    md5 <- unname(tools::md5sum(p))
    # Keyed by the file's own checksum, so a changed file misses and an
    # unchanged one is never read twice. The key carries the schema version and
    # a format epoch as well: a cached result produced by older code would
    # otherwise survive a fix to lv_export_one() and quietly export the bug.
    cf <- if (!is.null(cache_dir) && !is.na(md5)) fs::path(cache_dir, paste0(md5, ".rds")) else NULL
    if (!is.null(cf) && fs::file_exists(cf)) {
      hit <- tryCatch(readRDS(cf), error = function(e) NULL)
      if (!is.null(hit)) return(hit)
    }
    L <- tryCatch(suppressWarnings(lipdR::readLipd(p)), error = function(e) NULL)
    if (is.null(L)) return(NULL)
    res <- tryCatch(lv_export_one(L, file_md5 = md5), error = function(e) NULL)
    if (!is.null(res) && !is.null(cf)) {
      # gzip, not xz. xz cost more to write than re-reading the LiPD file saved:
      # a cold cache over 60 datasets took 168s against 59s with no cache at all.
      tryCatch(saveRDS(res, cf), error = function(e) NULL)
    }
    res
  }

  # Reading a few thousand LiPD files is the whole cost of an export and each
  # file is independent, so it parallelises perfectly -- and until 2026-08-14 it
  # never did. `parallel` was decided by whether the package is *installed*,
  # because a future worker can only attach an installed package. It is not
  # installed on the machine that runs this, and every script starts with
  # devtools::load_all(), so the answer was always FALSE: hydroclimate2k's
  # 54-minute export used one of 24 cores.
  #
  # A forked worker inherits the loaded namespace and needs to attach nothing,
  # which is exactly the case load_all() creates. So fork where forking works,
  # and keep the portable future backend for an installed package on Windows.
  if (is.na(parallel)) parallel <- length(paths) > 1
  n_workers <- max(1L, min(as.integer(workers %||% (parallel::detectCores() - 1L)), length(paths)))
  can_fork <- .Platform$OS.type != "windows"
  parts <- if (!parallel || n_workers < 2) {
    purrr::map(unname(paths), one)
  } else if (can_fork) {
    parallel::mclapply(unname(paths), one, mc.cores = n_workers)
  } else if (lv_pkg_installed()) {
    furrr::future_map(unname(paths), one, .options = furrr::furrr_options(
      seed = TRUE, packages = c("lipdR", "lipdverseUpdater")))
  } else {
    purrr::map(unname(paths), one)
  }
  # mclapply returns the condition rather than throwing when a worker fails.
  parts <- lapply(parts, function(x) if (inherits(x, c("try-error", "condition"))) NULL else x)
  ok <- !vapply(parts, is.null, logical(1))
  if (any(!ok)) cli::cli_alert_warning("{sum(!ok)} file{?s} could not be exported.")
  parts <- parts[ok]
  out <- stats::setNames(lapply(nms, function(n)
    dplyr::bind_rows(lapply(parts, `[[`, n))), nms)

  # Context tables come from the store rather than the files. Without a
  # compilation there is no curated state to speak of, so they stay empty
  # rather than being guessed at.
  ctx <- if (!is.null(compilation))
    lv_export_context(compilation, store = store %||% qc_store())
  else stats::setNames(lapply(c("qc_state", "versions", "vocab"), lv_export_empty),
                       c("qc_state", "versions", "vocab"))
  for (n in names(ctx)) out[[n]] <- ctx[[n]]

  # Any table the schema declares that nothing above produced comes back empty
  # rather than absent. A missing table fails lv_export_validate() with a
  # complaint about the contract, which is true but points at the schema instead
  # of at the table nobody filled in -- as compilation_versions did the moment it
  # was declared.
  # ncol as well as is.null: binding zero parts gives a 0x0 tibble, which is a
  # data frame with none of the contract's columns, so it passes an is.null test
  # and fails validation.
  for (n in nms) if (is.null(out[[n]]) || !is.data.frame(out[[n]]) || ncol(out[[n]]) == 0) {
    out[[n]] <- lv_export_empty(n)
  }
  out[nms]
}

#' @rdname export
#' @param tables From [lv_export_tables()].
#' @param path Output directory; created if absent.
#' @param meta Named list folded into the manifest (compilation, version, run_id, ...).
#' @param strict Abort on a contract violation rather than warn.
#' @return The manifest, invisibly.
#' @export
lv_export_write <- function(tables, path, meta = list(), strict = TRUE,
                            ensemble_dir = NULL) {
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

  schema <- lv_export_schema()
  # Tables the schema marks external are written once, somewhere shared, and
  # referenced rather than copied. Duplicating the ensembles into every
  # compilation would be 3.43 GB a time.
  ext <- names(schema$tables)[vapply(schema$tables,
    function(t) isTRUE(t$external), logical(1))]
  ens_dir <- ensemble_dir %||% fs::path(path, "values_ensemble")

  files <- list()
  for (nm in setdiff(names(tables), ext)) {
    f <- fs::path(path, paste0(nm, ".parquet"))
    arrow::write_parquet(tables[[nm]], f)
    files[[length(files) + 1L]] <- list(
      name = jsonlite::unbox(paste0(nm, ".parquet")),
      n_rows = jsonlite::unbox(nrow(tables[[nm]])),
      bytes = jsonlite::unbox(as.numeric(fs::file_size(f))),
      sha256 = jsonlite::unbox(digest::digest(file = f, algo = "sha256")))
  }

  external <- list()
  for (nm in intersect(ext, names(tables))) {
    x <- tables[[nm]]
    key <- schema$tables[[nm]]$partition_by
    fs::dir_create(ens_dir)
    if (nrow(x)) {
      # Hive layout: <dir>/<key>=<value>/part-0.parquet, which arrow and duckdb
      # both prune on when the query filters the key.
      arrow::write_dataset(x, ens_dir, partitioning = key, format = "parquet",
                           existing_data_behavior = "overwrite")
    }
    parts <- fs::dir_ls(ens_dir, glob = "*.parquet", recurse = TRUE, type = "file")
    # One hash over the sorted per-file hashes: a directory has no hash of its
    # own, and this is the same shape as the database fingerprint.
    ph <- sort(vapply(parts, function(p) digest::digest(file = p, algo = "sha256"), character(1)))
    external[[length(external) + 1L]] <- list(
      name = jsonlite::unbox(nm),
      dir = jsonlite::unbox(as.character(ens_dir)),
      partition_by = jsonlite::unbox(key),
      n_rows = jsonlite::unbox(nrow(x)),
      n_partitions = jsonlite::unbox(length(unique(as.character(x[[key]])))),
      n_files = jsonlite::unbox(length(parts)),
      bytes = jsonlite::unbox(sum(as.numeric(fs::file_size(parts)))),
      sha256 = jsonlite::unbox(digest::digest(paste(ph, collapse = ""), algo = "sha256")))
  }

  # The manifest is what makes an export reproducible after the fact: it records
  # which database, which vocabulary and which code produced it.
  man <- c(
    list(schema_version = jsonlite::unbox(lv_export_schema()$version)),
    lapply(meta, function(x) if (length(x) == 1 && !is.list(x)) jsonlite::unbox(x) else x),
    list(generated_at = jsonlite::unbox(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
         generator_version = jsonlite::unbox(as.character(utils::packageVersion("lipdverseUpdater"))),
         lipdr_version = jsonlite::unbox(as.character(utils::packageVersion("lipdR"))),
         files = files, external = external))
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
  # External tables are a directory of partitions rather than one file, so the
  # recorded hash is over the sorted per-file hashes. An export that references
  # a shared store is only trustworthy if that store is checked too -- it is the
  # part most likely to have moved on since.
  for (e in man$external %||% list()) {
    if (!fs::dir_exists(e$dir)) {
      iss <- lv_issues_bind(iss, lv_issues(check = "export_external_missing",
        severity = "error",
        message = sprintf("%s: no directory at %s.", e$name, e$dir)))
      next
    }
    parts <- fs::dir_ls(e$dir, glob = "*.parquet", recurse = TRUE, type = "file")
    if (length(parts) != e$n_files) {
      iss <- lv_issues_bind(iss, lv_issues(check = "export_external_file_count",
        severity = "error",
        message = sprintf("%s: %d partition file%s, manifest recorded %d.",
                          e$name, length(parts), if (length(parts) == 1) "" else "s", e$n_files)))
    }
    ph <- sort(vapply(parts, function(p) digest::digest(file = p, algo = "sha256"), character(1)))
    if (!identical(digest::digest(paste(ph, collapse = ""), algo = "sha256"), e$sha256))
      iss <- lv_issues_bind(iss, lv_issues(check = "export_hash_mismatch",
        severity = "error",
        message = sprintf("%s does not match its recorded sha256.", e$name)))
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
lv_export_duckdb <- function(dir, file = "lipdverse.duckdb", overwrite = TRUE,
                             ensemble_dir = NULL) {
  if (!requireNamespace("duckdb", quietly = TRUE))
    cli::cli_abort("The {.pkg duckdb} package is required.", class = "lv_error_export")
  schema <- lv_export_schema()
  ext <- names(schema$tables)[vapply(schema$tables,
    function(t) isTRUE(t$external), logical(1))]
  nms <- setdiff(names(schema$tables), ext)
  missing <- nms[!fs::file_exists(fs::path(dir, paste0(nms, ".parquet")))]
  if (length(missing))
    cli::cli_abort(c("Export at {.path {dir}} is incomplete.",
                     i = "Missing: {.val {missing}}"), class = "lv_error_export")

  # The external tables live wherever the manifest says, which is usually not
  # inside this directory. Take it from there rather than assuming.
  man_path <- fs::path(dir, "export_manifest.json")
  ens_dirs <- list()
  if (is.null(ensemble_dir) && fs::file_exists(man_path)) {
    man <- jsonlite::fromJSON(man_path, simplifyVector = FALSE)
    for (e in man$external %||% list()) ens_dirs[[e$name]] <- e$dir
  }
  for (nm in ext) {
    if (!is.null(ensemble_dir)) ens_dirs[[nm]] <- ensemble_dir
    else if (is.null(ens_dirs[[nm]])) ens_dirs[[nm]] <- fs::path(dir, nm)
  }

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

  # External tables become views, not tables: copying 3.43 GB of ensembles into
  # every compilation's database would undo the reason they were split out.
  # A view over the hive layout still prunes partitions when the query filters
  # on the partition key.
  for (nm in ext) {
    d <- ens_dirs[[nm]]
    if (is.null(d) || !fs::dir_exists(d)) {
      cli::cli_alert_warning("{nm}: no data at {.path {d %||% '<unset>'}}; view not created.")
      next
    }
    parts <- fs::dir_ls(d, glob = "*.parquet", recurse = TRUE, type = "file")
    if (!length(parts)) next
    DBI::dbExecute(con, sprintf(
      'CREATE VIEW "%s" AS SELECT * FROM read_parquet(\'%s/**/*.parquet\', hive_partitioning = true)',
      nm, d))
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

#' @rdname export
#' @details
#' `lv_export_context()` builds the tables that do not come from the LiPD files:
#' the curated state, the version ledger and the vocabulary in force. They are
#' what lets an export be read years later without the rest of this system --
#' the vocabulary especially, since checking a value against today's terms
#' answers a different question from checking it against the ones that applied.
#'
#' @param compilation Compilation name.
#' @param store From [qc_store()].
#' @param vocab Vocabulary tables, overlay included.
#' @return A named list of the context tibbles.
#' @export
lv_export_context <- function(compilation, store = qc_store(),
                              vocab = lv_vocab_overlay(store = store)) {
  st <- tryCatch(qc_state_current(store, compilation), error = function(e) NULL)
  qc <- if (is.null(st) || !nrow(st)) lv_export_empty("qc_state") else {
    keep <- intersect(names(lv_export_schema()$tables$qc_state$columns), names(st))
    x <- tibble::as_tibble(st)[, keep, drop = FALSE]
    for (k in c("updated_at", "source", "actor", "dataset_id", "value"))
      if (k %in% names(x)) x[[k]] <- as.character(x[[k]])
    if ("present" %in% names(x)) x$present <- as.logical(x$present)
    # present is required, and a row with an unknown present is not a fact.
    x[!is.na(x$present), , drop = FALSE]
  }

  vs <- tryCatch(lv_versions(store), error = function(e) NULL)
  ver <- local({
    empty <- lv_export_empty("versions")
    if (is.null(vs) || !nrow(vs)) return(empty)
    x <- tibble::as_tibble(vs)
    x <- x[x$compilation %in% compilation, , drop = FALSE]
    if (!nrow(x)) return(empty)
    # Built column by column into a new tibble, not assigned into the empty one:
    # assigning an n-length vector into a zero-row tibble is a recycling error,
    # which only fires once a compilation actually has versions -- so it passed
    # every test that used a fresh store.
    cols <- names(empty)
    tibble::as_tibble(stats::setNames(lapply(cols, function(k) {
      if (k %in% names(x)) {
        if (identical(k, "n_datasets")) suppressWarnings(as.integer(x[[k]]))
        else as.character(x[[k]])
      } else if (identical(k, "n_datasets")) rep(NA_integer_, nrow(x))
      else rep(NA_character_, nrow(x))
    }), cols))
  })

  vb <- list()
  for (f in names(vocab)) {
    v <- vocab[[f]]
    if (!is.data.frame(v) || !"lipdName" %in% names(v)) next
    syn <- if ("synonym" %in% names(v)) as.character(v$synonym) else as.character(v$lipdName)
    vb[[length(vb) + 1L]] <- tibble::tibble(
      field = f, lipdName = as.character(v$lipdName), synonym = syn)
  }
  vbt <- if (length(vb)) dplyr::bind_rows(vb) else lv_export_empty("vocab")
  vbt <- vbt[!is.na(vbt$lipdName) & !is.na(vbt$synonym), , drop = FALSE]
  vbt <- vbt[!duplicated(paste(vbt$field, vbt$lipdName, vbt$synonym)), , drop = FALSE]

  list(qc_state = qc, versions = ver, vocab = vbt)
}

#' @rdname export
#' @details
#' `lv_export()` is the whole thing: build the tables for a compilation, write
#' them under `<export_dir>/<compilation>/<version>/`, and record in the manifest
#' exactly which database, vocabulary and curated state produced them.
#'
#' Those three fingerprints are the point. An export nobody can trace back to its
#' inputs is a snapshot of an unknown thing, and the site generator that consumes
#' it has no way to tell a stale copy from a current one.
#'
#' The ensembles go to a shared store outside the version directory. They are
#' 88.9% of the value rows and change far less often than the metadata around
#' them, so copying them into every compilation and every version would cost
#' tens of gigabytes to say the same thing repeatedly.
#'
#' @param cfg From [lv_config()].
#' @param version The version being exported, e.g. `"0_6_1"`.
#' @param datasets Dataset names to export. Defaults to the compilation's
#'   considered set, which the caller usually already has.
#' @param export_dir Root for exports.
#' @param ensemble_dir Shared ensemble store; defaults to `<export_dir>/ensembles`.
#' @param store From [qc_store()].
#' @param duckdb Also build the convenience database.
#' @param bundle Also write the member `.lpd` files as a zip, which is what a
#'   visitor downloads.
#' @param bib Also write the BibTeX bibliography for the reference page.
#' @param dry_run Report without writing.
#' @return The manifest, invisibly.
#' @export
lv_export <- function(cfg, version, datasets, export_dir = lv_path("export"),
                      ensemble_dir = NULL, store = qc_store(),
                      duckdb = TRUE, bundle = TRUE, bib = TRUE, dry_run = TRUE,
                      run_id = lv_run_id(), progress = TRUE) {
  comp <- cfg$compilation
  dir <- fs::path(export_dir, comp, version)
  ens <- ensemble_dir %||% fs::path(export_dir, "ensembles")

  if (progress) cli::cli_alert_info("Exporting {.val {comp}} {.val {version}} from {length(datasets)} dataset{?s}")
  tables <- lv_export_tables(cfg$lipd_dir, datasets = datasets, progress = progress,
                             compilation = comp, store = store)

  # What this compilation contained at each of its published versions, so a
  # shell site for an old version can name the pages it should embed.
  tables$compilation_versions <- lv_version_members(store, compilation = comp)

  # Fill the publications from the reference database before anything is
  # written, so the parquet, the duckdb and the bibliography all say the same
  # thing. The files keep whatever they have; the store answers only for gaps.
  refs <- tryCatch(lv_references(store), error = function(e) NULL)
  links <- tryCatch(lv_reference_links(store), error = function(e) NULL)
  if (!is.null(refs) && nrow(refs) && nrow(tables$publications)) {
    before <- sum(!is.na(tables$publications$title))
    tables$publications <- lv_resolve_references(tables$publications, refs, links)
    if (progress) {
      cli::cli_alert_info(
        "References: {sum(!is.na(tables$publications$ref_source))} of {nrow(tables$publications)} publication{?s} resolved from the store, titles {before} -> {sum(!is.na(tables$publications$title))}")
    }
  }

  iss <- lv_export_validate(tables)
  n_err <- lv_n_issues(iss, "error")
  cli::cli_alert_info("{nrow(tables$datasets)} dataset{?s}, {nrow(tables$timeseries)} timeseries, {nrow(tables$values)} value{?s}, {nrow(tables$values_ensemble)} ensemble value{?s}")
  if (n_err) cli::cli_alert_warning("{n_err} contract violation{?s}.")

  # Recorded, not recomputed later: these are what make the export traceable,
  # and each is cheap here and impossible to reconstruct afterwards.
  meta <- list(
    compilation = comp, version = version, run_id = run_id,
    db_fingerprint = lv_scan(cfg$lipd_dir)$fingerprint,
    vocab_pin = attr(lv_vocab(validate = FALSE), "pin") %||% NA_character_,
    # An empty state still hashes to something. NA here is indistinguishable
    # from "not recorded", and a consumer cannot tell an export of a compilation
    # with no curated state from one where the field was never filled in.
    qc_state_hash = if (nrow(tables$qc_state)) {
      lv_dataset_set_hash(paste(tables$qc_state$tsid, tables$qc_state$field,
                                tables$qc_state$value))
    } else digest::digest("", algo = "md5"),
    n_datasets = nrow(tables$datasets))

  if (dry_run) {
    cli::cli_alert_info("Dry run. Would write to {.path {dir}} (ensembles to {.path {ens}}).")
    return(invisible(c(meta, list(tables = vapply(tables, nrow, integer(1)),
                                  issues = iss)))) 
  }

  man <- lv_export_write(tables, dir, meta = meta, ensemble_dir = ens)
  if (duckdb) lv_export_duckdb(dir, ensemble_dir = ens)

  # The download artefacts, scoped to actual members rather than the considered
  # set the tables cover: a zip called hydroclimate2k that held 482 datasets not
  # in hydroclimate2k would be wrong in a way nobody would notice until they
  # used it. See lv_export_bundle().
  members <- local({
    c <- tables$compilations
    ids <- unique(c$datasetId[c$compilation %in% comp & c$inThisCompilation %in% TRUE])
    list(ids = ids, names = tables$datasets$dataSetName[tables$datasets$datasetId %in% ids])
  })
  if (bundle) {
    lv_export_bundle(members$names, cfg$lipd_dir,
                     fs::path(dir, paste0(comp, version, ".zip")), progress = progress)
  }
  if (bib) {
    lv_export_bib(tables$publications, fs::path(dir, paste0(comp, "-", version, ".bib")),
                  datasets = members$ids, progress = progress)
  }
  invisible(man)
}

#' @rdname export
#' @details
#' `lv_export_database()` is the same thing for the whole database rather than
#' one compilation. The site indexes everything LiPDverse holds, and **221 of
#' 7,293 datasets belong to no compilation at all**, so a site assembled only
#' from per-compilation exports would silently omit them.
#'
#' Two tables differ from a compilation export, both because they are
#' compilation-scoped by nature:
#'
#' \describe{
#'   \item{`qc_state`}{Empty. The curated state belongs to a compilation -- the
#'     store is keyed by one -- and the table's key is `[tsid, field]`, with no
#'     room for two compilations holding different values for the same cell.
#'     Unioning them would collide silently, so the per-compilation exports
#'     remain the place to read curated state.}
#'   \item{`versions`}{Every compilation's version ledger, not one's, since the
#'     table already carries a `compilation` column.}
#' }
#'
#' `compilations` needs no special handling: it is derived from the files, so it
#' is already the full membership picture.
#'
#' @param label Name for this snapshot, used as the directory under
#'   `<export_dir>/_database/`. Defaults to the date, since the database has a
#'   fingerprint rather than a version.
#' @export
lv_export_database <- function(label = format(Sys.Date(), "%Y-%m-%d"),
                               dir = lv_path("database"),
                               export_dir = lv_path("export"), ensemble_dir = NULL,
                               store = qc_store(), duckdb = TRUE, dry_run = TRUE,
                               run_id = lv_run_id(), progress = TRUE) {
  out <- fs::path(export_dir, "_database", label)
  ens <- ensemble_dir %||% fs::path(export_dir, "ensembles")

  if (progress) cli::cli_alert_info("Exporting the whole database as {.val {label}}")
  # compilation = NULL: the tables are built from the files, and the context
  # tables that need a compilation are filled in below.
  tables <- lv_export_tables(dir, datasets = NULL, progress = progress,
                             compilation = NULL, store = NULL)

  vs <- tryCatch(lv_versions(store), error = function(e) NULL)
  if (!is.null(vs) && nrow(vs)) {
    empty <- lv_export_empty("versions")
    x <- tibble::as_tibble(vs)
    tables$versions <- tibble::as_tibble(stats::setNames(lapply(names(empty), function(k) {
      if (!k %in% names(x)) {
        if (identical(k, "n_datasets")) rep(NA_integer_, nrow(x)) else rep(NA_character_, nrow(x))
      } else if (identical(k, "n_datasets")) suppressWarnings(as.integer(x[[k]]))
      else as.character(x[[k]])
    }), names(empty)))
  }
  # The vocabulary is global, so it comes through whichever compilation is asked
  # for; asking for none would leave it empty.
  tables$vocab <- lv_export_context(NA_character_, store = store)$vocab

  # The whole-database export never resolved its publications, which is why
  # citekey and ref_source were empty for all 13,271 of them while the
  # per-compilation exports filled them in (issue #16). Same store, same rules.
  refs <- tryCatch(lv_references(store), error = function(e) NULL)
  links <- tryCatch(lv_reference_links(store), error = function(e) NULL)
  if (!is.null(refs) && nrow(refs) && nrow(tables$publications)) {
    before <- sum(!is.na(tables$publications$title))
    tables$publications <- lv_resolve_references(tables$publications, refs, links)
    if (progress) {
      cli::cli_alert_info(
        "References: {sum(!is.na(tables$publications$ref_source))} of {nrow(tables$publications)} publication{?s} resolved, titles {before} -> {sum(!is.na(tables$publications$title))}")
    }
  }
  # Every compilation's published membership, since this export is the whole
  # database rather than one compilation.
  tables$compilation_versions <- lv_version_members(store)

  iss <- lv_export_validate(tables)
  cli::cli_alert_info("{nrow(tables$datasets)} dataset{?s}, {nrow(tables$timeseries)} timeseries, {nrow(tables$values)} value{?s}, {nrow(tables$values_ensemble)} ensemble value{?s}")
  if (lv_n_issues(iss, "error")) {
    cli::cli_alert_warning("{lv_n_issues(iss, 'error')} contract violation{?s}.")
  }
  n_orphan <- sum(!tables$datasets$datasetId %in% tables$compilations$datasetId)
  cli::cli_alert_info("{n_orphan} dataset{?s} in no compilation, which a per-compilation export cannot reach")

  meta <- list(
    compilation = NA_character_, version = label, run_id = run_id,
    db_fingerprint = lv_scan(dir)$fingerprint,
    vocab_pin = attr(lv_vocab(validate = FALSE), "pin") %||% NA_character_,
    qc_state_hash = digest::digest("", algo = "md5"),
    n_datasets = nrow(tables$datasets))

  if (dry_run) {
    cli::cli_alert_info("Dry run. Would write to {.path {out}} (ensembles to {.path {ens}}).")
    return(invisible(c(meta, list(tables = vapply(tables, nrow, integer(1)), issues = iss))))
  }
  man <- lv_export_write(tables, out, meta = meta, ensemble_dir = ens)
  if (duckdb) lv_export_duckdb(out, ensemble_dir = ens)
  invisible(man)
}
