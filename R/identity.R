#' Build an identity index over the database
#'
#' Reads only the metadata JSON inside each `.lpd` (a zip) rather than doing a
#' full [lipdR::readLipd()], because identity checks need names and IDs, not
#' data columns. That makes indexing the whole database cheap enough to run
#' before every update.
#'
#' Replaces lipdverseR's `createDatabaseReference()`, which cached into a global
#' via `<<-` and whose duplicate-name branch tested `ref$dataSetNames`, a column
#' that does not exist, so `duplicated(NULL)` was always `logical(0)` and the
#' warning could never fire.
#'
#' @param scan An `lv_scan`, or a directory path.
#' @param workers Parallel workers.
#' @param cache Reuse cached per-file extracts keyed by file md5.
#' @return An `lv_index` list with `datasets` and `timeseries` tibbles.
#' @export
lv_db_index <- function(scan = lv_scan(), workers = NULL, cache = TRUE) {
  if (is.character(scan)) scan <- lv_scan(scan)
  files <- scan$files

  # Versioned filename: the extract schema has grown (tableKind, compilations),
  # and a cache keyed only on file md5 would keep serving extracts that predate
  # the new fields.
  cache_file <- fs::path(lv_path("state"), "cache", "index-extracts-v2.rds")
  prior <- if (cache && fs::file_exists(cache_file)) readRDS(cache_file) else NULL

  hit <- if (is.null(prior)) rep(FALSE, nrow(files)) else files$md5 %in% names(prior)
  todo <- files[!hit, ]

  if (nrow(todo)) {
    cli::cli_alert_info("Extracting identity from {nrow(todo)} file{?s} ({sum(hit)} cached)")
    # Sequential on purpose: reading one small JSON member out of each zip runs
    # the full 7,177-file database in ~23s, which is not worth the cost of
    # shipping this package's helpers into worker processes. File hashing is
    # parallel because it reads every byte; this does not.
    got <- lapply(todo$path, lv_extract_identity)
    names(got) <- todo$md5
    prior <- c(prior, got)
  }

  if (cache) {
    fs::dir_create(fs::path_dir(cache_file))
    saveRDS(prior[files$md5], cache_file)
  }

  extracts <- prior[files$md5]

  datasets <- purrr::list_rbind(purrr::map2(extracts, seq_len(nrow(files)), function(e, i) {
    tibble::tibble(
      path           = files$path[i],
      file           = files$file[i],
      md5            = files$md5[i],
      dataSetName    = e$dataSetName %||% NA_character_,
      datasetId      = e$datasetId %||% NA_character_,
      datasetVersion = e$datasetVersion %||% NA_character_,
      archiveType    = e$archiveType %||% NA_character_,
      n_ts           = length(e$tsids),
      parse_error    = e$error %||% NA_character_
    )
  }))
  # The filename is the operational key: readLipd/writeLipd round-trip through
  # it, and lipdverseR names its in-memory list by it.
  datasets$fileDataSetName <- sub("\\.lpd$", "", datasets$file)

  timeseries <- purrr::list_rbind(purrr::map(extracts, function(e) {
    if (!length(e$tsids)) return(tibble::tibble(TSid = character(), datasetId = character(),
                                                dataSetName = character(), tableType = character(),
                                                tableKind = character(), variableName = character(),
                                                compilations = list()))
    tibble::tibble(
      TSid         = e$tsids,
      datasetId    = e$datasetId %||% NA_character_,
      dataSetName  = e$dataSetName %||% NA_character_,
      tableType    = e$tstypes %||% NA_character_,
      # measurement vs a model's summary/ensemble table. Compilation membership
      # belongs on measurement columns; model columns are derived.
      tableKind    = e$tskinds %||% NA_character_,
      variableName = e$tsnames %||% NA_character_,
      # Which compilations claim this column. A list column because membership
      # is many-to-one: 56% of datasets are in two or more compilations.
      compilations = e$tscomps %||% vector("list", length(e$tsids))
    )
  }))

  structure(list(datasets = datasets, timeseries = timeseries,
                 fingerprint = scan$fingerprint, indexed_at = Sys.time()),
            class = "lv_index")
}

# Pull identity fields out of one .lpd without a full LiPD parse.
lv_extract_identity <- function(path) {
  out <- list(tsids = character(), tsnames = character(), tstypes = character())
  con <- NULL
  txt <- tryCatch({
    nms <- utils::unzip(path, list = TRUE)$Name
    j <- grep("\\.jsonld$", nms, value = TRUE)
    if (!length(j)) stop("no .jsonld member")
    con <- unz(path, j[1])
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }, error = function(e) {
    out$error <<- conditionMessage(e)
    NULL
  })
  if (!is.null(con)) try(close(con), silent = TRUE)
  if (is.null(txt)) return(out)

  m <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                error = function(e) { out$error <<- paste("json:", conditionMessage(e)); NULL })
  if (is.null(m)) return(out)

  out$dataSetName <- as_chr1(m$dataSetName)
  out$datasetId   <- as_chr1(m$datasetId)
  out$archiveType <- as_chr1(m$archiveType)
  out$datasetVersion <- lv_changelog_version(m$changelog)

  cols <- c(lv_walk_columns(m$paleoData, "paleo"), lv_walk_columns(m$chronData, "chron"))
  if (length(cols)) {
    out$tsids   <- vapply(cols, function(c) c$TSid %||% NA_character_, character(1))
    out$tsnames <- vapply(cols, function(c) c$variableName %||% NA_character_, character(1))
    out$tstypes <- vapply(cols, function(c) c$type %||% NA_character_, character(1))
    out$tskinds <- vapply(cols, function(c) c$kind %||% NA_character_, character(1))
    out$tscomps <- lapply(cols, function(c) c$compilations %||% character())
    keep <- !is.na(out$tsids)
    out$tsids <- out$tsids[keep]; out$tsnames <- out$tsnames[keep]
    out$tstypes <- out$tstypes[keep]; out$tskinds <- out$tskinds[keep]
    out$tscomps <- out$tscomps[keep]
  }
  out
}

# measurementTable / model[[]]$summaryTable / ensembleTable all hold columns.
#
# Two on-disk layouts exist. Current files put columns in an unnamed list under
# a `columns` key; older ones store each column as a named entry of the table
# itself. Handle both, since the database spans many years of writers.
lv_walk_columns <- function(x, type) {
  res <- list()
  if (is.null(x)) return(res)
  for (obj in x) {
    meas <- obj$measurementTable
    modl <- unlist(lapply(obj$model, function(m) {
      c(m$summaryTable, m$ensembleTable, m$distributionTable)
    }), recursive = FALSE)
    tabs <- c(meas, modl)
    kinds <- c(rep("measurement", length(meas)), rep("model", length(modl)))
    for (ti in seq_along(tabs)) {
      tb <- tabs[[ti]]; kind <- kinds[ti]
      if (!is.list(tb)) next
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      nms  <- names(cols)
      for (k in seq_along(cols)) {
        col <- cols[[k]]
        if (!is.list(col) || is.null(col$TSid)) next
        res[[length(res) + 1L]] <- list(
          TSid = as_chr1(col$TSid),
          variableName = as_chr1(col$variableName) %||% (if (!is.null(nms)) nms[k] else NA_character_),
          type = type, kind = kind,
          compilations = lv_column_compilations(col$inCompilation)
        )
      }
    }
  }
  res
}

# Compilation membership is per column, not per dataset, so it belongs on the
# timeseries table. Two columns in the database store `inCompilation` as a bare
# string rather than a list of entries; handle both rather than dropping them.
lv_column_compilations <- function(ic) {
  if (is.null(ic) || !length(ic)) return(character())
  if (!is.list(ic)) return(as.character(ic))
  out <- vapply(ic, function(e) {
    if (!is.list(e)) return(as_chr1(e) %||% NA_character_)
    as_chr1(e$compilationName) %||% NA_character_
  }, character(1))
  unique(out[!is.na(out)])
}

#' Timeseries belonging to a compilation
#'
#' Membership lives on the column, so the compilation's scope is a property of
#' the files rather than of the sheet. Using the sheet instead would silently
#' widen a run to every column of every member dataset.
#'
#' @param index An `lv_index`.
#' @param compilation Compilation name.
#' @return A character vector of TSids.
#' @export
lv_compilation_timeseries <- function(index, compilation) {
  hit <- vapply(index$timeseries$compilations, function(v) compilation %in% v, logical(1))
  index$timeseries$TSid[hit]
}

lv_changelog_version <- function(cl) {
  if (is.null(cl) || !length(cl)) return(NA_character_)
  v <- vapply(cl, function(e) as_chr1(e$version) %||% NA_character_, character(1))
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  as.character(max(numeric_version(v, strict = FALSE), na.rm = TRUE))
}

as_chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- x[[1]]
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(NULL)
  as.character(x)
}

#' @export
print.lv_index <- function(x, ...) {
  cli::cli_h3("lv_index")
  cli::cli_bullets(c(
    "*" = "{nrow(x$datasets)} dataset{?s}, {nrow(x$timeseries)} timeseries",
    "*" = "{sum(!is.na(x$datasets$parse_error))} parse failure{?s}"
  ))
  invisible(x)
}
