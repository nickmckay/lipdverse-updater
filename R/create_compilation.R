#' Create a compilation from a list of TSids
#'
#' Adds an `inCompilation` entry to every named column. This is the common way
#' a compilation begins: someone has a list of timeseries and wants them
#' gathered under a name.
#'
#' The write is **additive**. An existing membership for the same compilation is
#' updated in place (its version list gains the new version); memberships for
#' other compilations are untouched. Nothing else in the file changes.
#'
#' Never writes in place: output goes to a staging directory for [lv_promote()].
#'
#' @param tsids TSids to include.
#' @param compilation Compilation name.
#' @param version Compilation version to record, e.g. `"1_0_0"`.
#' @param dir Source database directory.
#' @param out Staging directory.
#' @param index An `lv_index`, to avoid re-reading the database.
#' @param progress Show progress.
#' @return A list of `issues`, `datasets` touched, and `tsids` placed.
#' @export
lv_create_compilation <- function(tsids, compilation, version = "1_0_0",
                                  dir = lv_path("database"), out,
                                  index = NULL, progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  if (!nzchar(compilation)) cli::cli_abort("{.arg compilation} must be a non-empty name.")
  fs::dir_create(out)

  tsids <- unique(stats::na.omit(as.character(tsids)))
  if (!length(tsids)) cli::cli_abort("No TSids given.", class = "lv_error_compilation")

  if (is.null(index)) index <- lv_db_index(lv_scan(dir), cache = TRUE)
  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  ds <- unname(ts2ds[tsids])

  missing_ts <- tsids[is.na(ds)]
  issues <- if (length(missing_ts)) {
    lv_issues(check = "tsid_not_in_database", severity = "warn",
              message = "TSid is not in any file; not added to the compilation.",
              TSid = missing_ts)
  } else lv_issues_empty()

  keep <- !is.na(ds)
  tsids <- tsids[keep]; ds <- ds[keep]
  by_ds <- split(tsids, ds)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)

  if (progress) {
    cli::cli_alert_info("Adding {length(tsids)} TSid{?s} across {length(by_ds)} dataset{?s} to {.val {compilation}}")
  }

  placed <- 0L
  for (dsn in names(by_ds)) {
    p <- paths[[dsn]]
    L <- tryCatch(lipdR::readLipd(p), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the dataset.", dataSetName = dsn, path = p))
      next
    }
    r <- lv_add_membership(L, by_ds[[dsn]], compilation, version)
    placed <- placed + r$placed
    if (nrow(r$issues)) issues <- lv_issues_bind(issues, r$issues)
    lipdR::writeLipd(r$L, path = out, removeNamesFromLists = TRUE)
  }

  list(issues = issues, datasets = names(by_ds), tsids = tsids, placed = placed)
}

#' Add compilation membership to the named columns of one dataset
#' @keywords internal
lv_add_membership <- function(L, tsids, compilation, version) {
  issues <- lv_issues_empty()
  dsn <- L$dataSetName %||% NA_character_
  want <- tsids
  placed <- 0L

  for (blk in c("paleoData", "chronData")) {
    if (is.null(L[[blk]])) next
    for (pd in seq_along(L[[blk]])) {
      for (tb in seq_along(L[[blk]][[pd]]$measurementTable)) {
        tab <- L[[blk]][[pd]]$measurementTable[[tb]]
        for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
          col <- tab[[cn]]
          if (!is.list(col) || is.null(col$TSid)) next
          tsid <- as.character(col$TSid)[1]
          if (!tsid %in% want) next

          ic <- col$inCompilation
          if (!is.list(ic)) {
            # Two columns in the database store inCompilation as a bare string
            # rather than a list of entries. Discarding it would be exactly the
            # kind of silent loss this rewrite exists to stop, so promote it to
            # the normal structure and report it.
            old <- as.character(ic)
            old <- old[!is.na(old) & nzchar(old)]
            ic <- lapply(old, function(n) list(compilationName = n))
            if (length(old)) {
              issues <- lv_issues_bind(issues, lv_issues(
                check = "malformed_inCompilation", severity = "warn",
                message = "inCompilation was a bare string; converted to an entry.",
                dataSetName = dsn, TSid = tsid, value = old))
            }
          }
          names_here <- vapply(ic, function(e) {
            n <- if (is.list(e)) unlist(e[["compilationName"]]) else NULL
            if (length(n)) as.character(n)[1] else NA_character_
          }, character(1))

          j <- which(names_here == compilation)
          if (length(j)) {
            # Already a member: add this version if it is not recorded yet.
            j <- j[1]
            v <- unlist(ic[[j]]$compilationVersion)
            if (!version %in% v) ic[[j]]$compilationVersion <- c(v, version)
          } else {
            ic[[length(ic) + 1L]] <- list(compilationName = compilation,
                                          compilationVersion = version)
          }
          col$inCompilation <- ic
          tab[[cn]] <- col
          want <- setdiff(want, tsid)
          placed <- placed + 1L
        }
        L[[blk]][[pd]]$measurementTable[[tb]] <- tab
      }
    }
  }

  if (length(want)) {
    issues <- lv_issues_bind(issues, lv_issues(
      check = "tsid_not_in_dataset", severity = "warn",
      message = "TSid was expected in this dataset but not found.",
      dataSetName = dsn, TSid = want))
  }
  list(L = L, issues = issues, placed = placed)
}

#' Build the tabs for a new compilation's QC sheet
#'
#' @param cells A cell table from [qc_frame()], restricted to the compilation.
#' @param index An `lv_index`, for the membership tab.
#' @param registry Field registry.
#' @return A named list of data frames, one per tab.
#' @export
lv_compilation_sheet <- function(cells, index, registry = lv_qc_fields()) {
  qc <- qc_cells_to_sheet(cells, registry)
  # TSid first, then the rest alphabetically: a stable column order makes the
  # sheet diffable between runs.
  rest <- sort(setdiff(names(qc), "TSid"))
  qc <- qc[, c("TSid", rest), drop = FALSE]

  ds <- unique(index$timeseries$dataSetName[index$timeseries$TSid %in% cells$tsid])
  dsid <- index$datasets$datasetId[match(ds, index$datasets$fileDataSetName)]
  members <- tibble::tibble(dsn = ds, dsid = dsid, inComp = "TRUE")
  members <- members[order(members$dsn), , drop = FALSE]

  list(QC = qc, datasetsInCompilation = members)
}
