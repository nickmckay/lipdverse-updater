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

#' Which datasets are in a compilation
#'
#' The datasets a run *considers*, which is wider than the datasets whose
#' timeseries are members. Membership grows in two steps, and this is the first:
#'
#' 1. A curator sets a dataset to TRUE in `datasetsInCompilation`. Every one of
#'    its timeseries then appears in the QC tab, all with
#'    `inThisCompilation = FALSE`.
#' 2. They flag individual timeseries TRUE there to admit them.
#'
#' So this returns the QC tab's scope, not the compilation's contents. Names are
#' checked against the database rather than trusted, since a sheet can name a
#' dataset that no longer exists.
#'
#' @param cfg An `lv_config`.
#' @param backend A sheet backend.
#' @param index An `lv_index`, to resolve names against the files.
#' @return A list of `datasets` considered, `missing` names, and `excluded`.
#' @export
lv_compilation_datasets <- function(cfg, backend, index = NULL) {
  x <- sheet_read(backend, cfg$qc_sheet_id, cfg$qc_tabs$datasets)
  x <- x[, !duplicated(names(x)) & nzchar(names(x)) & !is.na(names(x)), drop = FALSE]
  nm <- intersect(c("dsn", "dataSetName"), names(x))
  if (!length(nm)) {
    cli::cli_abort("Membership tab {.val {cfg$qc_tabs$datasets}} has no dsn column.",
                   class = "lv_error_sheet")
  }

  # The tab's own instructions: "Any datasets marked as FALSE will not be
  # considered for the update, NA or TRUE will be considered." Requiring TRUE
  # instead would drop every dataset a curator had not yet ruled on, which is
  # precisely the set the tab exists to surface.
  keep <- if ("inComp" %in% names(x)) !tolower(trimws(x$inComp)) %in% "false" else rep(TRUE, nrow(x))
  want <- unique(stats::na.omit(x[[nm[1]]][keep]))

  if (is.null(index)) index <- lv_db_index(lv_scan(cfg$lipd_dir), cache = TRUE)
  have <- want %in% index$datasets$fileDataSetName
  list(datasets = want[have], missing = want[!have],
       excluded = unique(stats::na.omit(x[[nm[1]]][!keep])))
}

#' The file-side view of compilation membership
#'
#' One cell per timeseries, so membership merges by the same rules as any other
#' curator-owned field: the sheet wins a straight disagreement, the store
#' supplies the baseline, and every change becomes an event.
#'
#' @param index An `lv_index`.
#' @param compilation Compilation name.
#' @param tsids Timeseries in scope. Those not in the compilation get `FALSE`,
#'   which is what exposes a candidate to the curator.
#' @return A cell table.
#' @export
lv_membership_frame <- function(index, compilation, tsids) {
  members <- lv_compilation_timeseries(index, compilation)
  dsid <- index$timeseries$datasetId[match(tsids, index$timeseries$TSid)]
  tibble::tibble(
    tsid = tsids, field = "inThisCompilation",
    value = ifelse(tsids %in% members, "TRUE", "FALSE"),
    present = TRUE, dataset_id = dsid,
    updated_at = NA_character_, source = "lipd", actor = NA_character_)
}

#' Apply merged membership decisions to the files
#'
#' Additions add an `inCompilation` entry; removals drop it. Both are written
#' to staging for [lv_promote()], never in place.
#'
#' @param cells Merged cells for `inThisCompilation`.
#' @param index An `lv_index`.
#' @param compilation Compilation name.
#' @param version Compilation version recorded on an addition.
#' @param dir Source database directory.
#' @param out Staging directory.
#' @param progress Show progress.
#' @return A list of `added`, `removed`, `issues` and `datasets` touched.
#' @export
lv_apply_membership <- function(cells, index, compilation, version = "1_0_0",
                                dir = lv_path("database"), out, progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  cells <- cells[cells$field == "inThisCompilation", , drop = FALSE]
  members <- lv_compilation_timeseries(index, compilation)

  want_in <- cells$tsid[cells$value %in% "TRUE"]
  # Only an explicit FALSE removes. A blank is "no opinion": most cells in a
  # real QC tab are blank, and treating those as removals would empty the
  # compilation on the first run.
  want_out <- cells$tsid[cells$value %in% "FALSE"]
  add <- setdiff(want_in, members)
  drop <- intersect(want_out, members)

  if (!length(add) && !length(drop)) {
    return(list(added = character(), removed = character(),
                issues = lv_issues_empty(), datasets = character()))
  }
  fs::dir_create(out)

  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)
  touched <- unique(stats::na.omit(unname(ts2ds[c(add, drop)])))
  if (progress) {
    cli::cli_alert_info(
      "Membership: {length(add)} addition{?s}, {length(drop)} removal{?s} across {length(touched)} dataset{?s}")
  }

  issues <- lv_issues_empty()
  for (dsn in touched) {
    p <- paths[[dsn]]
    L <- tryCatch(lipdR::readLipd(p), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the dataset.", dataSetName = dsn, path = p))
      next
    }
    a <- intersect(add, names(which(ts2ds == dsn)))
    d <- intersect(drop, names(which(ts2ds == dsn)))
    if (length(a)) {
      r <- lv_add_membership(L, a, compilation, version)
      L <- r$L
      if (nrow(r$issues)) issues <- lv_issues_bind(issues, r$issues)
    }
    if (length(d)) L <- lv_drop_membership(L, d, compilation)
    lipdR::writeLipd(L, path = out, removeNamesFromLists = TRUE)
  }

  list(added = add, removed = drop, issues = issues, datasets = touched)
}

#' Remove compilation membership from the named columns
#' @keywords internal
lv_drop_membership <- function(L, tsids, compilation) {
  for (blk in c("paleoData", "chronData")) {
    if (is.null(L[[blk]])) next
    for (pd in seq_along(L[[blk]])) {
      for (tb in seq_along(L[[blk]][[pd]]$measurementTable)) {
        tab <- L[[blk]][[pd]]$measurementTable[[tb]]
        for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
          col <- tab[[cn]]
          if (!is.list(col) || is.null(col$TSid)) next
          if (!as.character(col$TSid)[1] %in% tsids) next
          ic <- col$inCompilation
          if (!is.list(ic) || !length(ic)) next
          nm <- vapply(ic, function(e) {
            n <- if (is.list(e)) unlist(e[["compilationName"]]) else unlist(e)
            if (length(n)) as.character(n)[1] else NA_character_
          }, character(1))
          keep <- is.na(nm) | nm != compilation
          # Drop the key entirely rather than leaving an empty list, which
          # lipdR would write back as an empty array.
          col$inCompilation <- if (any(keep)) ic[keep] else NULL
          tab[[cn]] <- col
        }
        L[[blk]][[pd]]$measurementTable[[tb]] <- tab
      }
    }
  }
  L
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

  # The membership tab catalogues the *whole database*, not just the members.
  # That is what makes a new dataset visible: a curator flips it to TRUE here,
  # its timeseries appear in the QC tab on the next run, and they are then
  # admitted one by one via inThisCompilation.
  considered <- unique(index$timeseries$dataSetName[index$timeseries$TSid %in% cells$tsid])
  all_ds <- index$datasets$fileDataSetName
  members <- tibble::tibble(
    dsn = all_ds,
    dsid = index$datasets$datasetId,
    inComp = ifelse(all_ds %in% considered, "TRUE", "FALSE"),
    instructions = LV_MEMBERSHIP_INSTRUCTIONS)
  members <- members[order(members$dsn), , drop = FALSE]

  list(QC = qc, datasetsInCompilation = members)
}

# Kept verbatim from the existing sheets: compilation leads read this text, and
# it is also the specification -- FALSE excludes, anything else considers.
LV_MEMBERSHIP_INSTRUCTIONS <- paste(
  "Any datasets marked as FALSE will not be considered for the update,",
  "NA or TRUE will be considered.")
