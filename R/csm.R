#' Compilation-specific metadata in the merge
#'
#' A csm field is a judgement a compilation makes about a timeseries, stored
#' inside the `inCompilation` entry that already records membership:
#'
#' ```json
#' {"compilationName": "hydroclimate2k",
#'  "compilationVersion": ["1_0_0"],
#'  "csm": {"QCCertification": "NPM"}}
#' ```
#'
#' The registry names both halves: `csm_compilation` says whose it is and
#' `csm_field` says what it is called inside `csm`. The QC sheet and the store
#' keep using the field's `qc_name`, so a curator sees one column and the store
#' keeps one cell per timeseries, exactly as for a merged field.
#'
#' Until this file existed, csm was declared everywhere and merged nowhere: the
#' registry named the fields and [lv_compilation_sheet()] put the columns on a
#' new sheet, but [qc_frame()] read only roles `merged` and `key` and
#' [qc_merge()] dropped role `csm` outright. A curator's certification sat on the
#' sheet and reached no file. [lv_csm_frame()] and [lv_apply_csm()] are the two
#' missing ends; the merge itself needed only to stop discarding the role.
#'
#' @section Scoping:
#' Every end is scoped to one compilation, so a cross-compilation csm conflict
#' cannot arise rather than being resolved. A QC sheet can carry a column
#' belonging to another compilation -- sheets get copied from one another, and
#' the registry holds a synonym for iso2k's certification column -- and without
#' the scope a curator editing hydroclimate2k's tab would write into iso2k's
#' `csm`, which no iso2k lead would ever see.
#'
#' The exclusion happens at the sheet, in [lv_csm_scope()], before the merge:
#' a foreign cell that reaches the plan also reaches [qc_plan_state()], and from
#' there the store and the sheet push, so refusing it at write time is too late.
#' [lv_apply_csm()] refuses it as well, as the last check before a file changes.
#'
#' Only the sheet is scoped, never the store. A cell the store holds and the
#' resolved state does not becomes a tombstone in [qc_diff_to_events()], so
#' filtering the baseline would record deletions of another compilation's values
#' rather than ignoring them.
#'
#' @section What is not read here:
#' Only `csm` is read. The pre-migration flat keys (`paleoData_QCCertification`
#' and the compilation-private spellings beside it) are deliberately not a
#' fallback: those values were replicated into every member compilation by the
#' migration, so reading them back would let one compilation's certification
#' arrive as another's. Where a compilation's csm needs seeding, it is seeded
#' from that compilation's own QC sheet.
#'
#' @name csm
NULL

#' The csm fields belonging to one compilation
#'
#' @param compilation Compilation name.
#' @param registry Field registry.
#' @return A tibble of `qc_name` (what the sheet and store call it) and
#'   `csm_field` (what it is called inside `csm`).
#' @export
lv_csm_fields <- function(compilation, registry = lv_qc_fields()) {
  x <- registry[registry$role %in% "csm" & !is.na(registry$csm_compilation), , drop = FALSE]
  if (!nrow(x)) return(tibble::tibble(qc_name = character(), csm_field = character()))
  # A field can belong to several compilations -- five coral fields are claimed
  # by both CoralHydro2k and GBRCD -- so csm_compilation is ";"-separated and
  # only the matching part counts.
  hit <- vapply(strsplit(x$csm_compilation, ";", fixed = TRUE),
                function(v) compilation %in% v, logical(1))
  x <- x[hit, , drop = FALSE]
  tibble::tibble(qc_name = x$qc_name, csm_field = x$csm_field)
}

#' Keep only the csm a compilation may speak for
#'
#' Splits a sheet's cells into those this compilation owns and those belonging to
#' another compilation's `csm`. Merged fields and everything else pass through
#' untouched; only role `csm` is scoped.
#'
#' Apply this to the sheet before merging, never to the store baseline: see the
#' scoping note in [csm].
#'
#' @param cells A cell table.
#' @param compilation Compilation name.
#' @param registry Field registry.
#' @return A list of `cells` to merge and `foreign` cells held back.
#' @export
lv_csm_scope <- function(cells, compilation, registry = lv_qc_fields()) {
  if (nrow(cells) == 0) return(list(cells = cells, foreign = cells))
  role <- lv_field_rule(cells$field, registry)$role
  mine <- lv_csm_fields(compilation, registry)$qc_name
  foreign <- role %in% "csm" & !cells$field %in% mine
  list(cells = cells[!foreign, , drop = FALSE],
       foreign = cells[foreign, , drop = FALSE])
}

#' The file-side view of a compilation's csm
#'
#' Reads `inCompilation[[compilation]]$csm` from each file and emits the same
#' long cell table [qc_frame()] and [qc_sheet_pull()] produce, so csm merges by
#' the ordinary three-way rules.
#'
#' Only populated values are emitted. An absent cell means "unchanged", which is
#' what keeps a file that has never carried a csm value from reading as a
#' deletion of one the curator just typed.
#'
#' @param dir Directory of `.lpd` files, or an `lv_scan`.
#' @param compilation Compilation name.
#' @param registry Field registry.
#' @param datasets Optional dataset names to restrict to.
#' @param tsids Optional TSids to restrict to.
#' @param progress Show progress.
#' @return A cell table.
#' @export
lv_csm_frame <- function(dir = lv_path("database"), compilation,
                         registry = lv_qc_fields(), datasets = NULL,
                         tsids = NULL, progress = TRUE) {
  fields <- lv_csm_fields(compilation, registry)
  if (!nrow(fields)) return(qc_cells_empty())

  paths <- if (inherits(dir, "lv_scan")) dir$files$path else
    fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (!is.null(datasets)) {
    paths <- paths[sub("\\.lpd$", "", basename(paths)) %in% datasets]
  }
  if (!length(paths)) return(qc_cells_empty())

  # Keyed by the name inside `csm`; the cell carries the registry's qc_name, so
  # the sheet and the store keep one namespace.
  to_qc <- stats::setNames(fields$qc_name, fields$csm_field)

  if (progress) {
    cli::cli_alert_info("Reading {compilation} csm from {length(paths)} file{?s}")
  }
  parts <- lapply(paths, function(p) lv_csm_frame_one(p, compilation, to_qc))
  out <- purrr::list_rbind(parts)
  if (nrow(out) == 0) return(qc_cells_empty())
  if (!is.null(tsids)) out <- out[out$tsid %in% tsids, , drop = FALSE]
  out[!duplicated(paste(out$tsid, out$field)), , drop = FALSE]
}

lv_csm_frame_one <- function(path, compilation, to_qc) {
  m <- tryCatch({
    nms <- utils::unzip(path, list = TRUE)$Name
    j <- grep("\\.jsonld$", nms, value = TRUE)
    if (!length(j)) stop("no jsonld")
    con <- unz(path, j[1]); on.exit(close(con), add = TRUE)
    jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                       simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (is.null(m)) return(NULL)

  dsid <- as_chr1(m$datasetId) %||% NA_character_
  rows <- list()
  for (blk in c("paleoData", "chronData")) {
    for (pd in m[[blk]]) {
      tables <- c(pd$measurementTable,
                  unlist(lapply(pd$model, function(md) c(md$summaryTable, md$ensembleTable)),
                         recursive = FALSE))
      for (tb in tables) {
        if (!is.list(tb)) next
        cols <- if (!is.null(tb$columns)) tb$columns else tb
        for (col in cols) {
          if (!is.list(col) || is.null(col$TSid)) next
          tsid <- as_chr1(col$TSid)
          if (is.null(tsid)) next
          csm <- lv_csm_of(col$inCompilation, compilation)
          if (!length(csm)) next
          for (k in names(csm)) {
            qc <- if (k %in% names(to_qc)) to_qc[[k]] else NULL
            if (is.null(qc)) next
            v <- lv_scalar_value(csm[[k]])
            if (length(v) != 1 || is.na(v) || !nzchar(v)) next
            rows[[length(rows) + 1L]] <- tibble::tibble(
              tsid = tsid, field = qc, value = v, present = TRUE,
              dataset_id = dsid, updated_at = NA_character_,
              source = "lipd", actor = NA_character_)
          }
        }
      }
    }
  }
  if (!length(rows)) return(NULL)
  purrr::list_rbind(rows)
}

# The `csm` list of a column's entry for one compilation, or NULL.
lv_csm_of <- function(ic, compilation) {
  i <- lv_csm_entry_index(ic, compilation)
  if (is.na(i)) return(NULL)
  csm <- ic[[i]]$csm
  if (is.list(csm)) csm else NULL
}

# Which entry of an inCompilation list is this compilation's. Matched by name
# rather than by position: 11 of 19 compilations occupy more than one index
# across the database, and hydroclimate2k occupies all of 1 to 5.
lv_csm_entry_index <- function(ic, compilation) {
  if (!is.list(ic) || !length(ic)) return(NA_integer_)
  nm <- vapply(ic, function(e) {
    n <- if (is.list(e)) unlist(e[["compilationName"]]) else unlist(e)
    if (length(n)) as.character(n)[1] else NA_character_
  }, character(1))
  hit <- which(!is.na(nm) & nm == compilation)
  if (!length(hit)) NA_integer_ else hit[1]
}

#' Apply merged csm cells to the files
#'
#' Writes each value into the compilation's own `inCompilation` entry, on the
#' column the TSid names. Written to staging for [lv_promote()], never in place.
#'
#' A cell whose column carries no entry for this compilation is not applied: csm
#' is metadata *about a membership*, so without the membership there is nowhere
#' for it to live. Run [lv_apply_membership()] first and point `index` at its
#' staging output, which is what the runner does.
#'
#' @param cells Merged cells; anything that is not a csm field of `compilation`
#'   is reported and skipped.
#' @param index An `lv_index`.
#' @param compilation Compilation name.
#' @param dir Source database directory.
#' @param out Staging directory.
#' @param registry Field registry.
#' @param progress Show progress.
#' @return A list of `n` applied, `issues` and `datasets` touched.
#' @export
lv_apply_csm <- function(cells, index, compilation, dir = lv_path("database"),
                         out, registry = lv_qc_fields(), progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  issues <- lv_issues_empty()
  empty <- list(n = 0L, issues = issues, datasets = character())
  if (nrow(cells) == 0) return(empty)

  fields <- lv_csm_fields(compilation, registry)
  rule <- lv_field_rule(cells$field, registry)
  # A sheet can hold another compilation's column, and writing it would put this
  # compilation's curator into a namespace they cannot see. Reported, not applied.
  foreign <- cells[rule$role %in% "csm" & !cells$field %in% fields$qc_name, , drop = FALSE]
  if (nrow(foreign)) {
    issues <- lv_issues_bind(issues, lv_issues(
      check = "csm_other_compilation", severity = "warn",
      message = paste0("Field is compilation-specific metadata of another compilation, not ",
                       compilation, "; not applied."),
      TSid = foreign$tsid, field = foreign$field, value = foreign$value))
  }
  cells <- cells[cells$field %in% fields$qc_name, , drop = FALSE]
  if (nrow(cells) == 0) return(list(n = 0L, issues = issues, datasets = character()))

  cells$csm_field <- fields$csm_field[match(cells$field, fields$qc_name)]

  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  cells$dataSetName <- unname(ts2ds[cells$tsid])
  orphan <- cells[is.na(cells$dataSetName), , drop = FALSE]
  cells <- cells[!is.na(cells$dataSetName), , drop = FALSE]
  if (nrow(orphan)) {
    issues <- lv_issues_bind(issues, lv_issues(
      check = "tsid_not_in_database", severity = "warn",
      message = "TSid is not in any file; value not applied.",
      TSid = orphan$tsid, field = orphan$field, value = orphan$value))
  }
  if (nrow(cells) == 0) return(list(n = 0L, issues = issues, datasets = character()))

  fs::dir_create(out)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)
  by_ds <- split(cells, cells$dataSetName)
  if (progress) {
    cli::cli_alert_info("Applying {nrow(cells)} {compilation} csm cell{?s} across {length(by_ds)} dataset{?s}")
  }

  applied <- 0L
  touched <- character()
  for (dsn in names(by_ds)) {
    p <- paths[[dsn]]
    if (is.null(p) || is.na(p)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "dataset_not_found", severity = "error",
        message = "Dataset has csm cells but no file.", dataSetName = dsn))
      next
    }
    L <- tryCatch(lipdR::readLipd(p), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the dataset.", dataSetName = dsn, path = p))
      next
    }
    res <- lv_apply_csm_to_lipd(L, by_ds[[dsn]], compilation)
    if (res$n) {
      lipdR::writeLipd(res$L, path = out, removeNamesFromLists = TRUE)
      applied <- applied + res$n
      touched <- c(touched, dsn)
    }
    if (nrow(res$issues)) issues <- lv_issues_bind(issues, res$issues)
  }
  list(n = applied, issues = issues, datasets = touched)
}

#' Apply one dataset's csm cells to its LiPD object
#' @param L A LiPD object.
#' @param cells That dataset's csm cells, carrying `csm_field`.
#' @param compilation Compilation name.
#' @return A list of `L`, `n` applied and `issues`.
#' @keywords internal
lv_apply_csm_to_lipd <- function(L, cells, compilation) {
  issues <- lv_issues_empty()
  dsn <- L$dataSetName %||% NA_character_
  want <- split(cells, cells$tsid)
  n <- 0L

  for (blk in c("paleoData", "chronData")) {
    if (is.null(L[[blk]])) next
    for (pd in seq_along(L[[blk]])) {
      for (tb in seq_along(L[[blk]][[pd]]$measurementTable)) {
        tab <- L[[blk]][[pd]]$measurementTable[[tb]]
        for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
          col <- tab[[cn]]
          if (!is.list(col) || is.null(col$TSid)) next
          tsid <- as.character(col$TSid)[1]
          if (is.null(want[[tsid]])) next
          rows <- want[[tsid]]
          want[[tsid]] <- NULL

          i <- lv_csm_entry_index(col$inCompilation, compilation)
          if (is.na(i)) {
            issues <- lv_issues_bind(issues, lv_issues(
              check = "csm_without_membership", severity = "warn",
              message = paste0("Column is not in ", compilation,
                               "; compilation-specific metadata not applied."),
              dataSetName = dsn, TSid = tsid, field = rows$field, value = rows$value))
            next
          }
          entry <- col$inCompilation[[i]]
          if (!is.list(entry$csm)) entry$csm <- list()
          for (r in seq_len(nrow(rows))) {
            # NULL removes the key: a cleared cell must leave no empty string
            # behind, or the next read sees a value the curator deleted.
            entry$csm[[rows$csm_field[r]]] <- lv_coerce_value(rows$value[r])
            n <- n + 1L
          }
          if (!length(entry$csm)) entry$csm <- NULL
          col$inCompilation[[i]] <- entry
          tab[[cn]] <- col
        }
        L[[blk]][[pd]]$measurementTable[[tb]] <- tab
      }
    }
  }
  if (length(want)) {
    left <- dplyr::bind_rows(want)
    issues <- lv_issues_bind(issues, lv_issues(
      check = "tsid_not_in_dataset", severity = "warn",
      message = "TSid was expected in this dataset but not found.",
      dataSetName = dsn, TSid = left$tsid, field = left$field))
  }
  list(L = L, n = n, issues = issues)
}
