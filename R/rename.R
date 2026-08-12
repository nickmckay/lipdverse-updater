#' Renaming a dataset from the QC sheet
#'
#' `dataSetName` is a merged field, so a curator can correct it on the QC sheet
#' and the merge honours it. Applying that is a **rename**: the file's name comes
#' from its metadata, so writing the corrected name produces a file under the new
#' name and leaves the old one behind. Until this existed the pipeline did
#' exactly that and called it done, so a promote added a duplicate rather than
#' renaming, and only the collateral-change invariant noticed -- because the new
#' name fell outside the compilation. Had it been on the membership tab the
#' duplicate would have gone through silently.
#'
#' The identity is `datasetId`, not the filename. A rename keeps the same
#' `datasetId`, which is what distinguishes it from a new dataset that happens to
#' have taken the old one's place.
#'
#' @section What a rename touches:
#' \describe{
#'   \item{the staged file}{written under the new name by the ordinary apply}
#'   \item{the old file}{declared as a deletion so [lv_promote()] moves it to
#'     `.trash` rather than leaving a duplicate}
#'   \item{the collateral check}{a declared rename is expected outside the
#'     compilation's dataset list; an undeclared one still stops the run}
#'   \item{the membership tab}{`datasetsInCompilation` names datasets by name, so
#'     the old entry matches nothing after a rename and the dataset silently
#'     drops out of the considered set on the next run}
#' }
#'
#' @name renames
NULL

#' Renames implied by a set of merged cells
#'
#' @param cells Merged cells about to be applied.
#' @param index An `lv_index`.
#' @param registry Field registry.
#' @return A tibble of `dataSetName`, `new_name`, `datasetId`, `file_old`,
#'   `file_new` and `n_timeseries`, with an `issue` column naming anything that
#'   makes the rename unsafe.
#' @export
lv_planned_renames <- function(cells, index, registry = lv_qc_fields()) {
  empty <- tibble::tibble(dataSetName = character(), new_name = character(),
                          datasetId = character(), file_old = character(),
                          file_new = character(), n_timeseries = integer(),
                          issue = character())
  if (nrow(cells) == 0) return(empty)
  x <- cells[cells$field %in% "dataSetName" & !is.na(cells$value) & nzchar(cells$value),
             , drop = FALSE]
  if (nrow(x) == 0) return(empty)

  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  x$dataSetName <- unname(ts2ds[x$tsid])
  x <- x[!is.na(x$dataSetName), , drop = FALSE]
  # Only where the name actually changes. Compared in one normal form, because a
  # filename is decomposed and a sheet value is composed, and the two spellings
  # of an accented name would otherwise read as a rename on every run forever.
  x <- x[lv_nfc(trimws(x$value)) != lv_nfc(x$dataSetName), , drop = FALSE]
  if (nrow(x) == 0) return(empty)

  by <- split(x, x$dataSetName)
  out <- lapply(names(by), function(d) {
    v <- unique(trimws(by[[d]]$value))
    dsid <- unique(stats::na.omit(index$datasets$datasetId[
      lv_nfc(index$datasets$fileDataSetName) == lv_nfc(d)]))
    tibble::tibble(
      dataSetName = d,
      new_name = v[1],
      datasetId = if (length(dsid)) dsid[1] else NA_character_,
      file_old = paste0(d, ".lpd"),
      file_new = paste0(v[1], ".lpd"),
      n_timeseries = nrow(by[[d]]),
      # One dataset, two proposed names, is a sheet that disagrees with itself:
      # the rows of a dataset must agree before anything is renamed.
      issue = if (length(v) > 1) {
        paste0("rows disagree on the new name: ", paste(v, collapse = " | "))
      } else NA_character_)
  })
  out <- dplyr::bind_rows(out)

  # A rename onto a name the database already holds would overwrite a different
  # dataset. Checked against the whole database rather than the compilation,
  # since the collision need not be a member.
  taken <- lv_nfc(index$datasets$fileDataSetName)
  collide <- lv_nfc(out$new_name) %in% taken
  out$issue[collide & is.na(out$issue)] <- "a dataset of that name already exists"

  # A name that cannot be a filename cannot be a dataset name.
  bad <- grepl("[/\\\\]", out$new_name) | !nzchar(out$new_name)
  out$issue[bad & is.na(out$issue)] <- "not usable as a filename"

  out
}

#' Report renames as issues
#'
#' @param renames From [lv_planned_renames()].
#' @return An `lv_issues` tibble; `error` for anything unsafe, `info` otherwise.
#' @export
lv_rename_issues <- function(renames) {
  if (nrow(renames) == 0) return(lv_issues_empty())
  lv_issues(
    check = ifelse(is.na(renames$issue), "dataset_renamed", "dataset_rename_unsafe"),
    severity = ifelse(is.na(renames$issue), "info", "error"),
    message = ifelse(is.na(renames$issue),
                     paste0("Renamed to ", renames$new_name, " by the QC sheet."),
                     renames$issue),
    dataSetName = renames$dataSetName,
    datasetId = renames$datasetId,
    field = "dataSetName",
    value = renames$new_name)
}

#' Point the membership tab at a renamed dataset
#'
#' `datasetsInCompilation` names datasets by name, so after a rename the old
#' entry matches nothing: the dataset drops out of the considered set and its
#' timeseries vanish from the QC tab on the following run, which looks like the
#' compilation losing a dataset for no reason.
#'
#' Patches the cell in place, addressed by matching the old name, so the tab's
#' other rows and formatting are untouched.
#'
#' @param renames From [lv_planned_renames()]; unsafe rows are ignored.
#' @param cfg An `lv_config`.
#' @param backend A sheet backend.
#' @param dry_run Report without writing.
#' @return A tibble of what was (or would be) patched.
#' @export
lv_rename_in_membership <- function(renames, cfg, backend, dry_run = TRUE) {
  done <- tibble::tibble(dataSetName = character(), new_name = character(),
                         row = integer(), written = logical())
  renames <- renames[is.na(renames$issue), , drop = FALSE]
  if (nrow(renames) == 0) return(done)

  tab <- tryCatch(sheet_read(backend, cfg$qc_sheet_id, cfg$qc_tabs$datasets),
                  error = function(e) NULL)
  if (is.null(tab)) return(done)
  nm <- intersect(c("dsn", "dataSetName"), names(tab))[1]
  if (is.na(nm)) return(done)

  row <- match(lv_nfc(renames$dataSetName), lv_nfc(tab[[nm]]))
  keep <- !is.na(row)
  if (!any(keep)) return(done)
  col <- match(nm, names(tab))
  addr <- paste0(lv_col_letter(rep(col, sum(keep))), row[keep] + 1L)  # +1 for the header
  if (!dry_run) sheet_write_cells(backend, cfg$qc_sheet_id, cfg$qc_tabs$datasets,
                                  addr, renames$new_name[keep])
  tibble::tibble(dataSetName = renames$dataSetName[keep],
                 new_name = renames$new_name[keep],
                 row = row[keep], written = !dry_run)
}
