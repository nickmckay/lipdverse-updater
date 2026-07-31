#' Three-way QC merge
#'
#' Merges three views of the same cells:
#'
#' \describe{
#'   \item{`base`}{What the store last recorded for this compilation.}
#'   \item{`sheet`}{What the QC sheet says now.}
#'   \item{`frame`}{What the LiPD files say now.}
#' }
#'
#' Replaces `daff::merge_data()`, which compared whole rows and could not
#' express "absent" separately from "empty". NA arriving from the files read as
#' a deletion and wiped curated values; the surviving workarounds in lipdverseR
#' were an NA-backfill pass, a `sticky_fields` restore, and string-patching of
#' `"((( null ))) TRUE /// FALSE"` conflict markers.
#'
#' @section Rules:
#' Per cell, with base `b`, sheet `s`, file `f`:
#'
#' | condition | result |
#' | --- | --- |
#' | `s == b`, `f == b` | unchanged |
#' | `s != b`, `f == b` | sheet (a curator edit) |
#' | `s == b`, `f != b` | file (a file change) |
#' | `s != b`, `f != b`, `s == f` | converged |
#' | `s != b`, `f != b`, `s != f` | by ownership |
#'
#' On genuine divergence, `ownership` decides: `curator` takes the sheet,
#' `machine` takes the file, `key` is an error, and `shared` is a conflict —
#' base is retained and the disagreement reported rather than guessed.
#'
#' @section The clear rule:
#' **A cell is only cleared when the clear is explicit.** A blank arriving from
#' the files never deletes. A blank from the sheet deletes only where the
#' registry marks the field `nullable_by_curator`, or where the cell holds the
#' sentinel `<<CLEAR>>`. Otherwise blank means "unchanged".
#'
#' This single rule is what makes the NA-as-deletion loss impossible.
#'
#' @name qc_merge_rules
NULL

LV_CLEAR_SENTINEL <- "<<CLEAR>>"

#' Merge policy
#' @param strict Abort on unresolved conflicts.
#' @param clear_sentinel Value a curator types to clear a cell explicitly.
#' @export
qc_merge_policy <- function(strict = TRUE, clear_sentinel = LV_CLEAR_SENTINEL) {
  list(strict = strict, clear_sentinel = clear_sentinel)
}

#' Merge base, sheet and file views of QC state
#'
#' @param base,sheet,frame Cell tables (`tsid`, `field`, `value`, ...).
#' @param registry Field registry from [lv_qc_fields()].
#' @param policy From [qc_merge_policy()].
#' @return A `qc_plan`.
#' @export
qc_merge <- function(base, sheet, frame, registry = lv_qc_fields(),
                     policy = qc_merge_policy()) {
  cells <- dplyr::full_join(
    dplyr::full_join(
      base[, c("tsid", "field", "value")]  |> dplyr::rename(base_value = "value"),
      sheet[, c("tsid", "field", "value")] |> dplyr::rename(sheet_value = "value"),
      by = c("tsid", "field")),
    frame[, c("tsid", "field", "value", "dataset_id")] |> dplyr::rename(file_value = "value"),
    by = c("tsid", "field"))

  rule <- lv_field_rule(cells$field, registry)
  cells$ownership <- rule$ownership
  cells$role <- rule$role
  cells$nullable <- rule$nullable_by_curator %in% TRUE
  cells$known_field <- rule$known

  # Only fields that participate in the merge. Everything else (csm, synonym,
  # control, unused, delete) is handled elsewhere or not at all.
  cells <- cells[cells$role %in% c("merged", "key") | !cells$known_field, , drop = FALSE]

  # With no rows, ifelse() below returns logical(0) and case_when cannot
  # reconcile that with the character branches.
  if (nrow(cells) == 0) {
    cells$resolution <- character()
    cells$sheet_clears <- logical()
    cells$value <- character()
    empty <- cells
    return(structure(list(
      cells = cells, changes = empty, conflicts = empty, errors = empty, unknown = empty,
      summary = list(n_cells = 0L, n_changed = 0L, n_conflicts = 0L, n_errors = 0L,
                     n_cleared = 0L, n_unknown_fields = 0L),
      policy = policy), class = "qc_plan"))
  }

  b <- cells$base_value; s <- cells$sheet_value; f <- cells$file_value

  blank <- function(x) is.na(x) | !nzchar(x)
  explicit_clear <- !is.na(s) & s == policy$clear_sentinel
  # A blank sheet cell only clears where the curator owns the field and the
  # registry says it may be cleared.
  sheet_clears <- explicit_clear | (blank(s) & cells$nullable & !blank(b))
  # Otherwise a blank from either side means "unchanged", so treat it as base.
  s_eff <- ifelse(sheet_clears, NA_character_, ifelse(blank(s), b, s))
  f_eff <- ifelse(blank(f), b, f)

  eq <- function(x, y) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  s_ch <- !eq(s_eff, b)
  f_ch <- !eq(f_eff, b)

  cells$resolution <- dplyr::case_when(
    !s_ch & !f_ch              ~ "unchanged",
    s_ch  & !f_ch              ~ "sheet",
    !s_ch & f_ch               ~ "file",
    eq(s_eff, f_eff)           ~ "converged",
    cells$role == "key"        ~ "error",
    cells$ownership == "curator" ~ "sheet",
    cells$ownership == "machine" ~ "file",
    TRUE                       ~ "conflict"
  )
  cells$sheet_clears <- sheet_clears

  cells$value <- dplyr::case_when(
    cells$resolution %in% c("sheet", "converged") ~ s_eff,
    cells$resolution == "file"                    ~ f_eff,
    TRUE                                          ~ b          # unchanged, conflict, error
  )

  changes <- cells[cells$resolution %in% c("sheet", "file", "converged"), , drop = FALSE]
  conflicts <- cells[cells$resolution == "conflict", , drop = FALSE]
  errors <- cells[cells$resolution == "error", , drop = FALSE]
  unknown <- cells[!cells$known_field, , drop = FALSE]

  structure(list(
    cells = cells,
    changes = changes,
    conflicts = conflicts,
    errors = errors,
    unknown = unknown,
    summary = list(
      n_cells = nrow(cells),
      n_changed = nrow(changes),
      n_conflicts = nrow(conflicts),
      n_errors = nrow(errors),
      n_cleared = sum(cells$sheet_clears, na.rm = TRUE),
      n_unknown_fields = dplyr::n_distinct(unknown$field)
    ),
    policy = policy
  ), class = "qc_plan")
}

#' The state a plan resolves to
#' @param plan A `qc_plan`.
#' @return A cell table.
#' @export
qc_plan_state <- function(plan) {
  x <- plan$cells[!is.na(plan$cells$value) & nzchar(plan$cells$value), , drop = FALSE]
  tibble::tibble(tsid = x$tsid, field = x$field, value = x$value, present = TRUE,
                 dataset_id = x$dataset_id, updated_at = NA_character_,
                 source = NA_character_, actor = NA_character_)
}

#' @export
print.qc_plan <- function(x, ...) {
  s <- x$summary
  cli::cli_h3("qc_plan")
  cli::cli_bullets(c(
    "*" = "{s$n_cells} cell{?s} considered",
    "*" = "{s$n_changed} change{?s} ({sum(x$changes$resolution == 'sheet')} from the sheet, {sum(x$changes$resolution == 'file')} from files, {sum(x$changes$resolution == 'converged')} converged)",
    if (s$n_cleared > 0) "!" = "{s$n_cleared} explicit clear{?s}",
    if (s$n_conflicts > 0) "!" = "{s$n_conflicts} conflict{?s} (base retained)",
    if (s$n_errors > 0) "x" = "{s$n_errors} error{?s} on key fields",
    if (s$n_unknown_fields > 0) "!" = "{s$n_unknown_fields} field{?s} not in the registry"
  ))
  if (nrow(x$conflicts)) {
    cli::cli_h3("conflicts by field")
    print(as.data.frame(dplyr::count(x$conflicts, .data$field, name = "n")), row.names = FALSE)
  }
  invisible(x)
}

#' Abort if a plan is not safe to apply
#' @param plan A `qc_plan`.
#' @param path Optional path for the conflict report.
#' @export
qc_plan_check <- function(plan, path = NULL) {
  if (!is.null(path) && (nrow(plan$conflicts) || nrow(plan$errors))) {
    fs::dir_create(fs::path_dir(path))
    readr::write_csv(dplyr::bind_rows(plan$conflicts, plan$errors), path, na = "")
  }
  if (nrow(plan$errors)) {
    cli::cli_abort("{nrow(plan$errors)} identifier conflict{?s}: a key field disagrees between sources.",
                   class = "lv_error_conflict")
  }
  if (isTRUE(plan$policy$strict) && nrow(plan$conflicts)) {
    cli::cli_abort(c("{nrow(plan$conflicts)} unresolved conflict{?s}.",
                     i = if (!is.null(path)) "Report: {.path {path}}" else NULL),
                   class = "lv_error_conflict")
  }
  invisible(plan)
}
