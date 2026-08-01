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

#' Are two stored values the same value?
#'
#' Compares text, except where both sides parse as numbers, in which case they
#' are compared at the coarser of the two precisions. The sheet and the files
#' round differently -- 2001.54167 against 2001.5417 is the same measurement
#' written twice -- and treating those as changes would churn the store and the
#' sheet on every run while burying real edits.
#'
#' Comparing at the coarser precision, rather than with a fixed tolerance, keeps
#' a genuine change visible: 2001.5417 against 2001.5418 still differs.
#'
#' @param x,y Character vectors.
#' @param numeric_ok Allow numeric comparison; `FALSE` for identifiers, where
#'   "007" and "7" are different strings and must stay so.
#' @return Logical vector.
#' @keywords internal
values_equal <- function(x, y, numeric_ok = TRUE) {
  both_na <- is.na(x) & is.na(y)
  # Trim before comparing. Google Sheets silently drops leading and trailing
  # whitespace, so a file value ending in a newline comes back from the sheet
  # without it. Comparing raw would report that as a curator edit on every run
  # forever, and on a curator-owned field would write the trimmed value back --
  # the sheet quietly editing the database because of what it cannot represent.
  same_text <- !is.na(x) & !is.na(y) & trimws(x) == trimws(y)
  out <- both_na | same_text
  numeric_ok <- numeric_ok %in% TRUE
  if (!any(numeric_ok)) return(out)

  cand <- !out & !is.na(x) & !is.na(y) & numeric_ok
  if (!any(cand)) return(out)

  nx <- suppressWarnings(as.numeric(x[cand]))
  ny <- suppressWarnings(as.numeric(y[cand]))
  ok <- !is.na(nx) & !is.na(ny) & is.finite(nx) & is.finite(ny)
  if (!any(ok)) return(out)

  # Digits after the decimal point: strip everything up to and including the
  # first dot, leaving "" (and so 0) when there is no dot at all.
  decimals <- function(v) nchar(sub("^[^.]*\\.?", "", v))
  dp <- pmin(decimals(x[cand]), decimals(y[cand]))
  same_num <- rep(FALSE, sum(cand))
  same_num[ok] <- round(nx[ok], dp[ok]) == round(ny[ok], dp[ok])
  out[cand] <- same_num
  out
}

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
  # Track which sources actually carry each cell. A cell absent from a table is
  # not the same as a cell present and empty, and only the latter can mean a
  # curator cleared it.
  b0 <- base[, c("tsid", "field", "value")]
  names(b0)[3] <- "base_value"
  s0 <- sheet[, c("tsid", "field", "value")]
  names(s0)[3] <- "sheet_value"
  s0$in_sheet <- TRUE
  f0 <- frame[, c("tsid", "field", "value", "dataset_id")]
  names(f0)[3] <- "file_value"
  f0$in_file <- TRUE

  cells <- dplyr::full_join(dplyr::full_join(b0, s0, by = c("tsid", "field")),
                            f0, by = c("tsid", "field"))
  cells$in_sheet <- cells$in_sheet %in% TRUE
  cells$in_file  <- cells$in_file %in% TRUE

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
  # A blank sheet cell only clears where the sheet actually carries that cell,
  # the curator owns the field, and the registry says it may be cleared. Without
  # the in_sheet test, a cell the sheet simply does not have would read as a
  # deletion -- the same conflation that made daff destroy curated values.
  sheet_clears <- explicit_clear |
    (cells$in_sheet & blank(s) & cells$nullable & !blank(b))
  # Otherwise a blank from either side means "unchanged", so treat it as base.
  s_eff <- ifelse(sheet_clears, NA_character_, ifelse(blank(s), b, s))
  # A blank or absent file value never deletes; it means "unchanged".
  f_eff <- ifelse(blank(f), b, f)

  # %in% rather than ==: role is NA for fields absent from the registry.
  is_key <- cells$role %in% "key"
  eq <- function(x, y) values_equal(x, y, numeric_ok = !is_key)
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
