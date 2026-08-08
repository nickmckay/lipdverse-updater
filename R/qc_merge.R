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

  # Compare the numbers, not their spelling. This exists so "1.0" and "1", or
  # "1.50" and "1.5", are one value written two ways rather than an edit on every
  # run forever.
  #
  # It used to round both to the LESSER of the two precisions first, which made
  # any difference beyond the shorter spelling invisible: 60.07 and 60.1 compared
  # equal, so a coordinate rounded in one place could never be detected as
  # differing from the unrounded one, and whichever side happened to be written
  # last silently won. That is how four datasets lost their coordinate precision.
  #
  # The tolerance is for float representation only -- a value read back as
  # 60.070000000000004 is the same number -- and is far tighter than any
  # difference a person would type.
  decimals <- function(v) nchar(sub("^[^.]*\\.?", "", v))
  dp <- pmin(decimals(x[cand]), decimals(y[cand]))

  same_num <- rep(FALSE, sum(cand))
  # Exactly equal as numbers: "1.0" and "1", "1e3" and "1000".
  same_num[ok] <- nx[ok] == ny[ok]

  # Otherwise compare at the precision the two have in common, which is what
  # makes a value the sheet has rounded not read as an edit. But only when that
  # shared precision is meaningful. It used to apply at any precision, so a value
  # carrying one decimal swallowed a finer one -- 60.07 and 60.1 compared equal,
  # and four datasets lost coordinate precision because nothing could see the
  # difference. Two decimals is the line: enough for a sheet-rounded 1770.79167
  # to still match 1770.7917, not enough for 60.1 to claim 60.07.
  round_ok <- ok & !same_num & dp >= LV_MIN_SHARED_DECIMALS
  if (any(round_ok)) {
    same_num[round_ok] <- round(nx[round_ok], dp[round_ok]) ==
                          round(ny[round_ok], dp[round_ok])
  }
  out[cand] <- same_num
  out
}

# Below this many shared decimal places, two numbers that are not exactly equal
# are treated as different. See values_equal().
LV_MIN_SHARED_DECIMALS <- 2L

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

  # Only fields that participate in the merge. `membership` is merged by the
  # same rules as a curator-owned field -- it just gets applied to the
  # inCompilation structure rather than written as a key. Everything else (csm,
  # synonym, control, unused, delete) is handled elsewhere or not at all.
  cells <- cells[cells$role %in% c("merged", "membership", "key") | !cells$known_field,
                 , drop = FALSE]

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

  # A machine-owned field is derived, and the sheet only displays it. The
  # ownership rule below is consulted just on *divergence*, so it never fired
  # for these once a baseline matched the files: the sheet then differed alone
  # and won unopposed. On hydroclimate2k that was 6,093 cells -- minYear,
  # maxYear, createdBy, lipdverseLink -- where a sheet last written years ago
  # would have overwritten values recomputed from the data since.
  #
  # The sheet never writes a machine field. It can still be reported as stale,
  # and the push puts the file's value back into the sheet.
  s_ch <- s_ch & !(cells$ownership %in% "machine")

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

#' Check resolved values against what the registry says they are
#'
#' A curator can type anything into a sheet cell. Without this, a value the
#' files cannot legally hold reaches [lv_apply_qc()], gets written into staging,
#' and is caught only by the writer's verification -- which aborts the whole
#' run. On hydroclimate2k one cell held a URL pasted into the latitude column,
#' and it would have blocked a 413-file promote.
#'
#' Reporting per cell instead lets the run continue with the other 2,738
#' changes and hands the curator a specific TSid and field to fix.
#'
#' @param cells Resolved cells (`tsid`, `field`, `value`).
#' @param registry Field registry.
#' @return An `lv_issues` tibble.
#' @export
lv_validate_values <- function(cells, registry = lv_qc_fields()) {
  if (nrow(cells) == 0) return(lv_issues_empty())
  i <- match(cells$field, registry$qc_name)
  type <- registry$type[i]
  v <- cells$value
  have <- !is.na(v) & nzchar(v)

  num <- have & type %in% "numeric"
  bad_num <- num & is.na(suppressWarnings(as.numeric(v)))

  # Range checks for the coordinate fields, which have a defined domain and are
  # the ones a mis-pasted cell corrupts most visibly.
  n <- suppressWarnings(as.numeric(v))
  bad_rng <- rep(FALSE, length(v))
  lat <- have & cells$field == "geo_latitude" & !is.na(n)
  lon <- have & cells$field == "geo_longitude" & !is.na(n)
  bad_rng[lat] <- abs(n[lat]) > 90
  bad_rng[lon] <- n[lon] < -180 | n[lon] > 360

  bad <- bad_num | bad_rng
  if (!any(bad)) return(lv_issues_empty())

  lv_issues(
    check = ifelse(bad_num[bad], "value_not_numeric", "value_out_of_range"),
    severity = "error",
    message = ifelse(bad_num[bad],
                     "Value is not numeric but the field is.",
                     "Value is outside the field's valid range."),
    TSid = cells$tsid[bad], field = cells$field[bad], value = v[bad],
    datasetId = if ("dataset_id" %in% names(cells)) cells$dataset_id[bad] else NA_character_)
}
