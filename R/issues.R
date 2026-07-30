#' Issue accumulator
#'
#' Vectorized work in this package never uses `try()`-and-continue. Problems
#' accumulate in an `lv_issues` tibble and are reported together, so a run that
#' finds 400 bad records reports 400 of them rather than aborting on the first
#' or silently swallowing all of them.
#'
#' Severity drives control flow: `error` rows abort the run, but only *after*
#' the full report has been written.
#'
#' @param ... Columns for the issue rows.
#' @return An `lv_issues` tibble.
#' @export
lv_issues <- function(...) {
  x <- tibble::tibble(...)
  cols <- c(check = NA_character_, severity = NA_character_, message = NA_character_,
            datasetId = NA_character_, dataSetName = NA_character_, TSid = NA_character_,
            field = NA_character_, value = NA_character_, path = NA_character_)
  for (nm in names(cols)) if (!nm %in% names(x)) x[[nm]] <- cols[[nm]]
  x <- x[, c(names(cols), setdiff(names(x), names(cols))), drop = FALSE]
  validate_lv_issues(x)
}

validate_lv_issues <- function(x) {
  bad <- setdiff(stats::na.omit(unique(x$severity)), c("info", "warn", "error"))
  if (length(bad)) {
    cli::cli_abort("Invalid severity {.val {bad}}; must be one of info, warn, error.")
  }
  structure(x, class = unique(c("lv_issues", class(x))))
}

#' @export
lv_issues_empty <- function() lv_issues(check = character())

#' Combine issue tables
#' @param ... `lv_issues` tibbles.
#' @export
lv_issues_bind <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  if (!length(parts)) return(lv_issues_empty())
  validate_lv_issues(dplyr::bind_rows(parts))
}

#' @param x An `lv_issues` tibble.
#' @param severity Severity to count.
#' @export
lv_n_issues <- function(x, severity = c("info", "warn", "error")) {
  sum(x$severity %in% severity)
}

#' @export
print.lv_issues <- function(x, ...) {
  if (nrow(x) == 0) {
    cli::cli_alert_success("No issues.")
    return(invisible(x))
  }
  counts <- table(factor(x$severity, levels = c("error", "warn", "info")))
  cli::cli_h3("{nrow(x)} issue{?s}")
  for (s in names(counts)) {
    if (counts[[s]] == 0) next
    fn <- switch(s, error = cli::cli_alert_danger,
                    warn  = cli::cli_alert_warning,
                    cli::cli_alert_info)
    fn("{counts[[s]]} {s}")
  }
  by_check <- dplyr::count(x, .data$severity, .data$check, name = "n")
  by_check <- dplyr::arrange(by_check, factor(.data$severity, levels = c("error", "warn", "info")), dplyr::desc(.data$n))
  print(as.data.frame(by_check), row.names = FALSE)
  invisible(x)
}

#' Abort if any error-severity issues are present
#'
#' Writes the report first so the operator always has the full picture, even
#' when the run stops.
#'
#' @param issues An `lv_issues` tibble.
#' @param path Optional path to write the full report to before aborting.
#' @param what Label for the error message.
#' @export
lv_issues_check <- function(issues, path = NULL, what = "validation") {
  n <- lv_n_issues(issues, "error")
  if (n == 0) return(invisible(issues))
  if (!is.null(path)) {
    fs::dir_create(fs::path_dir(path))
    readr::write_csv(issues, path, na = "")
  }
  cli::cli_abort(c(
    "{what} found {n} error{?s}.",
    i = if (!is.null(path)) "Full report: {.path {path}}" else NULL
  ), class = "lv_error_issues")
}
