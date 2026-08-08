
#' Drop quarantined cells from a write set
#'
#' A curator can type anything into a cell, and one value a file cannot hold
#' would otherwise abort a several-hundred-file promote. This removes exactly the
#' cells [lv_validate_values()] rejected and leaves everything else.
#'
#' A no-op when nothing was rejected, which is the normal case.
#'
#' @param cells A cell tibble, keyed on `tsid` and `field`.
#' @param issues From [lv_validate_values()], keyed on `TSid` and `field`.
#' @return `cells`, without the rejected ones.
#' @export
lv_drop_cells <- function(cells, issues) {
  if (is.null(issues) || !nrow(issues)) return(cells)
  key <- paste(issues$TSid, issues$field, sep = "\r")
  cells[!paste(cells$tsid, cells$field, sep = "\r") %in% key, , drop = FALSE]
}
