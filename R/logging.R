#' Generate a run identifier
#'
#' A UTC timestamp plus a random suffix, so run directories sort
#' chronologically to the second and two runs started in the same second do not
#' collide. Within one second the suffix decides the order, which is immaterial
#' for directory listings.
#'
#' @return A run id string.
#' @export
lv_run_id <- function() {
  paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-",
         paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""))
}

#' Directory holding a run's artifacts
#'
#' Every run writes its config, issues, plan and receipt here regardless of
#' outcome, so a failed run is as inspectable as a successful one.
#'
#' @param run_id Run identifier.
#' @param create Create the directory.
#' @return A path.
#' @export
lv_run_dir <- function(run_id, create = TRUE) {
  p <- fs::path(lv_path("state"), "runs", run_id)
  if (create) fs::dir_create(p)
  p
}

#' Log a message to the console and, if one is open, the run log
#'
#' @param ... Message parts, glued.
#' @param level One of info, success, warning, danger.
#' @param run_id Run to append to; defaults to the open run.
#' @export
lv_log <- function(..., level = c("info", "success", "warning", "danger"), run_id = NULL) {
  level <- match.arg(level)
  msg <- paste0(...)
  switch(level,
    info    = cli::cli_alert_info(msg),
    success = cli::cli_alert_success(msg),
    warning = cli::cli_alert_warning(msg),
    danger  = cli::cli_alert_danger(msg)
  )
  run_id <- run_id %||% lv_active_run()
  if (!is.null(run_id)) {
    line <- sprintf("%s [%s] %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                    level, cli::ansi_strip(msg))
    cat(line, "\n", sep = "", file = fs::path(lv_run_dir(run_id), "log.txt"), append = TRUE)
  }
  invisible(msg)
}

the_run <- new.env(parent = emptyenv())

lv_active_run <- function() the_run$id

#' Run a block with an active run context
#'
#' @param run_id Run identifier.
#' @param code Code to evaluate.
#' @export
lv_with_run <- function(run_id, code) {
  old <- the_run$id
  the_run$id <- run_id
  on.exit(the_run$id <- old, add = TRUE)
  force(code)
}

#' Write a JSON artifact into a run directory
#'
#' @param run_id Run identifier.
#' @param name File name, without extension.
#' @param x Object to serialize.
#' @export
lv_run_write <- function(run_id, name, x) {
  p <- fs::path(lv_run_dir(run_id), paste0(name, ".json"))
  jsonlite::write_json(x, p, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  invisible(p)
}
