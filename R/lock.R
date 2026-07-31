#' Acquire an exclusive lock on a compilation
#'
#' Uses `dir.create()`, which is atomic: exactly one caller wins the race.
#'
#' Release with [lv_unlock()], but prefer [lv_locked()], which registers the
#' release with `withr::defer()` so any `stop()`, `abort()` or interrupt still
#' unlocks. lipdverseR called `unFlagUpdate()` only on the last line of
#' `changeloggingAndUpdating()`, so any earlier failure left the database
#' flagged as updating indefinitely, and `addLipdToDatabase()` refused to run
#' until it was cleared by hand.
#'
#' @param compilation Compilation name.
#' @param run_id Run identifier recorded in the lock.
#' @param timeout_minutes Age past which a lock is considered stale.
#' @return The lock path, invisibly.
#' @export
lv_lock <- function(compilation, run_id = lv_run_id(), timeout_minutes = 240) {
  p <- lock_path(compilation)
  fs::dir_create(fs::path_dir(p))

  if (!dir.create(p, showWarnings = FALSE)) {
    info <- lv_lock_status(compilation)
    if (isTRUE(info$stale)) {
      cli::cli_alert_warning("Breaking stale lock on {.val {compilation}} (held by pid {info$pid}, {info$age_minutes} min old).")
      lv_lock_break(compilation)
      if (!dir.create(p, showWarnings = FALSE)) {
        cli::cli_abort("Could not acquire lock on {.val {compilation}}.", class = "lv_error_lock")
      }
    } else {
      cli::cli_abort(c(
        "{.val {compilation}} is locked by another run.",
        i = "pid {info$pid} on {info$host}, run {info$run_id}, started {info$started_at}.",
        i = "If that process is gone, use {.code lv_lock_break('{compilation}')}."
      ), class = "lv_error_lock")
    }
  }

  jsonlite::write_json(
    list(compilation = compilation, run_id = run_id, pid = Sys.getpid(),
         host = as.character(Sys.info()[["nodename"]]),
         started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         timeout_minutes = timeout_minutes),
    fs::path(p, "lock.json"), auto_unbox = TRUE, pretty = TRUE)

  invisible(p)
}

#' @rdname lv_lock
#' @export
lv_unlock <- function(compilation) {
  p <- lock_path(compilation)
  if (fs::dir_exists(p)) fs::dir_delete(p)
  invisible(TRUE)
}

#' Hold a lock for the duration of the calling frame
#'
#' @param compilation Compilation name.
#' @param run_id Run identifier.
#' @param timeout_minutes Stale threshold.
#' @param envir Frame whose exit releases the lock.
#' @export
lv_locked <- function(compilation, run_id = lv_run_id(), timeout_minutes = 240,
                      envir = parent.frame()) {
  lv_lock(compilation, run_id = run_id, timeout_minutes = timeout_minutes)
  withr::defer(lv_unlock(compilation), envir = envir)
  invisible(TRUE)
}

#' Inspect a lock
#' @param compilation Compilation name.
#' @return A list; `locked = FALSE` when free.
#' @export
lv_lock_status <- function(compilation) {
  p <- lock_path(compilation)
  if (!fs::dir_exists(p)) return(list(locked = FALSE))
  f <- fs::path(p, "lock.json")
  if (!fs::file_exists(f)) return(list(locked = TRUE, stale = TRUE, pid = NA, host = NA,
                                       run_id = NA, started_at = NA, age_minutes = NA))
  info <- jsonlite::read_json(f, simplifyVector = TRUE)
  started <- as.POSIXct(info$started_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  age <- as.numeric(difftime(Sys.time(), started, units = "mins"))
  same_host <- identical(info$host, as.character(Sys.info()[["nodename"]]))
  info$locked <- TRUE
  info$age_minutes <- round(age, 1)
  # Stale when the holder is gone, or when it has outlived its timeout. The
  # pid test only means anything on the machine that took the lock.
  info$stale <- (same_host && !pid_alive(info$pid)) ||
                (!is.na(age) && age > (info$timeout_minutes %||% 240))
  info
}

#' @rdname lv_lock_status
#' @export
lv_lock_break <- function(compilation) lv_unlock(compilation)

lock_path <- function(compilation) {
  # Separators become underscores so a name can never escape the lock
  # directory, and leading dots are replaced too: ".._evil.lock" would
  # otherwise be a dotfile and stay invisible to ordinary listings, including
  # the ones an operator uses to find a stuck lock.
  safe <- gsub("[^A-Za-z0-9_.-]", "_", compilation)
  safe <- sub("^[.]+", "_", safe)
  fs::path(lv_path("state"), "locks", paste0(safe, ".lock"))
}

pid_alive <- function(pid) {
  if (is.null(pid) || is.na(pid)) return(FALSE)
  if (.Platform$OS.type == "windows") return(TRUE)
  # kill -0 tests for existence without signalling.
  system2("kill", c("-0", as.integer(pid)), stdout = FALSE, stderr = FALSE) == 0
}
