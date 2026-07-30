#' Resolve a configured path from the environment
#'
#' Every path in this package comes from an environment variable with a
#' documented default. lipdverseR hardcoded absolute paths in dozens of places
#' and drifted between them (`/Volumes/data/Dropbox/...` vs `~/Dropbox/...` vs
#' `/Users/nicholas/Dropbox/...` in three functions of the same file), which
#' made it unrunnable anywhere else and hard to test.
#'
#' @param which Path name.
#' @param must_exist Abort if the resolved path does not exist.
#' @return A normalized path string.
#' @export
lv_path <- function(which = c("database", "snapshots", "qcstore", "state", "export", "holding_tank"),
                    must_exist = FALSE) {
  which <- match.arg(which)
  spec <- switch(which,
    database     = list(env = "LIPDVERSE_DATABASE",     default = "~/Dropbox/lipdverse/database"),
    snapshots    = list(env = "LIPDVERSE_SNAPSHOTS",    default = "~/lipdverse-snapshots"),
    qcstore      = list(env = "LIPDVERSE_QCSTORE",      default = "~/GitHub/lipdverse-qcstore"),
    state        = list(env = "LIPDVERSE_STATE",        default = "~/.local/share/lipdverse-updater"),
    export       = list(env = "LIPDVERSE_EXPORT",       default = "~/lipdverse-export"),
    holding_tank = list(env = "LIPDVERSE_HOLDING_TANK", default = "~/Dropbox/lipdverse/batchHoldingTank")
  )
  p <- Sys.getenv(spec$env, unset = spec$default)
  if (!nzchar(trimws(p))) {
    cli::cli_abort("{.envvar {spec$env}} is set but empty.")
  }
  p <- path.expand(p)
  if (must_exist && !fs::dir_exists(p) && !fs::file_exists(p)) {
    cli::cli_abort(c(
      "{which} path does not exist: {.path {p}}",
      i = "Set {.envvar {spec$env}} to override (default {.path {spec$default}})."
    ), class = "lv_error_path")
  }
  p
}

#' Read a secret without putting it in the repo
#'
#' lipdverseR read `sql.secret` by relative path from the working directory,
#' so the credential it returned depended on where R happened to be running.
#'
#' @param name Secret name, e.g. `"mysql"`.
#' @param required Abort when missing.
#' @return A named list, or `NULL` when absent and not required.
#' @export
lv_secret <- function(name, required = TRUE) {
  env <- paste0("LIPDVERSE_SECRET_", toupper(gsub("[^A-Za-z0-9]", "_", name)))
  raw <- Sys.getenv(env, unset = "")
  if (nzchar(raw)) return(as.list(jsonlite::fromJSON(raw)))

  f <- fs::path(lv_path("state"), "secrets", paste0(name, ".json"))
  if (fs::file_exists(f)) return(as.list(jsonlite::read_json(f, simplifyVector = TRUE)))

  if (!required) return(NULL)
  cli::cli_abort(c(
    "Secret {.val {name}} not found.",
    i = "Set {.envvar {env}} to a JSON object, or create {.path {f}}."
  ), class = "lv_error_secret")
}
