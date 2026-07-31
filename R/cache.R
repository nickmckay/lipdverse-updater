#' Content-addressed stage cache
#'
#' Caches a stage's result under a key derived from the stage name, a
#' stage-version constant, and the hashes of its inputs. Bumping the version
#' constant in the calling code invalidates every entry for that stage.
#'
#' This is drake's useful property without drake's coupling: lipdverseR's
#' stages communicated through a `params`/`data` list splatted into each
#' function's frame with `assign()`, so a target's real inputs were invisible
#' and cache correctness could not be reasoned about.
#'
#' @param stage Stage name.
#' @param inputs List of values the result depends on.
#' @param expr Expression producing the result; evaluated only on a miss.
#' @param version Stage version; bump to invalidate.
#' @param use_cache Set `FALSE` to force recomputation.
#' @return The stage result.
#' @export
lv_cached <- function(stage, inputs, expr, version = 1L, use_cache = TRUE) {
  key <- lv_cache_key(stage, inputs, version)
  p <- lv_cache_path(stage, key)

  if (use_cache && fs::file_exists(p)) {
    res <- tryCatch(readRDS(p), error = function(e) NULL)
    if (!is.null(res)) {
      lv_log("cache hit: ", stage, " (", substr(key, 1, 8), ")")
      return(res$value)
    }
    # A truncated file from an interrupted write must not poison the stage.
    fs::file_delete(p)
  }

  value <- force(expr)
  if (use_cache) {
    fs::dir_create(fs::path_dir(p))
    tmp <- paste0(p, ".tmp", Sys.getpid())
    saveRDS(list(stage = stage, key = key, version = version,
                 created_at = Sys.time(), value = value), tmp)
    # Rename so a reader never observes a half-written entry.
    fs::file_move(tmp, p)
  }
  value
}

lv_cache_key <- function(stage, inputs, version) {
  digest::digest(list(stage = stage, version = version, inputs = inputs), algo = "md5")
}

lv_cache_path <- function(stage, key) {
  fs::path(lv_path("state"), "cache", stage, paste0(key, ".rds"))
}

#' Clear cached stage results
#'
#' @param stage Stage to clear; `NULL` clears everything.
#' @return Number of files removed.
#' @export
lv_cache_clear <- function(stage = NULL) {
  root <- fs::path(lv_path("state"), "cache")
  if (!fs::dir_exists(root)) return(invisible(0L))
  target <- if (is.null(stage)) root else fs::path(root, stage)
  if (!fs::dir_exists(target)) return(invisible(0L))
  files <- fs::dir_ls(target, recurse = TRUE, type = "file")
  fs::file_delete(files)
  invisible(length(files))
}

#' Summarise the stage cache
#' @return A tibble of stages, entry counts and sizes.
#' @export
lv_cache_info <- function() {
  root <- fs::path(lv_path("state"), "cache")
  if (!fs::dir_exists(root)) return(tibble::tibble(stage = character(), n = integer(), bytes = numeric()))
  files <- fs::dir_ls(root, recurse = TRUE, type = "file", glob = "*.rds")
  if (!length(files)) return(tibble::tibble(stage = character(), n = integer(), bytes = numeric()))
  info <- fs::file_info(files)
  dplyr::summarise(
    dplyr::group_by(tibble::tibble(stage = basename(fs::path_dir(files)),
                                   bytes = as.numeric(info$size)), .data$stage),
    n = dplyr::n(), bytes = sum(.data$bytes), .groups = "drop"
  )
}
