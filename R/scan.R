#' Scan the database directory
#'
#' Hashes every `.lpd` file's bytes, in parallel, caching by
#' `(path, size, mtime)` so unchanged files are not rehashed on later runs.
#'
#' @param dir Database directory. Defaults to [lv_path()]`("database")`.
#' @param cache Use the on-disk hash cache.
#' @param workers Parallel workers; `NULL` picks a sensible default.
#' @return An `lv_scan` object.
#' @export
lv_scan <- function(dir = lv_path("database", must_exist = TRUE),
                    cache = TRUE, workers = NULL) {
  files <- fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (length(files) == 0) {
    cli::cli_abort("No .lpd files in {.path {dir}}.", class = "lv_error_scan")
  }
  info <- fs::file_info(files)[, c("path", "size", "modification_time")]
  names(info) <- c("path", "size", "mtime")
  info$path <- as.character(info$path)
  info$size <- as.numeric(info$size)

  cache_file <- fs::path(lv_path("state"), "cache", "scan-hashes.rds")
  prior <- if (cache && fs::file_exists(cache_file)) readRDS(cache_file) else NULL

  info$key <- paste(info$path, info$size, as.numeric(info$mtime), sep = "|")
  info$md5 <- if (is.null(prior)) NA_character_ else prior$md5[match(info$key, prior$key)]

  todo <- which(is.na(info$md5))
  if (length(todo)) {
    cli::cli_alert_info("Hashing {length(todo)} file{?s} ({nrow(info) - length(todo)} cached)")
    info$md5[todo] <- lv_hash_files(info$path[todo], workers = workers)
  }

  if (cache) {
    fs::dir_create(fs::path_dir(cache_file))
    saveRDS(info[, c("key", "md5")], cache_file)
  }

  out <- tibble::tibble(
    path        = info$path,
    file        = fs::path_file(info$path),
    dataSetName = sub("\\.lpd$", "", fs::path_file(info$path)),
    size        = info$size,
    mtime       = info$mtime,
    md5         = info$md5
  )
  out <- out[order(out$dataSetName), ]

  structure(
    list(files = out, dir = dir, fingerprint = lv_fingerprint(out), scanned_at = Sys.time()),
    class = "lv_scan"
  )
}

lv_hash_files <- function(paths, workers = NULL) {
  if (length(paths) == 0) return(character())
  if (length(paths) < 200) {
    return(vapply(paths, function(p) digest::digest(file = p, algo = "md5"), character(1), USE.NAMES = FALSE))
  }
  n <- workers %||% max(1L, min(16L, future::availableCores() - 1L))
  oplan <- future::plan(future::multisession, workers = n)
  on.exit(future::plan(oplan), add = TRUE)
  furrr::future_map_chr(paths, function(p) digest::digest(file = p, algo = "md5"))
}

#' Fingerprint a set of files
#'
#' The md5 of the sorted per-file md5s. lipdverseR's `directoryMD5()` zipped the
#' directory and hashed the zip, but zip embeds mtimes, so touching a file
#' without changing it produced a different fingerprint and forced a rebuild.
#'
#' @param files A tibble with `dataSetName` and `md5` columns.
#' @return A single md5 string.
#' @export
lv_fingerprint <- function(files) {
  o <- order(files$dataSetName)
  digest::digest(paste0(files$dataSetName[o], ":", files$md5[o], collapse = "\n"), algo = "md5")
}

#' @export
print.lv_scan <- function(x, ...) {
  cli::cli_h3("lv_scan")
  cli::cli_bullets(c(
    "*" = "{nrow(x$files)} file{?s} in {.path {x$dir}}",
    "*" = "{prettyNum(sum(x$files$size), big.mark = ',')} bytes",
    "*" = "fingerprint {.val {substr(x$fingerprint, 1, 12)}}"
  ))
  invisible(x)
}

#' Compare two scans
#' @param old,new `lv_scan` objects.
#' @return A tibble of added / removed / changed files.
#' @export
lv_scan_diff <- function(old, new) {
  o <- old$files; n <- new$files
  dplyr::bind_rows(
    tibble::tibble(status = "added",   dataSetName = setdiff(n$dataSetName, o$dataSetName)),
    tibble::tibble(status = "removed", dataSetName = setdiff(o$dataSetName, n$dataSetName)),
    {
      both <- intersect(o$dataSetName, n$dataSetName)
      ch <- both[o$md5[match(both, o$dataSetName)] != n$md5[match(both, n$dataSetName)]]
      tibble::tibble(status = "changed", dataSetName = ch)
    }
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
