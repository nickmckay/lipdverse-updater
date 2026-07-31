#' Inventory every key present in a database
#'
#' Walks the metadata of every `.lpd` and counts how often each key occurs,
#' naming keys the way the LiPD timeseries flattening does
#' (`paleoData_variableName`, `geo_latitude`, `pub1_doi`) so the result can be
#' compared directly against a key-standardization mapping.
#'
#' Publication and interpretation indices are collapsed to 1, matching the
#' convention in the standardization sheet: `pub3_doi` is the same key as
#' `pub1_doi`, occurring in a later slot.
#'
#' @param dir Directory of `.lpd` files, or an `lv_scan`.
#' @param collapse_index Collapse `pubN_`/`interpretationN_` to 1.
#' @return A tibble of `key`, `count`, `n_datasets`, `scope`, `example`.
#' @export
lv_key_inventory <- function(dir = lv_path("database"), collapse_index = TRUE) {
  paths <- if (inherits(dir, "lv_scan")) dir$files$path else fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (!length(paths)) cli::cli_abort("No .lpd files found.", class = "lv_error_keys")

  cli::cli_alert_info("Inventorying keys across {length(paths)} file{?s}")
  parts <- lapply(paths, function(p) lv_keys_one(p, collapse_index))
  all <- purrr::list_rbind(parts)
  if (!nrow(all)) return(all)

  dplyr::arrange(
    dplyr::summarise(
      dplyr::group_by(all, .data$key, .data$scope),
      count = sum(.data$n),
      n_datasets = dplyr::n_distinct(.data$dataset),
      example = utils::head(stats::na.omit(.data$example), 1)[1],
      .groups = "drop"
    ),
    dplyr::desc(.data$count)
  )
}

lv_keys_one <- function(path, collapse_index) {
  m <- tryCatch({
    nms <- utils::unzip(path, list = TRUE)$Name
    j <- grep("\\.jsonld$", nms, value = TRUE)
    if (!length(j)) stop("no jsonld")
    con <- unz(path, j[1]); on.exit(close(con), add = TRUE)
    jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (is.null(m)) return(NULL)

  ds <- as_chr1(m$dataSetName) %||% basename(path)
  acc <- new.env(parent = emptyenv())

  add <- function(key, scope, example) {
    if (collapse_index) key <- gsub("([A-Za-z]+)[0-9]+_", "\\1_", key)
    k <- paste(scope, key, sep = "")
    cur <- acc[[k]]
    if (is.null(cur)) acc[[k]] <- list(key = key, scope = scope, n = 1L, example = example)
    else acc[[k]]$n <- cur$n + 1L
  }

  # Root-level keys, excluding the containers walked separately.
  for (nm in setdiff(names(m), c("paleoData", "chronData", "pub", "geo", "@context"))) {
    add(nm, "root", short_example(m[[nm]]))
  }
  for (nm in names(m$geo)) {
    if (nm == "geometry") {
      for (g in names(m$geo$geometry)) add(paste0("geo_", g), "geo", short_example(m$geo$geometry[[g]]))
    } else {
      add(paste0("geo_", nm), "geo", short_example(m$geo[[nm]]))
    }
  }
  for (i in seq_along(m$pub)) {
    for (nm in names(m$pub[[i]])) add(paste0("pub", i, "_", nm), "pub", short_example(m$pub[[i]][[nm]]))
  }

  for (blk in c("paleoData", "chronData")) {
    if (is.null(m[[blk]])) next
    pfx <- blk
    for (obj in m[[blk]]) {
      tables <- c(obj$measurementTable,
                  unlist(lapply(obj$model, function(md) {
                    c(md$summaryTable, md$ensembleTable, md$distributionTable)
                  }), recursive = FALSE))
      for (tb in tables) {
        if (!is.list(tb)) next
        for (nm in setdiff(names(tb), "columns")) add(paste0(pfx, "_", nm), "table", short_example(tb[[nm]]))
        cc <- if (!is.null(tb$columns)) tb$columns else tb
        for (col in cc) {
          if (!is.list(col) || is.null(col$TSid)) next
          for (nm in names(col)) {
            if (nm %in% c("interpretation", "calibration", "hasResolution", "inCompilationBeta")) {
              sub <- col[[nm]]
              if (nm == "interpretation") {
                for (i in seq_along(sub)) {
                  for (s in names(sub[[i]])) add(paste0("interpretation", i, "_", s), "interpretation", short_example(sub[[i]][[s]]))
                }
              } else if (is.list(sub) && !is.null(names(sub))) {
                for (s in names(sub)) add(paste0(nm, "_", s), nm, short_example(sub[[s]]))
              } else {
                for (e in sub) for (s in names(e)) add(paste0(nm, "_", s), nm, short_example(e[[s]]))
              }
            } else {
              add(paste0(pfx, "_", nm), "column", short_example(col[[nm]]))
            }
          }
        }
      }
    }
  }

  vals <- as.list(acc)
  tibble::tibble(
    dataset = ds,
    key     = vapply(vals, function(v) v$key, character(1)),
    scope   = vapply(vals, function(v) v$scope, character(1)),
    n       = vapply(vals, function(v) v$n, integer(1)),
    example = vapply(vals, function(v) v$example %||% NA_character_, character(1))
  )
}

short_example <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) return(NA_character_)
  if (length(x) > 1) return(paste0("<", length(x), " values>"))
  v <- as.character(x)[1]
  if (is.na(v)) return(NA_character_)
  if (nchar(v) > 60) paste0(substr(v, 1, 57), "...") else v
}
