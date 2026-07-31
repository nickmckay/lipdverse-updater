#' Fields excluded from shadow comparison
#'
#' Regular expressions matched against `field_path`. These change on every write
#' regardless of whether anything meaningful differs, so leaving them in would
#' bury real findings.
#'
#' @return A tibble of `pattern` and `reason`.
#' @export
shadow_ignore <- function() {
  readr::read_csv(lv_extdata("shadow_ignore.csv"),
                  col_types = readr::cols(.default = readr::col_character()),
                  progress = FALSE)
}

#' Flatten a database directory to canonical long form
#'
#' Produces one row per leaf value, keyed by `datasetId`, `TSid` and
#' `field_path`, with deterministic ordering. Two directories normalized this
#' way can be compared field by field regardless of how their files are
#' ordered internally.
#'
#' Columns are keyed by **TSid, not position**, so reordering columns within a
#' table produces no diff. Data vectors are replaced by a hash of their
#' contents: a change is still detected, but the output stays small enough to
#' diff 7,000 datasets.
#'
#' @param dir Directory of `.lpd` files, or an `lv_scan`.
#' @param ignore Tibble of ignore patterns; `NULL` keeps everything.
#' @param progress Show a progress bar.
#' @return A tibble: `dataSetName`, `datasetId`, `TSid`, `field_path`, `value`.
#' @export
shadow_normalize <- function(dir, ignore = shadow_ignore(), progress = TRUE) {
  if (inherits(dir, "lv_scan")) {
    paths <- dir$files$path
  } else {
    paths <- fs::dir_ls(dir, glob = "*.lpd", type = "file")
  }
  if (!length(paths)) cli::cli_abort("No .lpd files found.", class = "lv_error_shadow")

  if (progress) cli::cli_alert_info("Normalizing {length(paths)} file{?s}")
  out <- purrr::list_rbind(lapply(paths, shadow_normalize_one))

  if (!is.null(ignore) && nrow(ignore) && nrow(out)) {
    drop <- Reduce(`|`, lapply(ignore$pattern, function(p) grepl(p, out$field_path)))
    out <- out[!drop, , drop = FALSE]
  }
  # Deterministic ordering is a requirement, not a nicety: shadow_diff joins on
  # these keys and the report is snapshot-compared.
  out[order(out$datasetId, out$TSid, out$field_path, method = "radix"), , drop = FALSE]
}

shadow_normalize_one <- function(path) {
  members <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) character())
  m <- tryCatch({
    j <- grep("\\.jsonld$", members, value = TRUE)
    if (!length(j)) stop("no .jsonld member")
    con <- unz(path, j[1])
    on.exit(close(con), add = TRUE)
    jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  }, error = function(e) NULL)

  if (is.null(m)) {
    return(tibble::tibble(dataSetName = sub("\\.lpd$", "", basename(path)),
                          datasetId = NA_character_, TSid = NA_character_,
                          field_path = "<unreadable>", value = "TRUE"))
  }

  dsn <- as_chr1(m$dataSetName) %||% sub("\\.lpd$", "", basename(path))
  dsid <- as_chr1(m$datasetId) %||% dsn

  # Dataset level: everything except the data blocks.
  root <- m[setdiff(names(m), c("paleoData", "chronData"))]
  ds <- flatten_leaves(root, "")

  # Column level: keyed by TSid so column order cannot create a diff.
  cols <- list()
  for (blk in c("paleoData", "chronData")) {
    if (is.null(m[[blk]])) next
    for (obj in m[[blk]]) {
      tables <- c(obj$measurementTable,
                  unlist(lapply(obj$model, function(md) {
                    c(md$summaryTable, md$ensembleTable, md$distributionTable)
                  }), recursive = FALSE))
      for (tb in tables) {
        if (!is.list(tb)) next
        cc <- if (!is.null(tb$columns)) tb$columns else tb
        for (col in cc) {
          if (!is.list(col) || is.null(col$TSid)) next
          tsid <- as_chr1(col$TSid)
          leaves <- flatten_leaves(col[setdiff(names(col), "TSid")], "")
          if (!length(leaves)) next
          cols[[length(cols) + 1L]] <- tibble::tibble(
            dataSetName = dsn, datasetId = dsid, TSid = tsid,
            field_path = paste0(blk, ".", names(leaves)), value = unname(leaves)
          )
        }
      }
    }
  }

  # The .jsonld carries metadata only; the measurements live in CSV members of
  # the bag. Without hashing those, a shadow diff would call two databases
  # identical while their data differed.
  data_rows <- shadow_data_hashes(path, members, dsn, dsid)

  dplyr::bind_rows(
    if (length(ds)) tibble::tibble(dataSetName = dsn, datasetId = dsid, TSid = NA_character_,
                                   field_path = names(ds), value = unname(ds)),
    purrr::list_rbind(cols),
    data_rows
  )
}

shadow_data_hashes <- function(path, members, dsn, dsid) {
  csvs <- grep("\\.csv$", members, value = TRUE)
  if (!length(csvs)) return(NULL)

  # Hash the parsed values, not the bytes. Two writers can serialize the same
  # numbers differently -- lipdverseR quotes every field, so a byte hash marks
  # every file as changed and buries any genuine data difference.
  vals <- vapply(csvs, function(f) {
    con <- unz(path, f)
    txt <- tryCatch(readLines(con, warn = FALSE), error = function(e) NULL)
    try(close(con), silent = TRUE)
    if (is.null(txt) || !length(txt)) return("<unreadable>")
    cells <- lapply(txt, function(line) {
      v <- utils::read.csv(text = line, header = FALSE, colClasses = "character",
                           stringsAsFactors = FALSE)
      trimws(as.character(unlist(v, use.names = FALSE)))
    })
    flat <- unlist(cells, use.names = FALSE)
    # Canonicalise numeric text so "1.10" and "1.1" agree.
    num <- suppressWarnings(as.numeric(flat))
    flat <- ifelse(is.na(num), flat, format(num, digits = 15, trim = TRUE, scientific = FALSE))
    paste0("<", length(txt), "x", length(cells[[1]]), ":",
           substr(digest::digest(flat, algo = "md5"), 1, 12), ">")
  }, character(1), USE.NAMES = FALSE)

  tibble::tibble(dataSetName = dsn, datasetId = dsid, TSid = NA_character_,
                 field_path = paste0("data.", basename(csvs)), value = vals)
}

# Recursively flatten to a named character vector of leaf values.
flatten_leaves <- function(x, prefix) {
  if (is.null(x)) return(character())

  # A data vector: hash rather than emit thousands of rows. A change still
  # shows up, but the normalized form of the whole database stays tractable.
  if (!is.null(names(x)) && identical(prefix, "")) {
    # top-level object, fall through
  }
  if (is.list(x) && !is.null(names(x))) {
    out <- character()
    for (nm in names(x)) {
      key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
      out <- c(out, if (nm == "values") hash_values(x[[nm]], key) else flatten_leaves(x[[nm]], key))
    }
    return(out)
  }
  if (is.list(x)) {
    if (!length(x)) return(character())
    out <- character()
    for (i in seq_along(x)) out <- c(out, flatten_leaves(x[[i]], sprintf("%s[%d]", prefix, i)))
    return(out)
  }
  if (length(x) == 0) return(character())
  if (length(x) > 1) return(hash_values(x, prefix))
  stats::setNames(scalar_chr(x), prefix)
}

hash_values <- function(v, key) {
  flat <- suppressWarnings(unlist(v, use.names = FALSE))
  stats::setNames(paste0("<", length(flat), ":", substr(digest::digest(scalar_chr(flat), algo = "md5"), 1, 12), ">"), key)
}

# Normalize numeric formatting so 1.10 and 1.1 do not read as a difference.
scalar_chr <- function(x) {
  if (is.numeric(x)) {
    return(ifelse(is.na(x), NA_character_, format(x, digits = 15, trim = TRUE, scientific = FALSE)))
  }
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, as.character(x)))
  as.character(x)
}

#' Diff two normalized databases
#'
#' @param old,new Tibbles from [shadow_normalize()].
#' @param by Key columns; `datasetId` is stable across renames.
#' @return A tibble of differences with a `class` of `only_old`, `only_new` or `differs`.
#' @export
shadow_diff <- function(old, new, by = c("datasetId", "TSid", "field_path")) {
  o <- dplyr::rename(old[, c(by, "value", "dataSetName")], old_value = "value")
  n <- dplyr::rename(new[, c(by, "value", "dataSetName")], new_value = "value")

  j <- dplyr::full_join(o, n, by = by, suffix = c("_old", "_new"))
  j$dataSetName <- dplyr::coalesce(j$dataSetName_old, j$dataSetName_new)

  j$class <- dplyr::case_when(
    is.na(j$new_value) & !is.na(j$old_value) ~ "only_old",
    is.na(j$old_value) & !is.na(j$new_value) ~ "only_new",
    j$old_value != j$new_value               ~ "differs",
    TRUE                                     ~ "same"
  )
  out <- j[j$class != "same", c("dataSetName", by, "class", "old_value", "new_value")]
  out[order(factor(out$class, levels = c("differs", "only_old", "only_new")),
            out$field_path, out$datasetId, method = "radix"), , drop = FALSE]
}

#' Summarise a shadow diff
#'
#' The acceptance gate for a compilation cutover: zero rows, or every remaining
#' row justified in an accepted-differences file.
#'
#' @param diff A tibble from [shadow_diff()].
#' @param path Optional CSV path to write the full diff to.
#' @return The diff, invisibly.
#' @export
shadow_report <- function(diff, path = NULL) {
  if (!is.null(path)) {
    fs::dir_create(fs::path_dir(path))
    readr::write_csv(diff, path, na = "")
  }
  if (nrow(diff) == 0) {
    cli::cli_alert_success("No differences.")
    return(invisible(diff))
  }
  cli::cli_h3("{nrow(diff)} difference{?s} across {dplyr::n_distinct(diff$datasetId)} dataset{?s}")
  print(as.data.frame(dplyr::count(diff, .data$class, name = "n")), row.names = FALSE)
  cli::cli_h3("by field")
  top <- dplyr::arrange(dplyr::count(diff, .data$field_path, .data$class, name = "n"), dplyr::desc(.data$n))
  print(as.data.frame(utils::head(top, 15)), row.names = FALSE)
  if (!is.null(path)) cli::cli_alert_info("Full diff: {.path {path}}")
  invisible(diff)
}

#' Snapshot a database directory for a shadow run
#'
#' Uses APFS clonefile when available, so copying the 7,177-file database is
#' near-instant and costs almost no disk.
#'
#' @param src Source directory.
#' @param dest Destination directory; must not already exist.
#' @return `dest`, invisibly.
#' @export
shadow_snapshot <- function(src, dest) {
  if (fs::dir_exists(dest)) cli::cli_abort("{.path {dest}} already exists.", class = "lv_error_shadow")
  fs::dir_create(dest)
  files <- fs::dir_ls(src, glob = "*.lpd", type = "file")
  ok <- suppressWarnings(system2("cp", c("-Rc", shQuote(src), shQuote(fs::path(dest, "db"))),
                                 stdout = FALSE, stderr = FALSE))
  if (!identical(ok, 0L)) {
    fs::dir_create(fs::path(dest, "db"))
    fs::file_copy(files, fs::path(dest, "db"), overwrite = TRUE)
  }
  invisible(dest)
}
