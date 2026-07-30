#' Validate database identity
#'
#' Runs before anything else in an update. Every finding is an `lv_issues` row;
#' `error` severity aborts the run.
#'
#' Nothing is auto-renamed. lipdverseR resolved duplicate TSids by appending
#' `-dup` in a `while` loop (`standardizeQCInfo:759`, `createQcFromFile:817`),
#' which mints identifiers that match nothing downstream and hides the problem
#' rather than fixing it. Repair here is a separate, explicit act.
#'
#' @param index An `lv_index` from [lv_db_index()].
#' @param strict Treat duplicate identifiers as errors rather than warnings.
#' @return An `lv_issues` tibble.
#' @export
lv_validate_identity <- function(index, strict = TRUE) {
  d <- index$datasets
  ts <- index$timeseries
  sev <- if (strict) "error" else "warn"
  out <- list()

  out$parse <- lv_issues(
    check = "file_unreadable", severity = "error",
    message = paste0("Could not extract metadata: ", d$parse_error[!is.na(d$parse_error)]),
    path = d$path[!is.na(d$parse_error)],
    dataSetName = d$fileDataSetName[!is.na(d$parse_error)]
  )

  miss_id <- d[is.na(d$datasetId) & is.na(d$parse_error), ]
  out$missing_id <- lv_issues(
    check = "missing_datasetId", severity = "warn",
    message = "Dataset has no datasetId; one will need to be minted.",
    dataSetName = miss_id$fileDataSetName, path = miss_id$path
  )

  miss_dsn <- d[is.na(d$dataSetName) & is.na(d$parse_error), ]
  out$missing_dsn <- lv_issues(
    check = "missing_dataSetName", severity = "error",
    message = "Metadata has no dataSetName.",
    dataSetName = miss_dsn$fileDataSetName, path = miss_dsn$path
  )

  # The filename is the operational key; a mismatch means readLipd/writeLipd
  # round-trips will rename the file or collide with another.
  mm <- d[!is.na(d$dataSetName) & d$dataSetName != d$fileDataSetName, ]
  out$name_mismatch <- lv_issues(
    check = "filename_metadata_mismatch", severity = "warn",
    message = paste0("Filename says '", mm$fileDataSetName, "' but metadata says '", mm$dataSetName, "'."),
    dataSetName = mm$fileDataSetName, path = mm$path, value = mm$dataSetName
  )

  out$dup_id  <- dup_issues(d, "datasetId",   "duplicate_datasetId",   sev,
                            "datasetId is used by more than one file.")
  out$dup_dsn <- dup_issues(d, "dataSetName", "duplicate_dataSetName", sev,
                            "dataSetName is used by more than one file.")
  out$dup_file <- dup_issues(d, "fileDataSetName", "duplicate_filename", "error",
                             "Two files resolve to the same dataSetName.")

  # A TSid must be unique across the whole database, not just within a file:
  # the QC sheets are keyed on it.
  if (nrow(ts)) {
    tally <- dplyr::count(ts, .data$TSid, name = "n")
    dups <- tally$TSid[tally$n > 1]
    dd <- ts[ts$TSid %in% dups, ]
    if (nrow(dd)) {
      agg <- dplyr::summarise(dplyr::group_by(dd, .data$TSid),
                              n = dplyr::n(),
                              datasets = paste(sort(unique(.data$dataSetName)), collapse = ";"),
                              vars = paste(sort(unique(.data$variableName)), collapse = ";"),
                              .groups = "drop")
      # Within one dataset is a file-level defect; across datasets silently
      # merges unrelated records in every QC sheet keyed on TSid.
      agg$scope <- ifelse(!grepl(";", agg$datasets), "within_dataset", "across_datasets")
      out$dup_tsid <- lv_issues(
        check = paste0("duplicate_TSid_", agg$scope), severity = sev,
        message = paste0("TSid appears ", agg$n, " times in ", agg$datasets, " (", agg$vars, ")."),
        TSid = agg$TSid, dataSetName = agg$datasets, value = agg$vars
      )
    }
    empty <- ts[is.na(ts$TSid) | !nzchar(ts$TSid), ]
    out$missing_tsid <- lv_issues(
      check = "missing_TSid", severity = "warn",
      message = "Column has no TSid.",
      dataSetName = empty$dataSetName, field = empty$variableName
    )
  }

  no_ts <- d[d$n_ts == 0 & is.na(d$parse_error), ]
  out$no_ts <- lv_issues(
    check = "no_timeseries", severity = "warn",
    message = "Dataset contains no columns with a TSid.",
    dataSetName = no_ts$fileDataSetName, path = no_ts$path
  )

  do.call(lv_issues_bind, unname(out))
}

dup_issues <- function(d, col, check, severity, msg) {
  v <- d[[col]]
  ok <- !is.na(v) & nzchar(v)
  tally <- table(v[ok])
  dups <- names(tally)[tally > 1]
  if (!length(dups)) return(NULL)
  rows <- d[ok & v %in% dups, ]
  agg <- dplyr::summarise(dplyr::group_by(rows, key = .data[[col]]),
                          n = dplyr::n(),
                          files = paste(sort(.data$file), collapse = ";"),
                          .groups = "drop")
  lv_issues(
    check = check, severity = severity,
    message = paste0(msg, " (", agg$n, " files: ", agg$files, ")"),
    value = agg$key, path = agg$files
  )
}
