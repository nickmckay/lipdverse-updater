#' Build the file-side view of QC state
#'
#' Reads the LiPD files and emits the same long cell table the sheet and store
#' produce, so all three inputs to [qc_merge()] share one shape and one
#' namespace.
#'
#' Replaces `createQCdataFrame()`, which built a wide frame whose column set
#' depended on which fields happened to be populated, so a field absent from
#' every loaded dataset simply vanished from the comparison.
#'
#' Only fields the registry marks `merged` or `key` are extracted; compilation
#' metadata now lives in `csm` and is not part of the shared merge.
#'
#' @param dir Directory of `.lpd` files, or an `lv_scan`.
#' @param registry Field registry.
#' @param datasets Optional dataset names to restrict to.
#' @param progress Show progress.
#' @return A cell table.
#' @export
qc_frame <- function(dir = lv_path("database"), registry = lv_qc_fields(),
                     datasets = NULL, progress = TRUE) {
  paths <- if (inherits(dir, "lv_scan")) dir$files$path else fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (!is.null(datasets)) {
    paths <- paths[sub("\\.lpd$", "", basename(paths)) %in% datasets]
  }
  if (!length(paths)) return(qc_cells_empty())

  wanted <- registry[registry$role %in% c("merged", "key"), ]
  # Files store fields without the structural prefix the registry uses.
  wanted$bare <- sub("^(paleoData|chronData|geo|pub[0-9]*|calibration)_", "", wanted$qc_name)
  canon <- stats::setNames(lv_canonical_field(wanted$qc_name, registry), wanted$bare)

  if (progress) cli::cli_alert_info("Reading QC state from {length(paths)} file{?s}")
  parts <- lapply(paths, function(p) qc_frame_one(p, canon))
  out <- purrr::list_rbind(parts)
  if (nrow(out) == 0) return(qc_cells_empty())

  # TSid and datasetId identify a cell rather than being cells themselves, and
  # the sheet reads them into the key columns rather than as values. Emitting
  # them here too would make datasetId read as a file-side change on every run
  # forever, adding one spurious event per timeseries each time.
  out <- out[!out$field %in% c("TSid", "datasetId"), , drop = FALSE]

  # One dataset can contribute the same dataset-level field from several
  # columns; keep one row per cell.
  out[!duplicated(paste(out$tsid, out$field)), , drop = FALSE]
}

qc_frame_one <- function(path, canon) {
  m <- tryCatch({
    nms <- utils::unzip(path, list = TRUE)$Name
    j <- grep("\\.jsonld$", nms, value = TRUE)
    if (!length(j)) stop("no jsonld")
    con <- unz(path, j[1]); on.exit(close(con), add = TRUE)
    jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (is.null(m)) return(NULL)

  dsid <- as_chr1(m$datasetId) %||% NA_character_
  dsn <- as_chr1(m$dataSetName) %||% sub("\\.lpd$", "", basename(path))

  # Dataset-level values are replicated onto every timeseries, matching how the
  # QC sheet presents them.
  root <- list()
  # `[[` on a named vector errors for a missing name, so look up by membership.
  cn <- function(k) if (length(k) == 1L && !is.na(k) && k %in% names(canon)) canon[[k]] else NULL
  add_root <- function(k, v) {
    if (is.null(cn(k))) return(invisible())
    s <- scalar_chr(v)
    if (length(s) != 1 || is.na(s) || !nzchar(s)) return(invisible())
    root[[cn(k)]] <<- s
  }
  # 624 datasets carry flattened interpretation keys at the dataset root, left
  # by lipdverseR. Interpretation is a column-level concept, so reading them
  # here replicates one column's interpretation onto every column of the
  # dataset -- and because they shadowed the real per-column values, they hid
  # the fact that per-column interpretations were not being read at all.
  root_keys <- setdiff(names(m), c("paleoData", "chronData", "pub", "geo", "@context"))
  root_keys <- root_keys[!grepl("Interpretation[0-9]+_", root_keys)]
  for (nm in root_keys) add_root(nm, m[[nm]])
  for (nm in names(m$geo)) {
    if (nm == "geometry") {
      for (g in names(m$geo$geometry)) add_root(paste0("geo_", g), m$geo$geometry[[g]])
    } else add_root(paste0("geo_", nm), m$geo[[nm]])
  }
  for (i in seq_along(m$pub)) {
    for (nm in names(m$pub[[i]])) add_root(paste0("pub", i, "_", nm), m$pub[[i]][[nm]])
  }
  # dataSetName is the operational key and always participates.
  root[["dataSetName"]] <- dsn

  rows <- list()
  for (pd in m$paleoData) {
    tables <- c(pd$measurementTable,
                unlist(lapply(pd$model, function(md) c(md$summaryTable, md$ensembleTable)),
                       recursive = FALSE))
    for (tb in tables) {
      if (!is.list(tb)) next
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      for (col in cols) {
        if (!is.list(col) || is.null(col$TSid)) next
        tsid <- as_chr1(col$TSid)
        if (is.null(tsid)) next
        vals <- root

        for (nm in names(col)) {
          if (nm == "interpretation") {
            # QC names index within a scope: environmentInterpretation1 is the
            # first *environment* interpretation, not the first entry in the
            # list. Numbering positionally instead produced names like
            # interpretation4_variable, which match nothing in the registry --
            # so every per-column interpretation was silently invisible to QC,
            # and the only interpretation values reaching the frame were the
            # flattened copies lipdverseR left at the dataset root.
            seen <- integer()
            for (i in seq_along(col$interpretation)) {
              it <- col$interpretation[[i]]
              if (!is.list(it)) next
              sc <- scalar_chr(it$scope)
              sc <- if (length(sc) == 1 && !is.na(sc) && nzchar(sc)) tolower(sc) else ""
              n <- if (sc %in% names(seen)) seen[[sc]] + 1L else 1L
              seen[[sc]] <- n
              prefix <- if (nzchar(sc)) paste0(sc, "Interpretation", n) else paste0("interpretation", n)
              for (s in names(it)) {
                k <- paste0(prefix, "_", s)
                if (!is.null(cn(k))) {
                  v <- scalar_chr(it[[s]])
                  if (length(v) == 1 && !is.na(v) && nzchar(v)) vals[[cn(k)]] <- v
                }
              }
            }
            next
          }
          if (is.list(col[[nm]])) next
          if (is.null(cn(nm))) next
          v <- scalar_chr(col[[nm]])
          if (length(v) == 1 && !is.na(v) && nzchar(v)) vals[[cn(nm)]] <- v
        }

        if (length(vals)) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            tsid = tsid, field = names(vals), value = unlist(vals, use.names = FALSE),
            present = TRUE, dataset_id = dsid, updated_at = NA_character_,
            source = "lipd", actor = NA_character_)
        }
      }
    }
  }
  if (!length(rows)) return(NULL)
  purrr::list_rbind(rows)
}
