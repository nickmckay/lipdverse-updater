#' Read a dataset's coordinates without loading it
#'
#' Coordinates appear in two shapes. Flat `latitude`/`longitude` keys, or GeoJSON
#' whose `coordinates` run `[longitude, latitude, elevation]` — note the order,
#' which is the opposite of how anyone says it aloud. `readLipd()` normalises the
#' two, but it also extracts every column of every table to do it, which is a lot
#' of work for two numbers.
#'
#' @param path A `.lpd` file.
#' @return A one-row tibble of `dataSetName`, `geo_latitude`, `geo_longitude`,
#'   or `NULL` if the file cannot be read.
#' @export
lv_geo_of <- function(path) {
  nm <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) NULL)
  j <- grep("jsonld$", nm, value = TRUE)
  if (!length(j)) return(NULL)
  con <- unz(path, j[1])
  m <- tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                   simplifyVector = FALSE), error = function(e) NULL)
  close(con)
  if (is.null(m)) return(NULL)

  g <- m$geo
  lat <- g$latitude; lon <- g$longitude
  co <- g$geometry$coordinates
  if (is.null(lat) && length(co) >= 2) lat <- co[[2]]
  if (is.null(lon) && length(co) >= 1) lon <- co[[1]]
  num <- function(v) {
    if (is.null(v) || !length(v)) return(NA_character_)
    format(as.numeric(v[[1]]), digits = 15, trim = TRUE, scientific = FALSE)
  }
  tibble::tibble(dataSetName = as_chr1(m$dataSetName) %||% NA_character_,
                 geo_latitude = num(lat), geo_longitude = num(lon))
}

#' Coordinates that disagree between a QC sheet and the files
#'
#' Written because a comparison bug hid these for as long as it existed:
#' [values_equal()] used to compare numbers at the lesser of their two
#' precisions, so a coordinate rounded to one decimal matched the unrounded one
#' it came from and nothing could see the difference. Four hydroclimate2k
#' datasets lost precision that way.
#'
#' Reports rather than repairs. Which side is right is a judgement — the finer
#' value usually is, but not when someone has corrected a coordinate outright.
#'
#' @param cfg From [lv_config()].
#' @param backend From [sheet_backend_google()].
#' @param index From [lv_db_index()].
#' @return A tibble of the disagreements, with which side carries more precision
#'   and whether one is the other rounded.
#' @export
lv_geo_drift <- function(cfg, backend, index = NULL) {
  index <- index %||% lv_db_index(lv_scan(cfg$lipd_dir), cache = TRUE)
  ds <- lv_compilation_datasets(cfg, backend, index)$datasets
  sheet <- qc_sheet_pull(backend, cfg$qc_sheet_id, cfg$qc_tabs$qc)

  paths <- index$datasets$path[index$datasets$fileDataSetName %in% ds]
  files <- dplyr::bind_rows(lapply(paths, lv_geo_of))
  if (!nrow(files)) return(lv_geo_drift_empty())
  files <- tidyr::pivot_longer(files, c("geo_latitude", "geo_longitude"),
                               names_to = "field", values_to = "file")

  dsn <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  s <- sheet |>
    dplyr::filter(field %in% c("geo_latitude", "geo_longitude")) |>
    dplyr::mutate(dataSetName = unname(dsn[tsid])) |>
    dplyr::filter(!is.na(dataSetName)) |>
    dplyr::select("dataSetName", "field", sheet = "value") |>
    dplyr::distinct()

  cmp <- dplyr::inner_join(s, files, by = c("dataSetName", "field"))
  cmp <- cmp[!is.na(cmp$sheet) & !is.na(cmp$file), , drop = FALSE]
  if (!nrow(cmp)) return(lv_geo_drift_empty())

  out <- cmp[!values_equal(cmp$sheet, cmp$file), , drop = FALSE]
  if (!nrow(out)) return(lv_geo_drift_empty())

  dp <- function(v) nchar(sub("^[^.]*\\.?", "", v))
  out$sheet_dp <- dp(out$sheet); out$file_dp <- dp(out$file)
  out$finer <- ifelse(out$sheet_dp > out$file_dp, "sheet",
               ifelse(out$file_dp > out$sheet_dp, "file", "tie"))
  # A rounding is a precision difference; anything else is a disagreement about
  # where the site is, and no rule about precision should settle that.
  out$is_rounding <- mapply(function(a, b, da, db) {
    n <- min(da, db)
    isTRUE(all.equal(round(as.numeric(a), n), round(as.numeric(b), n)))
  }, out$sheet, out$file, out$sheet_dp, out$file_dp)
  out$compilation <- cfg$compilation
  out[, c("compilation", "dataSetName", "field", "sheet", "file",
          "sheet_dp", "file_dp", "finer", "is_rounding")]
}

lv_geo_drift_empty <- function() {
  tibble::tibble(compilation = character(), dataSetName = character(),
                 field = character(), sheet = character(), file = character(),
                 sheet_dp = integer(), file_dp = integer(),
                 finer = character(), is_rounding = logical())
}
