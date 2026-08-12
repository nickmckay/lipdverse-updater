#' Fields calculated from the data on every update
#'
#' Some QC columns are not curated and not read: they are derived from the
#' measurement data each time the compilation runs. `minYear` and `maxYear` are
#' the common ones; `distinctYearsInCommonEra` is currently unique to
#' hydroclimate2k.
#'
#' Being unique to one compilation does not make a field `csm`. csm is a
#' judgement a compilation makes *about a dataset*; a calculated field is a fact
#' about the data that only some compilations choose to display. So these live
#' in the shared namespace, are `ownership = machine` so a sheet edit can never
#' win, and are recomputed rather than trusted.
#'
#' @section Which compilations get which calculations:
#' Decided by the QC sheet, not the registry: a calculation runs for a
#' compilation exactly when its QC tab carries the column. That keeps the
#' decision where the other column decisions already are -- a lead adds the
#' column to ask for the value -- and stops a compilation acquiring a field
#' nobody asked for. See [lv_sheet_calculations()].
#'
#' @section Why they are recomputed rather than read:
#' The values are stored in the LiPD files and the pipeline used to do no more
#' than echo them, so a dataset whose data changed kept a stale range and a
#' dataset ingested since the last legacy run had none at all. Measured on
#' hydroclimate2k 2026-08-12: 4,715 sheet rows carried a `minYear` that matched
#' its file exactly, and 190 rows across 116 datasets had no value in either.
#'
#' @name calculations
NULL

#' The available calculators
#'
#' Each entry takes the column's `year`, `age` and `values` vectors and returns
#' one scalar. `year` is years AD, `age` is years BP.
#'
#' @return A named list of functions, keyed by the QC field they populate.
#' @export
lv_calculators <- function() {
  list(
    # Masked to timesteps that actually carry a finite measurement, so the range
    # describes the data rather than the axis. A column padded with NaN to the
    # length of its table would otherwise report the table's span. Nick,
    # 2026-08-12: min/max always mask.
    minYear = function(year, age, values) lv_year_extreme(year, age, values, min),
    maxYear = function(year, age, values) lv_year_extreme(year, age, values, max),
    # Deliberately NOT masked, because the reference implementation
    # (lipdverseR/distinctYearsInCommonEra.R) counts the axis alone. Masking
    # would change every existing hydroclimate2k value, so it is a decision to
    # take on purpose rather than by tidying.
    distinctYearsInCommonEra = function(year, age, values) {
      lv_distinct_time_in_ce(year, age)
    }
  )
}

# Years AD from whichever axis the column has. LiPD stores age as years BP,
# where BP counts back from 1950.
#
# The extra step for non-positive years is the no-year-zero convention: the
# calendar runs 1 BCE, 1 CE, with nothing between, so 1950 - 5437 = -3487
# denotes 3488 BCE and is written -3488. geoChronR::convertBP2AD() does the same
# thing, and every hydroclimate2k minYear in the files already carries it.
# Without this the next run would rewrite 4,715 cells by one year each and call
# it a correction.
lv_year_from_age <- function(age) {
  y <- 1950 - age
  neg <- !is.na(y) & y <= 0
  y[neg] <- y[neg] - 1
  y
}

lv_year_extreme <- function(year, age, values, fun) {
  y <- if (!is.null(year) && any(is.finite(year))) year else
    if (!is.null(age) && any(is.finite(age))) lv_year_from_age(age) else NULL
  if (is.null(y)) return(NA_real_)
  # Mask to finite measurements. Where the two vectors are different lengths the
  # mask cannot be trusted, so the axis is used unmasked rather than silently
  # pairing the wrong elements.
  if (!is.null(values) && length(values) == length(y)) y <- y[is.finite(values)]
  y <- y[is.finite(y)]
  if (!length(y)) return(NA_real_)
  fun(y)
}

# Transcribed from lipdverseR/distinctYearsInCommonEra.R, which is the authority
# for hydroclimate2k's existing values. Prefers year; falls back to age; the
# windows are the same span expressed two ways (0-2025 AD, 1950 to -75 BP).
lv_distinct_time_in_ce <- function(year, age) {
  count <- function(v, lo, hi) {
    v <- floor(v[!is.na(v)])
    length(unique(v[v >= lo & v <= hi]))
  }
  if (!is.null(year) && !all(is.na(year))) return(count(year, 0, 2025))
  if (!is.null(age) && !all(is.na(age))) return(count(age, -75, 1950))
  NA_real_
}

#' Which calculated fields a compilation's QC sheet asks for
#'
#' A calculation runs when the tab has a column for it, so this reads the
#' header rather than the pulled cells: [qc_sheet_pull()] drops blank cells, so
#' a freshly added column would look absent exactly when it most needs filling.
#'
#' @param backend A sheet backend.
#' @param id Sheet id.
#' @param tab QC tab name.
#' @param registry Field registry.
#' @return Canonical field names.
#' @export
lv_sheet_calculations <- function(backend, id, tab = "QC", registry = lv_qc_fields()) {
  hdr <- tryCatch(names(sheet_read(backend, id, tab)), error = function(e) character())
  lv_calculations_for(hdr, registry)
}

#' @param columns Column names as they appear on a QC tab.
#' @rdname lv_sheet_calculations
#' @export
lv_calculations_for <- function(columns, registry = lv_qc_fields()) {
  if (!length(columns)) return(character())
  canon <- unique(lv_canonical_field(columns, registry))
  intersect(names(lv_calculators()), canon)
}

#' Calculate derived fields from the data
#'
#' Reads each dataset once and evaluates the requested calculators against every
#' curated paleo column, returning the same long cell table the other file-side
#' readers produce.
#'
#' @param fields Which calculated fields to produce; from
#'   [lv_sheet_calculations()].
#' @param dir Database directory.
#' @param datasets Dataset names to read.
#' @param tsids Optional TSids to restrict to.
#' @param index An `lv_index`, used to resolve dataset paths.
#' @param progress Show progress.
#' @return A cell table, with `source = "calc"`.
#' @export
lv_calculate <- function(fields, dir = lv_path("database"), datasets = NULL,
                         tsids = NULL, index = NULL, progress = TRUE) {
  calc <- lv_calculators()
  fields <- intersect(fields, names(calc))
  if (!length(fields)) return(qc_cells_empty())

  if (is.null(index)) index <- lv_db_index(lv_scan(dir), cache = TRUE)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)
  if (!is.null(datasets)) {
    # NFC on both sides; a filename is decomposed and a name from anywhere else
    # is composed. See qc_frame().
    keep <- lv_nfc(names(paths)) %in% lv_nfc(datasets)
    paths <- paths[keep]
  }
  if (!length(paths)) return(qc_cells_empty())

  if (progress) {
    cli::cli_alert_info("Calculating {.val {fields}} across {length(paths)} dataset{?s}")
  }
  parts <- lapply(unname(paths), function(p) lv_calculate_one(p, fields, calc))
  out <- purrr::list_rbind(parts)
  if (nrow(out) == 0) return(qc_cells_empty())
  if (!is.null(tsids)) out <- out[out$tsid %in% tsids, , drop = FALSE]
  out
}

lv_calculate_one <- function(path, fields, calc) {
  L <- tryCatch(suppressWarnings(lipdR::readLipd(path)), error = function(e) NULL)
  if (is.null(L)) return(NULL)
  dsid <- as_chr1(L$datasetId) %||% NA_character_
  rows <- list()

  for (pd in L$paleoData) {
    for (tb in pd$measurementTable) {
      if (!is.list(tb)) next
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      cols <- Filter(function(c) is.list(c) && !is.null(c$TSid), cols)
      if (!length(cols)) next

      # The axis is shared by every column of the table, so it is resolved once.
      axis <- lv_table_axes(cols)

      for (col in cols) {
        tsid <- as_chr1(col$TSid)
        if (is.null(tsid)) next
        nm <- tolower(trimws(as_chr1(col$variableName) %||% ""))
        # Axis columns are not curated and get no QC row, so calculating for
        # them would produce cells with nowhere to go.
        if (nm %in% LV_AXIS_QC_VARIABLES) next
        vals <- suppressWarnings(as.numeric(unlist(col$values)))
        for (f in fields) {
          v <- tryCatch(calc[[f]](axis$year, axis$age, vals), error = function(e) NA_real_)
          if (length(v) != 1 || !is.finite(v)) next
          rows[[length(rows) + 1L]] <- tibble::tibble(
            tsid = tsid, field = f, value = lv_format_number(v), present = TRUE,
            dataset_id = dsid, updated_at = NA_character_, source = "calc",
            actor = NA_character_)
        }
      }
    }
  }
  if (!length(rows)) return(NULL)
  purrr::list_rbind(rows)
}

# The year and age vectors of a measurement table, by variableName.
lv_table_axes <- function(cols) {
  pick <- function(names_wanted) {
    for (col in cols) {
      nm <- tolower(trimws(as_chr1(col$variableName) %||% ""))
      if (nm %in% names_wanted) {
        v <- suppressWarnings(as.numeric(unlist(col$values)))
        if (any(is.finite(v))) return(v)
      }
    }
    NULL
  }
  list(year = pick(c("year", "year ad", "yearad")),
       age = pick(c("age", "agebp", "age bp", "yearbp")))
}

# QC state is character. Whole numbers must not arrive as "1980.0000", which
# would read as a change against the sheet's "1980" on every run.
lv_format_number <- function(x) {
  if (!is.finite(x)) return(NA_character_)
  if (x == round(x) && abs(x) < 1e15) return(format(round(x), scientific = FALSE, trim = TRUE))
  format(x, digits = 15, scientific = FALSE, trim = TRUE)
}
