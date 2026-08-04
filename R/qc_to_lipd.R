#' Write merged QC state back into LiPD files
#'
#' Takes the cell table a merge resolved to and puts each value where it belongs
#' in the LiPD structure.
#'
#' Replaces `updateFromQC()` (`lipdverseR/R/qcSheet.R:568`), a nested loop over
#' datasets by variables carrying a `txtProgressBar` and a "vectorize this?"
#' TODO. Here the work is grouped once by dataset and applied in a single pass
#' per file.
#'
#' Compilation-specific metadata is **not** handled here. It lives under `csm`
#' inside `inCompilation` and is written by `lipdR`'s own collapse path.
#'
#' @name qc_to_lipd
NULL

#' Decompose a canonical field name into where it lives
#'
#' | pattern | container | example |
#' | --- | --- | --- |
#' | `geo_*` | `geo` | `geo_latitude` |
#' | `pubN_*` | `pub[[N]]` | `pub1_doi` |
#' | `paleoData_*` | column | `paleoData_units` |
#' | `<scope>InterpretationN_*` | column `interpretation[[N]]` | `climateInterpretation1_basis` |
#' | `interpretationN_*` | column `interpretation[[N]]` | `interpretation2_variable` |
#' | `calibration_*` | column `calibration` | `calibration_method` |
#' | anything else | dataset root | `archiveType` |
#'
#' @param field Canonical field names.
#' @return A tibble of `field`, `container`, `index`, `key`, `scope`, `level`.
#' @export
lv_field_path <- function(field) {
  n <- length(field)
  container <- rep("root", n); index <- rep(NA_integer_, n)
  key <- field; scope <- rep(NA_character_, n)

  m <- grepl("^geo_", field)
  container[m] <- "geo"; key[m] <- sub("^geo_", "", field[m])

  m <- grepl("^pub[0-9]+_", field)
  container[m] <- "pub"
  index[m] <- as.integer(sub("^pub([0-9]+)_.*$", "\\1", field[m]))
  key[m] <- sub("^pub[0-9]+_", "", field[m])

  m <- grepl("^calibration_", field)
  container[m] <- "calibration"; key[m] <- sub("^calibration_", "", field[m])

  # Scoped interpretations carry their scope in the prefix; lipdR stores the
  # scope as a field on the interpretation rather than in its name.
  m <- grepl("^[A-Za-z]+Interpretation[0-9]+_", field)
  container[m] <- "interpretation"
  scope[m] <- tolower(sub("^([A-Za-z]+)Interpretation[0-9]+_.*$", "\\1", field[m]))
  index[m] <- as.integer(sub("^[A-Za-z]+Interpretation([0-9]+)_.*$", "\\1", field[m]))
  key[m] <- sub("^[A-Za-z]+Interpretation[0-9]+_", "", field[m])

  m <- grepl("^interpretation[0-9]+_", field)
  container[m] <- "interpretation"
  index[m] <- as.integer(sub("^interpretation([0-9]+)_.*$", "\\1", field[m]))
  key[m] <- sub("^interpretation[0-9]+_", "", field[m])

  m <- grepl("^paleoData_", field)
  container[m] <- "column"; key[m] <- sub("^paleoData_", "", field[m])

  m <- grepl("^chronData_", field)
  container[m] <- "column"; key[m] <- sub("^chronData_", "", field[m])

  tibble::tibble(
    field = field, container = container, index = index, key = key, scope = scope,
    level = ifelse(container %in% c("root", "geo", "pub"), "dataset", "column")
  )
}

#' Apply merged QC cells to a database
#'
#' @param cells Merged cell table (`tsid`, `field`, `value`).
#' @param dir Source database directory.
#' @param out Staging directory to write into. Never writes in place; use
#'   [lv_promote()] to commit.
#' @param registry Field registry.
#' @param index An `lv_index`, to map TSids to files without re-reading.
#' @param progress Show progress.
#' @return An `lv_issues` tibble describing what could not be applied.
#' @export
lv_apply_qc <- function(cells, dir = lv_path("database"), out,
                        registry = lv_qc_fields(), index = NULL, progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  fs::dir_create(out)
  if (nrow(cells) == 0) return(lv_issues_empty())

  # Only fields that participate in the merge; csm and the rest are elsewhere.
  rule <- lv_field_rule(cells$field, registry)
  cells <- cells[rule$role %in% c("merged", "key"), , drop = FALSE]
  if (nrow(cells) == 0) return(lv_issues_empty())

  if (is.null(index)) index <- lv_db_index(lv_scan(dir), cache = TRUE)
  loc <- lv_field_path(unique(cells$field))
  cells <- dplyr::left_join(cells, loc, by = "field")

  # Which file each TSid lives in.
  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  cells$dataSetName <- unname(ts2ds[cells$tsid])
  orphan <- cells[is.na(cells$dataSetName), , drop = FALSE]
  cells <- cells[!is.na(cells$dataSetName), , drop = FALSE]

  issues <- if (nrow(orphan)) {
    lv_issues(check = "tsid_not_in_database", severity = "warn",
              message = "TSid is not in any file; value not applied.",
              TSid = orphan$tsid, field = orphan$field, value = orphan$value)
  } else lv_issues_empty()

  by_ds <- split(cells, cells$dataSetName)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)
  if (progress) cli::cli_alert_info("Applying {nrow(cells)} cell{?s} across {length(by_ds)} dataset{?s}")

  for (dsn in names(by_ds)) {
    p <- paths[[dsn]]
    if (is.null(p) || is.na(p)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "dataset_not_found", severity = "error",
        message = "Dataset has cells but no file.", dataSetName = dsn))
      next
    }
    L <- tryCatch(lipdR::readLipd(p), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the dataset.", dataSetName = dsn, path = p))
      next
    }
    res <- lv_apply_to_lipd(L, by_ds[[dsn]])
    lipdR::writeLipd(res$L, path = out, removeNamesFromLists = TRUE)
    if (nrow(res$issues)) issues <- lv_issues_bind(issues, res$issues)
  }
  issues
}

#' Apply one dataset's cells to its LiPD object
#' @param L A LiPD object.
#' @param cells That dataset's cells, already joined to field locations.
#' @return A list of `L` and `issues`.
#' @keywords internal
lv_apply_to_lipd <- function(L, cells) {
  issues <- lv_issues_empty()
  dsn <- L$dataSetName %||% NA_character_

  # Dataset-level values repeat across every timeseries of a dataset, so one is
  # taken. But they can disagree: hydroclimate2k's sheet holds two rows for
  # CO07CAFR whose pub1_citation differs, one of them mojibake, and taking the
  # first row silently wrote the mangled one. A field that is supposed to be the
  # same for the whole dataset and is not is a conflict, not a coin toss.
  ds_cells <- cells[cells$level == "dataset", , drop = FALSE]
  if (nrow(ds_cells)) {
    disagree <- stats::aggregate(list(n = ds_cells$value), by = list(field = ds_cells$field),
                                 FUN = function(v) length(unique(v[!is.na(v)])))
    bad <- disagree$field[disagree$n > 1]
    if (length(bad)) {
      d <- ds_cells[ds_cells$field %in% bad, , drop = FALSE]
      issues <- lv_issues_bind(issues, lv_issues(
        check = "dataset_field_disagrees", severity = "warn",
        message = "A dataset-level field has different values on different rows of this dataset; not applied.",
        dataSetName = dsn, TSid = d$tsid, field = d$field, value = d$value))
      ds_cells <- ds_cells[!ds_cells$field %in% bad, , drop = FALSE]
    }
  }
  ds_cells <- ds_cells[!duplicated(ds_cells$field), , drop = FALSE]
  for (i in seq_len(nrow(ds_cells))) {
    r <- ds_cells[i, ]
    v <- lv_coerce_value(r$value)
    if (r$container == "root") {
      L[[r$key]] <- v
    } else if (r$container == "geo") {
      if (is.null(L$geo)) L$geo <- list()
      L$geo[[r$key]] <- v
    } else if (r$container == "pub") {
      j <- r$index
      if (is.null(L$pub)) L$pub <- list()
      # Append the next slot, but never fabricate a run of empty publications
      # to reach a distant index: lipdR drops empty pub entries on write, which
      # would silently relocate the value to a different publication.
      if (j <= length(L$pub) + 1L) {
        if (j > length(L$pub)) L$pub[[j]] <- list()
        L$pub[[j]][[r$key]] <- v
      } else {
        issues <- lv_issues_bind(issues, lv_issues(
          check = "pub_index_gap", severity = "warn",
          message = paste0("Value targets publication ", j, " but the dataset has ",
                           length(L$pub), "; not applied."),
          dataSetName = dsn, field = r$field, value = r$value))
      }
    }
  }

  col_cells <- cells[cells$level == "column", , drop = FALSE]
  if (nrow(col_cells)) {
    want <- split(col_cells, col_cells$tsid)
    for (blk in c("paleoData", "chronData")) {
      if (is.null(L[[blk]])) next
      for (pd in seq_along(L[[blk]])) {
        for (tb in seq_along(L[[blk]][[pd]]$measurementTable)) {
          tab <- L[[blk]][[pd]]$measurementTable[[tb]]
          for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
            col <- tab[[cn]]
            if (!is.list(col) || is.null(col$TSid)) next
            tsid <- as.character(col$TSid)[1]
            if (is.null(want[[tsid]])) next
            col <- lv_apply_to_column(col, want[[tsid]])
            tab[[cn]] <- col
            want[[tsid]] <- NULL
          }
          L[[blk]][[pd]]$measurementTable[[tb]] <- tab
        }
      }
    }
    if (length(want)) {
      left <- dplyr::bind_rows(want)
      issues <- lv_issues_bind(issues, lv_issues(
        check = "tsid_not_in_dataset", severity = "warn",
        message = "TSid was expected in this dataset but not found.",
        dataSetName = dsn, TSid = left$tsid, field = left$field))
    }
  }
  list(L = L, issues = issues)
}

# Which entry of a column's interpretation list `<scope>InterpretationN` means.
#
# The index in a QC field name counts within a scope, but lipdR stores one flat
# list mixing scopes -- a column can hold three climate interpretations then
# three isotope ones. Writing environmentInterpretation1 to `interpretation[[1]]`
# overwrote a *climate* interpretation's variable on a real dataset.
#
# Returns NA when the slot does not exist yet, so the caller appends.
lv_interp_slot <- function(interp, scope, index) {
  if (is.na(index)) index <- 1L
  if (is.na(scope)) {
    # Unscoped names are positional over entries that carry no scope.
    hits <- which(vapply(interp, function(e) {
      s <- if (is.list(e)) as_chr1(e$scope) else NULL
      is.null(s) || !nzchar(s)
    }, logical(1)))
  } else {
    hits <- which(vapply(interp, function(e) {
      s <- if (is.list(e)) as_chr1(e$scope) else NULL
      !is.null(s) && identical(tolower(s), tolower(scope))
    }, logical(1)))
  }
  if (index <= length(hits)) hits[index] else NA_integer_
}

lv_apply_to_column <- function(col, cells) {
  for (i in seq_len(nrow(cells))) {
    r <- cells[i, ]
    v <- lv_coerce_value(r$value)
    if (r$container == "column") {
      col[[r$key]] <- v
    } else if (r$container == "calibration") {
      if (!is.list(col$calibration)) col$calibration <- list()
      col$calibration[[r$key]] <- v
    } else if (r$container == "interpretation") {
      if (!is.list(col$interpretation)) col$interpretation <- list()
      j <- lv_interp_slot(col$interpretation, r$scope, r$index)
      if (is.na(j)) {
        # The named slot does not exist yet: append rather than reuse a
        # position, which would overwrite an interpretation of another scope.
        col$interpretation[[length(col$interpretation) + 1L]] <- list()
        j <- length(col$interpretation)
        if (!is.na(r$scope)) col$interpretation[[j]]$scope <- r$scope
      }
      col$interpretation[[j]][[r$key]] <- v
    }
  }
  col
}

# QC state is character throughout. Restore the obvious types on the way back
# into the file, so a number does not become a quoted string.
lv_coerce_value <- function(x) {
  if (is.na(x)) return(NULL)          # clearing a cell removes the field
  if (grepl("^(TRUE|FALSE)$", x)) return(as.logical(x))
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n) && grepl("^\\s*-?[0-9.]+([eE][-+]?[0-9]+)?\\s*$", x)) return(n)
  x
}
