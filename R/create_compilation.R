#' Create a compilation from a list of TSids
#'
#' Adds an `inCompilation` entry to every named column. This is the common way
#' a compilation begins: someone has a list of timeseries and wants them
#' gathered under a name.
#'
#' The write is **additive**. An existing membership for the same compilation is
#' updated in place (its version list gains the new version); memberships for
#' other compilations are untouched. Nothing else in the file changes.
#'
#' Never writes in place: output goes to a staging directory for [lv_promote()].
#'
#' @param tsids TSids to include.
#' @param compilation Compilation name.
#' @param version Compilation version to record, e.g. `"1_0_0"`.
#' @param dir Source database directory.
#' @param out Staging directory.
#' @param index An `lv_index`, to avoid re-reading the database.
#' @param progress Show progress.
#' @return A list of `issues`, `datasets` touched, and `tsids` placed.
#' @export
lv_create_compilation <- function(tsids, compilation, version = "1_0_0",
                                  dir = lv_path("database"), out,
                                  index = NULL, progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  if (!nzchar(compilation)) cli::cli_abort("{.arg compilation} must be a non-empty name.")
  fs::dir_create(out)

  tsids <- unique(stats::na.omit(as.character(tsids)))
  if (!length(tsids)) cli::cli_abort("No TSids given.", class = "lv_error_compilation")

  if (is.null(index)) index <- lv_db_index(lv_scan(dir), cache = TRUE)
  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  ds <- unname(ts2ds[tsids])

  missing_ts <- tsids[is.na(ds)]
  issues <- if (length(missing_ts)) {
    lv_issues(check = "tsid_not_in_database", severity = "warn",
              message = "TSid is not in any file; not added to the compilation.",
              TSid = missing_ts)
  } else lv_issues_empty()

  keep <- !is.na(ds)
  tsids <- tsids[keep]; ds <- ds[keep]
  by_ds <- split(tsids, ds)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)

  if (progress) {
    cli::cli_alert_info("Adding {length(tsids)} TSid{?s} across {length(by_ds)} dataset{?s} to {.val {compilation}}")
  }

  placed <- 0L
  for (dsn in names(by_ds)) {
    p <- paths[[dsn]]
    L <- tryCatch(lipdR::readLipd(p), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the dataset.", dataSetName = dsn, path = p))
      next
    }
    r <- lv_add_membership(L, by_ds[[dsn]], compilation, version)
    placed <- placed + r$placed
    if (nrow(r$issues)) issues <- lv_issues_bind(issues, r$issues)
    lipdR::writeLipd(r$L, path = out, removeNamesFromLists = TRUE)
  }

  list(issues = issues, datasets = names(by_ds), tsids = tsids, placed = placed)
}

#' Add compilation membership to the named columns of one dataset
#' @keywords internal
lv_add_membership <- function(L, tsids, compilation, version) {
  issues <- lv_issues_empty()
  dsn <- L$dataSetName %||% NA_character_
  want <- tsids
  placed <- 0L

  for (blk in c("paleoData", "chronData")) {
    if (is.null(L[[blk]])) next
    for (pd in seq_along(L[[blk]])) {
      for (tb in seq_along(L[[blk]][[pd]]$measurementTable)) {
        tab <- L[[blk]][[pd]]$measurementTable[[tb]]
        for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
          col <- tab[[cn]]
          if (!is.list(col) || is.null(col$TSid)) next
          tsid <- as.character(col$TSid)[1]
          if (!tsid %in% want) next

          ic <- col$inCompilation
          if (!is.list(ic)) {
            # Two columns in the database store inCompilation as a bare string
            # rather than a list of entries. Discarding it would be exactly the
            # kind of silent loss this rewrite exists to stop, so promote it to
            # the normal structure and report it.
            old <- as.character(ic)
            old <- old[!is.na(old) & nzchar(old)]
            ic <- lapply(old, function(n) list(compilationName = n))
            if (length(old)) {
              issues <- lv_issues_bind(issues, lv_issues(
                check = "malformed_inCompilation", severity = "warn",
                message = "inCompilation was a bare string; converted to an entry.",
                dataSetName = dsn, TSid = tsid, value = old))
            }
          }
          names_here <- vapply(ic, function(e) {
            n <- if (is.list(e)) unlist(e[["compilationName"]]) else NULL
            if (length(n)) as.character(n)[1] else NA_character_
          }, character(1))

          j <- which(names_here == compilation)
          if (length(j)) {
            # Already a member: add this version if it is not recorded yet.
            j <- j[1]
            v <- unlist(ic[[j]]$compilationVersion)
            if (!version %in% v) ic[[j]]$compilationVersion <- c(v, version)
          } else {
            ic[[length(ic) + 1L]] <- list(compilationName = compilation,
                                          compilationVersion = version)
          }
          col$inCompilation <- ic
          tab[[cn]] <- col
          want <- setdiff(want, tsid)
          placed <- placed + 1L
        }
        L[[blk]][[pd]]$measurementTable[[tb]] <- tab
      }
    }
  }

  if (length(want)) {
    issues <- lv_issues_bind(issues, lv_issues(
      check = "tsid_not_in_dataset", severity = "warn",
      message = "TSid was expected in this dataset but not found.",
      dataSetName = dsn, TSid = want))
  }
  list(L = L, issues = issues, placed = placed)
}

#' Which datasets are in a compilation
#'
#' The datasets a run *considers*, which is wider than the datasets whose
#' timeseries are members. Membership grows in two steps, and this is the first:
#'
#' 1. A curator sets a dataset to TRUE in `datasetsInCompilation`. Every one of
#'    its timeseries then appears in the QC tab, all with
#'    `inThisCompilation = FALSE`.
#' 2. They flag individual timeseries TRUE there to admit them.
#'
#' So this returns the QC tab's scope, not the compilation's contents. Names are
#' checked against the database rather than trusted, since a sheet can name a
#' dataset that no longer exists.
#'
#' @param cfg An `lv_config`.
#' @param backend A sheet backend.
#' @param index An `lv_index`, to resolve names against the files.
#' @return A list of `datasets` considered, `missing` names, and `excluded`.
#' @export
lv_compilation_datasets <- function(cfg, backend, index = NULL) {
  x <- sheet_read(backend, cfg$qc_sheet_id, cfg$qc_tabs$datasets)
  x <- x[, !duplicated(names(x)) & nzchar(names(x)) & !is.na(names(x)), drop = FALSE]
  nm <- intersect(c("dsn", "dataSetName"), names(x))
  if (!length(nm)) {
    cli::cli_abort("Membership tab {.val {cfg$qc_tabs$datasets}} has no dsn column.",
                   class = "lv_error_sheet")
  }

  # The tab's own instructions: "Any datasets marked as FALSE will not be
  # considered for the update, NA or TRUE will be considered." Requiring TRUE
  # instead would drop every dataset a curator had not yet ruled on, which is
  # precisely the set the tab exists to surface.
  keep <- if ("inComp" %in% names(x)) !tolower(trimws(x$inComp)) %in% "false" else rep(TRUE, nrow(x))
  want <- unique(stats::na.omit(x[[nm[1]]][keep]))

  if (is.null(index)) index <- lv_db_index(lv_scan(cfg$lipd_dir), cache = TRUE)
  # Compare in one normal form. macOS hands back filenames in NFD while the
  # metadata inside the file is NFC, so `Büntgen` on disk and `Büntgen` in the
  # sheet are different strings that render identically -- and the dataset reads
  # as missing from a database it is sitting in.
  have <- lv_nfc(want) %in% index$datasets$fileDataSetName
  list(datasets = want[have], missing = want[!have],
       excluded = unique(stats::na.omit(x[[nm[1]]][!keep])))
}

#' The file-side view of compilation membership
#'
#' One cell per timeseries, so membership merges by the same rules as any other
#' curator-owned field: the sheet wins a straight disagreement, the store
#' supplies the baseline, and every change becomes an event.
#'
#' @param index An `lv_index`.
#' @param compilation Compilation name.
#' @param tsids Timeseries in scope. Those not in the compilation get `FALSE`,
#'   which is what exposes a candidate to the curator.
#' @return A cell table.
#' @export
lv_membership_frame <- function(index, compilation, tsids) {
  # QC covers paleoData only. This function emits a row per TSid it is handed,
  # so a chron TSid in `tsids` becomes a sheet row carrying nothing but a
  # membership flag: no dataSetName, no archiveType, nothing to curate. That is
  # how 6,495 chron rows reached the hydroclimate2k sheet.
  tsids <- lv_qc_timeseries(index, tsids = tsids)
  members <- lv_compilation_timeseries(index, compilation)
  dsid <- index$timeseries$datasetId[match(tsids, index$timeseries$TSid)]
  tibble::tibble(
    tsid = tsids, field = "inThisCompilation",
    value = ifelse(tsids %in% members, "TRUE", "FALSE"),
    present = TRUE, dataset_id = dsid,
    updated_at = NA_character_, source = "lipd", actor = NA_character_)
}

#' Apply merged membership decisions to the files
#'
#' Additions add an `inCompilation` entry; removals drop it. Both are written
#' to staging for [lv_promote()], never in place.
#'
#' @param cells Merged cells for `inThisCompilation`.
#' @param index An `lv_index`.
#' @param compilation Compilation name.
#' @param version Compilation version recorded on an addition.
#' @param dir Source database directory.
#' @param out Staging directory.
#' @param progress Show progress.
#' @return A list of `added`, `removed`, `issues` and `datasets` touched.
#' @export
lv_apply_membership <- function(cells, index, compilation, version = "1_0_0",
                                dir = lv_path("database"), out, progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  cells <- cells[cells$field == "inThisCompilation", , drop = FALSE]
  members <- lv_compilation_timeseries(index, compilation)

  want_in <- cells$tsid[cells$value %in% "TRUE"]
  # Only an explicit FALSE removes. A blank is "no opinion": most cells in a
  # real QC tab are blank, and treating those as removals would empty the
  # compilation on the first run.
  want_out <- cells$tsid[cells$value %in% "FALSE"]
  add <- setdiff(want_in, members)
  drop <- intersect(want_out, members)

  if (!length(add) && !length(drop)) {
    return(list(added = character(), removed = character(),
                issues = lv_issues_empty(), datasets = character()))
  }
  fs::dir_create(out)

  ts2ds <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  paths <- stats::setNames(index$datasets$path, index$datasets$fileDataSetName)
  touched <- unique(stats::na.omit(unname(ts2ds[c(add, drop)])))
  if (progress) {
    cli::cli_alert_info(
      "Membership: {length(add)} addition{?s}, {length(drop)} removal{?s} across {length(touched)} dataset{?s}")
  }

  issues <- lv_issues_empty()
  for (dsn in touched) {
    p <- paths[[dsn]]
    L <- tryCatch(lipdR::readLipd(p), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the dataset.", dataSetName = dsn, path = p))
      next
    }
    a <- intersect(add, names(which(ts2ds == dsn)))
    d <- intersect(drop, names(which(ts2ds == dsn)))
    if (length(a)) {
      r <- lv_add_membership(L, a, compilation, version)
      L <- r$L
      if (nrow(r$issues)) issues <- lv_issues_bind(issues, r$issues)
    }
    if (length(d)) L <- lv_drop_membership(L, d, compilation)
    lipdR::writeLipd(L, path = out, removeNamesFromLists = TRUE)
  }

  list(added = add, removed = drop, issues = issues, datasets = touched)
}

#' Remove compilation membership from the named columns
#' @keywords internal
lv_drop_membership <- function(L, tsids, compilation) {
  for (blk in c("paleoData", "chronData")) {
    if (is.null(L[[blk]])) next
    for (pd in seq_along(L[[blk]])) {
      for (tb in seq_along(L[[blk]][[pd]]$measurementTable)) {
        tab <- L[[blk]][[pd]]$measurementTable[[tb]]
        for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
          col <- tab[[cn]]
          if (!is.list(col) || is.null(col$TSid)) next
          if (!as.character(col$TSid)[1] %in% tsids) next
          ic <- col$inCompilation
          if (!is.list(ic) || !length(ic)) next
          nm <- vapply(ic, function(e) {
            n <- if (is.list(e)) unlist(e[["compilationName"]]) else unlist(e)
            if (length(n)) as.character(n)[1] else NA_character_
          }, character(1))
          keep <- is.na(nm) | nm != compilation
          # Drop the key entirely rather than leaving an empty list, which
          # lipdR would write back as an empty array.
          col$inCompilation <- if (any(keep)) ic[keep] else NULL
          tab[[cn]] <- col
        }
        L[[blk]][[pd]]$measurementTable[[tb]] <- tab
      }
    }
  }
  L
}

#' Build the tabs for a new compilation's QC sheet
#'
#' @param cells A cell table from [qc_frame()], restricted to the compilation.
#' @param index An `lv_index`, for the membership tab.
#' @param registry Field registry.
#' @return A named list of data frames, one per tab.
#' @export
lv_compilation_sheet <- function(cells, index, registry = lv_qc_fields()) {
  # Thematic, not alphabetical. qc_cells_to_sheet already orders this way when
  # given no template; sorting again here undid it, and a newly created sheet is
  # exactly where the grouping matters most -- there is no existing layout for a
  # lead to fall back on.
  qc <- qc_cells_to_sheet(cells, registry)

  # The membership tab catalogues the *whole database*, not just the members.
  # That is what makes a new dataset visible: a curator flips it to TRUE here,
  # its timeseries appear in the QC tab on the next run, and they are then
  # admitted one by one via inThisCompilation.
  considered <- unique(index$timeseries$dataSetName[index$timeseries$TSid %in% cells$tsid])
  all_ds <- index$datasets$fileDataSetName
  members <- tibble::tibble(
    dsn = all_ds,
    dsid = index$datasets$datasetId,
    inComp = ifelse(all_ds %in% considered, "TRUE", "FALSE"),
    instructions = LV_MEMBERSHIP_INSTRUCTIONS)
  members <- members[order(members$dsn), , drop = FALSE]

  list(QC = qc, datasetsInCompilation = members)
}

# Kept verbatim from the existing sheets: compilation leads read this text, and
# it is also the specification -- FALSE excludes, anything else considers.
LV_MEMBERSHIP_INSTRUCTIONS <- paste(
  "Any datasets marked as FALSE will not be considered for the update,",
  "NA or TRUE will be considered.")

#' Offer datasets to a compilation
#'
#' Writes names into the `datasetsInCompilation` tab so their timeseries appear
#' in the QC tab on the next run, each with `inThisCompilation = FALSE`. This is
#' the first half of the two-step: it makes a dataset *visible* to the leads, who
#' then admit individual timeseries on the QC tab. It never adds anything to a
#' compilation.
#'
#' Appends, and skips names already listed, so it is safe to re-run. Nothing is
#' rewritten: the tab holds thousands of rows of other people's curation.
#'
#' @param names Dataset names. Typically the `add` rows of [lv_run_receipt()].
#' @param cfg From [lv_config()].
#' @param backend From [sheet_backend_google()].
#' @param index From [lv_db_index()]; supplies each dataset's id.
#' @param in_compilation Value for the `inComp` column. `TRUE` marks them as
#'   considered, which is what makes them appear in the QC tab.
#' @param dry_run Report without writing. Defaults to `TRUE`.
#' @return The rows appended (or that would be), invisibly.
#' @export
lv_offer_to_compilation <- function(names, cfg, backend, index = NULL,
                                    in_compilation = TRUE, dry_run = TRUE) {
  names <- unique(stats::na.omit(as.character(names)))
  tab <- sheet_read(backend, cfg$qc_sheet_id, cfg$qc_tabs$datasets)
  cols <- base::names(tab)
  if (!"dsn" %in% cols) {
    cli::cli_abort("{.field dsn} column not found in {.val {cfg$qc_tabs$datasets}}; found {.val {cols}}.")
  }

  already <- intersect(names, tab$dsn)
  todo <- setdiff(names, tab$dsn)
  if (length(already)) {
    cli::cli_alert_info("{length(already)} name{?s} already listed; skipping {?it/them}.")
  }
  if (!length(todo)) {
    cli::cli_alert_success("Nothing to add.")
    return(invisible(tab[0, , drop = FALSE]))
  }

  idx <- index %||% lv_db_index(lv_scan(lv_path("database")), cache = TRUE)
  id_of <- stats::setNames(idx$datasets$datasetId, idx$datasets$dataSetName)
  add <- tibble::tibble(dsn = todo, dsid = unname(id_of[todo]),
                        inComp = in_compilation)
  missing_id <- is.na(add$dsid)
  if (any(missing_id)) {
    cli::cli_alert_warning(
      "{sum(missing_id)} dataset{?s} have no datasetId in the index: {.val {utils::head(add$dsn[missing_id], 5)}}")
  }
  # Match the tab's own columns, leaving any others (e.g. instructions) blank.
  for (nm in setdiff(cols, base::names(add))) add[[nm]] <- NA
  add <- add[, cols, drop = FALSE]

  if (dry_run) {
    cli::cli_alert_info("Dry run: would append {nrow(add)} row{?s} to {.val {cfg$qc_tabs$datasets}} ({nrow(tab)} rows now).")
    return(invisible(add))
  }
  sheet_append(backend, cfg$qc_sheet_id, cfg$qc_tabs$datasets, add)
  cli::cli_alert_success("Appended {nrow(add)} row{?s} to {.val {cfg$qc_tabs$datasets}}.")
  invisible(add)
}

#' The timeseries QC covers
#'
#' paleoData only. Chron columns are lab IDs, dated materials and reservoir ages:
#' real data, but not what a compilation's QC sheet is for, and the QC field
#' registry has no vocabulary for them.
#'
#' @param index From [lv_db_index()].
#' @param datasets Restrict to these dataset names.
#' @param tsids Restrict to these TSids.
#' @param axes Include depth, age and year columns. They are not curated, so the
#'   default is to leave them out of the sheet.
#' @return A character vector of TSids.
#' @export
lv_qc_timeseries <- function(index, datasets = NULL, tsids = NULL, axes = FALSE) {
  t <- index$timeseries
  if (!"tableType" %in% names(t)) return(if (is.null(tsids)) t$TSid else tsids)
  keep <- t$tableType %in% "paleo"
  # Axis columns are not curated. They were 2,667 of the 7,525 rows in the
  # hydroclimate2k sheet -- a third of it, all of it noise to whoever is working
  # through the tab. lipdverseR hid them and so does this.
  if (!axes && "variableName" %in% names(t)) {
    keep <- keep & !tolower(trimws(t$variableName)) %in% LV_AXIS_QC_VARIABLES
  }
  if (!is.null(datasets)) keep <- keep & t$dataSetName %in% datasets
  if (!is.null(tsids)) keep <- keep & t$TSid %in% tsids
  unique(t$TSid[keep])
}

#' Normalise text for comparison
#'
#' macOS returns filenames decomposed (NFD) while the metadata inside a LiPD file
#' is composed (NFC). The two render identically and compare unequal, which is
#' how four datasets came to be reported as absent from a database that held
#' them. Any comparison between a filename and a name from anywhere else has to
#' go through this.
#'
#' @param x A character vector.
#' @return `x`, in NFC.
#' @export
lv_nfc <- function(x) {
  if (!requireNamespace("stringi", quietly = TRUE)) return(x)
  stringi::stri_trans_nfc(x)
}

# Columns that carry the axis rather than a measurement. Not curated, so they do
# not belong in a QC sheet.
LV_AXIS_QC_VARIABLES <- c("year", "age", "depth", "yearbp", "agebp", "age14c",
                          "sampleid", "top", "bottom", "depthtop", "depthbottom",
                          "year ad", "cal age", "calendar age")

#' Resolve a compilation seed to TSids
#'
#' A compilation usually starts as a list someone has to hand, and that list is
#' as likely to be dataset names or datasetIds as TSids. This resolves any of
#' them to the TSids that membership is actually written against.
#'
#' `by` is required. Detecting the kind would mean deciding, on the caller's
#' behalf, what a list of strings means, and a wrong guess does not fail: it
#' produces a plausible compilation of the wrong things. [lv_seed_kind()]
#' answers the question when it is genuinely unknown, and reports rather than
#' acts.
#'
#' Values matching nothing are reported rather than dropped. A half-matching
#' list is the dangerous case, where the compilation comes out smaller than
#' intended and nothing says so.
#'
#' For a dataset-level seed the QC scope is used: paleo measurement columns,
#' axes excluded. A compilation of `year` and `depth` columns is not meaningful,
#' and including them would put them in the QC sheet, which is the thing the
#' hydroclimate2k curators asked to have removed. `scope = "all"` overrides.
#'
#' @param seed Character vector of TSids, datasetIds or dataSetNames.
#' @param index An `lv_index` from [lv_db_index()].
#' @param by Which of those the seed holds. Required; see [lv_seed_kind()].
#' @param scope For dataset-level seeds: `"qc"` for the QC scope, `"all"` for
#'   every timeseries in the dataset.
#' @return A list of `tsids`, the `by` used, the `matched` and `unmatched` seed
#'   values, and an `lv_issues` tibble.
#' @export
lv_resolve_seed <- function(seed, index, by, scope = c("qc", "all")) {
  if (missing(by)) {
    cli::cli_abort(c("{.arg by} is required: say whether the seed holds TSids, datasetIds or dataSetNames.",
                     i = "A wrong guess would not fail; it would build a plausible compilation of the wrong things.",
                     i = "Use {.code lv_seed_kind(seed, index)} if you are unsure what the list holds."),
                   class = "lv_error_compilation")
  }
  by <- match.arg(by, c("TSid", "datasetId", "dataSetName"))
  scope <- match.arg(scope)
  seed <- unique(stats::na.omit(as.character(seed)))
  seed <- seed[nzchar(trimws(seed))]
  if (!length(seed)) cli::cli_abort("Seed is empty.", class = "lv_error_compilation")

  ts <- index$timeseries
  pools <- lv_seed_pools(index)
  hits <- vapply(pools, function(p) sum(lv_nfc(seed) %in% lv_nfc(p)), integer(1))
  if (hits[[by]] == 0) {
    other <- names(hits)[hits > 0]
    cli::cli_abort(c("None of the {length(seed)} seed value{?s} matched a {by}.",
                     i = if (length(other)) "The list does match as {.val {other}}." else
                       "It matches nothing in the database.",
                     i = "See {.code lv_seed_kind(seed, index)}."),
                   class = "lv_error_compilation")
  }

  key <- lv_nfc(seed)
  matched <- seed[key %in% lv_nfc(pools[[by]])]
  unmatched <- setdiff(seed, matched)

  tsids <- if (by == "TSid") {
    ts$TSid[lv_nfc(ts$TSid) %in% lv_nfc(matched)]
  } else {
    col <- if (by == "datasetId") ts$datasetId else ts$dataSetName
    in_ds <- lv_nfc(col) %in% lv_nfc(matched)
    if (scope == "all") ts$TSid[in_ds] else {
      dsn <- unique(ts$dataSetName[in_ds])
      lv_qc_timeseries(index, datasets = dsn, axes = FALSE)
    }
  }
  tsids <- unique(stats::na.omit(tsids))

  issues <- if (length(unmatched)) {
    lv_issues(check = "seed_not_found", severity = "warn",
              message = sprintf("Seed value did not match any %s.", by),
              value = unmatched)
  } else lv_issues_empty()

  list(tsids = tsids, by = by, matched = matched, unmatched = unmatched,
       hits = hits, issues = issues)
}

# The three identifier pools a seed can be drawn from. Names are compared
# unicode-normalised: a dataset name differing only by composition is the same
# name, and macOS supplies both forms.
lv_seed_pools <- function(index) {
  ts <- index$timeseries; ds <- index$datasets
  list(TSid = unique(stats::na.omit(ts$TSid)),
       datasetId = unique(stats::na.omit(c(ts$datasetId, ds$datasetId))),
       dataSetName = unique(stats::na.omit(c(ts$dataSetName, ds$dataSetName,
                                             ds$fileDataSetName))))
}

#' What kind of identifiers does a list hold?
#'
#' Reports how many of the values match as TSids, datasetIds and dataSetNames,
#' so [lv_resolve_seed()] can be told which it is rather than guessing. Answers
#' the question; does not act on the answer.
#'
#' A list that matches two kinds is worth knowing about before it is used: it
#' usually means the list is mixed, or that it was pasted from two sources.
#'
#' @param seed Character vector.
#' @param index An `lv_index` from [lv_db_index()].
#' @return A tibble of one row per kind, with the match count and share.
#' @export
lv_seed_kind <- function(seed, index) {
  seed <- unique(stats::na.omit(as.character(seed)))
  seed <- seed[nzchar(trimws(seed))]
  if (!length(seed)) cli::cli_abort("Seed is empty.", class = "lv_error_compilation")
  pools <- lv_seed_pools(index)
  n <- vapply(pools, function(p) sum(lv_nfc(seed) %in% lv_nfc(p)), integer(1))
  tibble::tibble(kind = names(n), matched = unname(n), of = length(seed),
                 share = round(unname(n) / length(seed), 3))[order(-n), ]
}

# TSids in a dataset carrying a given compilation. Exact names throughout:
# `$` partial-matches, and inCompilation sits beside inCompilationBeta.
lv_membership_tsids <- function(L, compilation) {
  out <- character()
  for (blk in c("paleoData", "chronData")) {
    for (pd in L[[blk]]) for (tb in pd$measurementTable) {
      if (!is.list(tb)) next
      cols <- if (!is.null(tb[["columns"]])) tb[["columns"]] else
        tb[!names(tb) %in% c("filename", "tableName", "missingValue", "googWorkSheetKey")]
      for (cl in cols) {
        if (!is.list(cl) || is.null(cl[["TSid"]])) next
        for (nm in grep("^inCompilation", names(cl), value = TRUE)) {
          for (ic in cl[[nm]]) {
            if (!is.list(ic)) next
            cn <- unlist(ic[["compilationName"]])
            if (length(cn) && identical(as.character(cn)[1], compilation)) {
              out <- c(out, as.character(cl[["TSid"]])[1])
            }
          }
        }
      }
    }
  }
  unique(out)
}

lv_count_membership <- function(L, compilation) length(lv_membership_tsids(L, compilation))
