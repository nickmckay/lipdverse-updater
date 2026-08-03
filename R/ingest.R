#' Ingest new LiPD files into the database
#'
#' Contributors submit files that are not yet LiPDverse-ready: most carry no
#' `datasetId`, many carry no `TSid`s at all, and some carry identifiers copied
#' from whatever file was used as a template.
#'
#' Replaces `addLipdBatchToDatabase()` (`lipdverseR/R/addToDatabasee.R:303`),
#' which spent its effort on vocabulary and treated identity as a formality. It
#' seeded `usedTSids` from the empty set rather than from the database, so a
#' submitted file carrying a TSid that already exists in LiPDverse was staged
#' and committed, minting a duplicate. It also wrote straight into the live
#' database with no staging, verification or rollback.
#'
#' @name ingest
NULL

#' Read the identity of every column in a directory of LiPD files
#'
#' One row per column, whether or not it has a TSid, because the missing ones
#' are most of the work.
#'
#' @param dir Directory of `.lpd` files.
#' @param progress Show progress.
#' @return A tibble of `file`, `dataSetName`, `datasetId`, `block`, `paleo`,
#'   `table`, `column`, `variableName`, `TSid`.
#' @export
lv_ingest_scan <- function(dir, progress = TRUE) {
  paths <- fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (!length(paths)) cli::cli_abort("No {.file .lpd} files in {.path {dir}}")
  if (progress) cli::cli_alert_info("Scanning {length(paths)} incoming file{?s}")

  purrr::list_rbind(lapply(paths, function(p) {
    nm <- tryCatch(utils::unzip(p, list = TRUE)$Name, error = function(e) NULL)
    j <- grep("jsonld$", nm, value = TRUE)
    if (!length(j)) {
      return(tibble::tibble(file = fs::path_file(p), dataSetName = NA_character_,
                            datasetId = NA_character_, block = NA_character_,
                            paleo = NA_integer_, table = NA_integer_, column = NA_integer_,
                            variableName = NA_character_, TSid = NA_character_,
                            error = "no jsonld member"))
    }
    con <- unz(p, j[1])
    m <- tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                     simplifyVector = FALSE), error = function(e) NULL)
    close(con)
    if (is.null(m)) {
      return(tibble::tibble(file = fs::path_file(p), dataSetName = NA_character_,
                            datasetId = NA_character_, block = NA_character_,
                            paleo = NA_integer_, table = NA_integer_, column = NA_integer_,
                            variableName = NA_character_, TSid = NA_character_,
                            error = "unparseable jsonld"))
    }
    rows <- list()
    for (blk in c("paleoData", "chronData")) {
      for (pi in seq_along(m[[blk]])) {
        # Model tables carry TSids too, and an identifier collision there counts
        # exactly as much as one in a measurement table.
        meas <- m[[blk]][[pi]]$measurementTable
        modl <- unlist(lapply(m[[blk]][[pi]]$model, function(md) {
          c(md$summaryTable, md$ensembleTable, md$distributionTable)
        }), recursive = FALSE)
        tabs <- c(meas, modl)
        kinds <- c(rep("measurement", length(meas)), rep("model", length(modl)))
        for (ti in seq_along(tabs)) {
          tb <- tabs[[ti]]
          if (!is.list(tb)) next
          cols <- if (!is.null(tb$columns)) tb$columns else tb
          ci <- 0L
          for (cl in cols) {
            if (!is.list(cl) || is.null(cl$variableName)) next
            ci <- ci + 1L
            rows[[length(rows) + 1L]] <- tibble::tibble(
              file = fs::path_file(p),
              dataSetName = as_chr1(m$dataSetName) %||% NA_character_,
              datasetId = as_chr1(m$datasetId) %||% NA_character_,
              block = blk, paleo = pi, table = ti, kind = kinds[ti], column = ci,
              variableName = as_chr1(cl$variableName) %||% NA_character_,
              TSid = as_chr1(cl$TSid) %||% NA_character_,
              error = NA_character_)
          }
        }
      }
    }
    if (!length(rows)) return(NULL)
    purrr::list_rbind(rows)
  }))
}

#' Decide what to do with every incoming TSid
#'
#' A TSid that already exists in LiPDverse means one of two opposite things, and
#' the difference decides whether it is preserved or replaced:
#'
#' * The file **is** that dataset — its `datasetId` matches, or it has no
#'   `datasetId` yet and its `dataSetName` matches. This is an update, and the
#'   TSid must be kept so the timeseries keeps its identity and its compilation
#'   memberships.
#' * The file is **not** that dataset. Then someone has used an existing file as
#'   a template and left its identifiers in place. The TSid is re-minted.
#'
#' The same reasoning applies inside a batch: a TSid repeated across submissions
#' that no existing dataset claims is a copied template, and every occurrence is
#' re-minted rather than one being arbitrarily blessed.
#'
#' @param scan From [lv_ingest_scan()].
#' @param index An `lv_index` of the live database.
#' @return `scan` with `action` (`keep`, `mint`, `remint`), `reason`, and the
#'   resolved `new_TSid`.
#' @export
lv_ingest_identity <- function(scan, index) {
  x <- scan[is.na(scan$error), , drop = FALSE]
  if (!nrow(x)) return(dplyr::mutate(scan, action = character(), reason = character(),
                                     new_TSid = character()))

  # Which database dataset, if any, currently holds each incoming TSid.
  claim_dsn <- stats::setNames(index$timeseries$dataSetName, index$timeseries$TSid)
  ds_id     <- stats::setNames(index$datasets$datasetId, index$datasets$fileDataSetName)
  x$claim_dataSetName <- unname(claim_dsn[x$TSid])
  x$claim_datasetId   <- unname(ds_id[x$claim_dataSetName])

  claimed <- !is.na(x$TSid) & !is.na(x$claim_dataSetName)
  # The file is the dataset that holds the TSid: an update.
  same_id   <- !is.na(x$datasetId) & !is.na(x$claim_datasetId) & x$datasetId == x$claim_datasetId
  same_name <- is.na(x$datasetId) & !is.na(x$claim_dataSetName) & x$dataSetName == x$claim_dataSetName
  is_update <- claimed & (same_id | same_name)

  # Repeated across submissions and claimed by no existing dataset. Compared on
  # distinct (file, TSid) so a TSid legitimately appearing twice inside one file
  # is not mistaken for cross-file reuse.
  pairs <- unique(x[!is.na(x$TSid) & !claimed, c("file", "TSid")])
  reused <- !is.na(x$TSid) & !claimed & x$TSid %in% pairs$TSid[duplicated(pairs$TSid)]

  x$action <- dplyr::case_when(
    is.na(x$TSid)          ~ "mint",
    is_update              ~ "keep",
    claimed & !is_update   ~ "remint",
    reused                 ~ "remint",
    TRUE                   ~ "keep")
  x$reason <- dplyr::case_when(
    x$action == "mint"                    ~ "no TSid on the incoming column",
    x$action == "keep" & is_update        ~ "update: the file is the dataset holding this TSid",
    x$action == "keep"                    ~ "new TSid, unique",
    claimed & !is_update                  ~ "TSid belongs to a different dataset; template reuse",
    TRUE                                  ~ "TSid repeated across submissions; template reuse")

  # Mint against everything already spoken for, so a new id cannot collide with
  # the database, with a kept TSid, or with another minted one.
  taken <- unique(c(index$timeseries$TSid, x$TSid[x$action == "keep"]))
  need <- which(x$action %in% c("mint", "remint"))
  x$new_TSid <- x$TSid
  for (i in need) {
    repeat {
      id <- lipdR::createTSid()
      if (!id %in% taken) break
    }
    taken <- c(taken, id)
    x$new_TSid[i] <- id
  }

  dplyr::bind_rows(x, dplyr::mutate(scan[!is.na(scan$error), , drop = FALSE],
                                    action = "error", reason = scan$error[!is.na(scan$error)],
                                    new_TSid = NA_character_))
}

#' Report an identity plan as issues
#' @param plan From [lv_ingest_identity()].
#' @return An `lv_issues` tibble.
#' @export
lv_ingest_issues <- function(plan) {
  bad <- plan[plan$action %in% c("remint", "error"), , drop = FALSE]
  if (!nrow(bad)) return(lv_issues_empty())
  lv_issues(
    check = ifelse(bad$action == "error", "unreadable", "tsid_template_reuse"),
    severity = ifelse(bad$action == "error", "error", "warn"),
    message = bad$reason,
    dataSetName = bad$dataSetName, TSid = bad$TSid,
    field = bad$variableName, path = bad$file)
}

#' Walk a LiPD object's columns in the same order as [lv_ingest_scan()]
#' @keywords internal
lv_ingest_walk <- function(L) {
  out <- list()
  for (blk in c("paleoData", "chronData")) {
    for (pi in seq_along(L[[blk]])) {
      meas <- L[[blk]][[pi]]$measurementTable
      modl <- unlist(lapply(L[[blk]][[pi]]$model, function(md) {
        c(md$summaryTable, md$ensembleTable, md$distributionTable)
      }), recursive = FALSE)
      tabs <- c(meas, modl)
      for (ti in seq_along(tabs)) {
        tb <- tabs[[ti]]
        if (!is.list(tb)) next
        cols <- if (!is.null(tb$columns)) tb$columns else tb
        for (cn in seq_along(cols)) {
          cl <- cols[[cn]]
          if (!is.list(cl) || is.null(cl$variableName)) next
          out[[length(out) + 1L]] <- list(
            blk = blk, pi = pi, ti = ti, n_meas = length(meas),
            name = if (!is.null(names(cols))) names(cols)[cn] else NA_character_,
            idx = cn, has_columns = !is.null(tb$columns),
            variableName = as_chr1(cl$variableName))
        }
      }
    }
  }
  out
}

#' Write the resolved identity into staged copies of the incoming files
#'
#' Never writes in place. Assigns the TSids from [lv_ingest_identity()] and a
#' `datasetId` to any file lacking one, then stages for [lv_promote()].
#'
#' Columns are matched by position within a deterministic walk, because
#' `variableName` is not unique inside a table -- one incoming file carries four
#' `d13C` columns and five `d2H`. The walk is verified against the plan before
#' anything is assigned, so a file whose structure does not line up is reported
#' and skipped rather than silently mis-assigned.
#'
#' @param plan From [lv_ingest_identity()].
#' @param dir Directory holding the incoming files.
#' @param out Staging directory.
#' @param index An `lv_index`, so a minted `datasetId` cannot collide.
#' @param progress Show progress.
#' @return A list of `staged` files, `skipped`, and `issues`.
#' @export
lv_ingest_apply <- function(plan, dir, out, index, progress = TRUE) {
  if (missing(out)) cli::cli_abort("{.arg out} is required; this never writes in place.")
  fs::dir_create(out)
  plan <- plan[plan$action != "error", , drop = FALSE]
  files <- unique(plan$file)
  if (progress) cli::cli_alert_info("Applying identity to {length(files)} file{?s}")

  taken_ds <- unique(stats::na.omit(index$datasets$datasetId))
  issues <- lv_issues_empty()
  staged <- character(); skipped <- character()

  for (f in files) {
    p <- fs::path(dir, f)
    L <- tryCatch(suppressWarnings(lipdR::readLipd(p)), error = function(e) NULL)
    if (is.null(L)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "unreadable", severity = "error",
        message = "Could not read the incoming file.", path = f))
      skipped <- c(skipped, f); next
    }

    want <- plan[plan$file == f, , drop = FALSE]
    walk <- lv_ingest_walk(L)
    # The guard that makes positional assignment safe. One real submission
    # declares 13 columns but leaves the table's filename empty, so readLipd
    # returns none of them; assigning positionally there would be nonsense.
    if (length(walk) != nrow(want) ||
        !identical(vapply(walk, function(w) w$variableName %||% NA_character_, character(1)),
                   want$variableName)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "structure_mismatch", severity = "error",
        message = sprintf("Metadata declares %d columns but the file yields %d; not staged.",
                          nrow(want), length(walk)),
        dataSetName = want$dataSetName[1], path = f))
      skipped <- c(skipped, f); next
    }

    for (i in seq_along(walk)) {
      w <- walk[[i]]
      tsid <- want$new_TSid[i]
      if (is.na(tsid)) next
      if (w$ti <= w$n_meas) {
        tb <- L[[w$blk]][[w$pi]]$measurementTable[[w$ti]]
        if (w$has_columns) tb$columns[[w$idx]]$TSid <- tsid else tb[[w$name]]$TSid <- tsid
        L[[w$blk]][[w$pi]]$measurementTable[[w$ti]] <- tb
      } else {
        # Model tables are nested a level deeper; find the same flat position.
        k <- w$ti - w$n_meas
        seen <- 0L
        for (mi in seq_along(L[[w$blk]][[w$pi]]$model)) {
          md <- L[[w$blk]][[w$pi]]$model[[mi]]
          for (slot in c("summaryTable", "ensembleTable", "distributionTable")) {
            for (si in seq_along(md[[slot]])) {
              seen <- seen + 1L
              if (seen != k) next
              tb <- md[[slot]][[si]]
              if (w$has_columns) tb$columns[[w$idx]]$TSid <- tsid else tb[[w$name]]$TSid <- tsid
              L[[w$blk]][[w$pi]]$model[[mi]][[slot]][[si]] <- tb
            }
          }
        }
      }
    }

    if (is.null(L$datasetId) || is.na(as_chr1(L$datasetId) %||% NA_character_)) {
      repeat {
        id <- lipdR::createDatasetId()
        if (!id %in% taken_ds) break
      }
      taken_ds <- c(taken_ds, id)
      L$datasetId <- id
    }

    lipdR::writeLipd(L, path = out, removeNamesFromLists = TRUE)

    # Verify by re-reading, because the loss can happen on write. One submission
    # leaves its measurement table's filename empty: readLipd returns all 13
    # columns of metadata but no values, and writeLipd then emits a dataset with
    # one column and no CSV member at all -- 2 KB where a dataset should be.
    # Checking the plan against the read would never catch that.
    dsn <- as_chr1(L$dataSetName) %||% sub("\\.lpd$", "", f)
    sp <- fs::path(out, paste0(dsn, ".lpd"))
    back <- if (fs::file_exists(sp)) tryCatch(suppressWarnings(lipdR::readLipd(sp)),
                                              error = function(e) NULL) else NULL
    got <- if (is.null(back)) list() else lv_ingest_walk(back)
    has_csv <- fs::file_exists(sp) &&
      any(grepl("[.]csv$", tryCatch(utils::unzip(sp, list = TRUE)$Name, error = function(e) character())))
    if (length(got) != nrow(want) || (nrow(want) > 0 && !has_csv)) {
      issues <- lv_issues_bind(issues, lv_issues(
        check = "write_lost_columns", severity = "error",
        message = sprintf("Staged file has %d of %d columns%s; not usable.",
                          length(got), nrow(want),
                          if (!has_csv) " and no data file" else ""),
        dataSetName = dsn, path = f))
      if (fs::file_exists(sp)) fs::file_delete(sp)
      skipped <- c(skipped, f); next
    }
    staged <- c(staged, f)
  }

  list(staged = staged, skipped = skipped, issues = issues)
}

# Names that are clearly a template left unfilled. `author.1111` and
# `lastname.year` are both present in the live database, 139 datasets between
# them, so this is not hypothetical.
#
# `author` on its own is deliberately not here. It occupies the surname slot of
# the convention, so matching it flags any dataset whose author is actually
# named Author -- and `X.author.1111` is caught anyway by its impossible year.
LV_NAME_PLACEHOLDER <- "(^|[.])(lastname|firstname|unknown|tbd|xxx)([.]|$)|[.]year$"

#' Validate incoming names and metadata before they enter LiPDverse
#'
#' Severity is calibrated against the live database rather than an ideal, so the
#' gate rejects what LiPDverse genuinely does not contain and merely reports what
#' it contains but would rather not:
#'
#' * `Site.Author.YYYY` covers 82% of the database, not all of it — the rest are
#'   legitimate short codes like `AB08MEN01`. A non-conforming name is a warning.
#' * Every one of the 7,177 datasets has an `archiveType` and coordinates, so
#'   their absence is an error.
#' * An implausible year is an error even though 107 existing datasets fail it.
#'   They exist because there was no gate; using them to justify admitting more
#'   would be circular.
#' * No dataset name contains a filesystem-hostile character, so that is an
#'   error. Spaces and non-ASCII do occur (7 and 15 datasets), so they warn.
#'
#' `writeLipd()` names its output from `dataSetName`, not from the incoming
#' filename, and every dataset in the database satisfies `dataSetName == file`.
#' So a disagreement is not an error, but it does mean the file lands under a
#' different name than it arrived with, which is worth saying out loud.
#'
#' @param dir Directory of incoming `.lpd` files.
#' @param index An `lv_index` of the live database.
#' @param progress Show progress.
#' @return An `lv_issues` tibble.
#' @export
lv_ingest_validate <- function(dir, index, progress = TRUE) {
  paths <- fs::dir_ls(dir, glob = "*.lpd", type = "file")
  if (progress) cli::cli_alert_info("Validating {length(paths)} incoming file{?s}")
  issues <- lv_issues_empty()
  add <- function(check, severity, message, file, dsn = NA_character_, value = NA_character_) {
    issues <<- lv_issues_bind(issues, lv_issues(
      check = check, severity = severity, message = message,
      dataSetName = dsn, value = value, path = file))
  }

  for (p in paths) {
    f <- fs::path_file(p)
    nm <- tryCatch(utils::unzip(p, list = TRUE)$Name, error = function(e) NULL)
    j <- grep("jsonld$", nm, value = TRUE)
    if (!length(j)) { add("unreadable", "error", "No jsonld member.", f); next }
    con <- unz(p, j[1])
    m <- tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                     simplifyVector = FALSE), error = function(e) NULL)
    close(con)
    if (is.null(m)) { add("unreadable", "error", "Unparseable jsonld.", f); next }

    dsn <- as_chr1(m$dataSetName) %||% NA_character_

    # ---- the name ----------------------------------------------------------
    if (is.na(dsn) || !nzchar(dsn)) {
      add("name_missing", "error", "No dataSetName; the file cannot be named.", f)
    } else {
      if (grepl("[/\\:*?\"<>|]", dsn)) {
        add("name_illegal_characters", "error",
            "dataSetName contains characters that cannot appear in a filename.", f, dsn, dsn)
      }
      if (grepl(LV_NAME_PLACEHOLDER, dsn, ignore.case = TRUE)) {
        add("name_placeholder", "error",
            "dataSetName still contains a template placeholder.", f, dsn, dsn)
      }
      if (grepl(" ", dsn)) {
        add("name_has_space", "warn",
            "dataSetName contains a space; LiPDverse names do not, by convention.", f, dsn, dsn)
      }
      if (grepl("[^ -~]", dsn)) {
        add("name_non_ascii", "warn",
            "dataSetName contains non-ASCII characters, which travel badly through URLs and sheets.",
            f, dsn, dsn)
      }
      if (!grepl("^[A-Za-z0-9_-]+[.][A-Za-z0-9_-]+[.][0-9]{4}$", dsn)) {
        add("name_not_conventional", "warn",
            "dataSetName is not Site.Author.YYYY. Legitimate for a short code, otherwise worth fixing.",
            f, dsn, dsn)
      }
      yr <- suppressWarnings(as.integer(sub("^.*[.]", "", dsn)))
      if (grepl("[.][0-9]{4}$", dsn) && (is.na(yr) || yr < 1500 || yr > 2030)) {
        # An error, not a warning, even though 107 datasets in the database
        # already fail it -- almost all of them `.1111`, the placeholder year.
        # Those exist because there was no gate; using them to justify letting
        # more through would be circular.
        add("name_year_implausible", "error",
            "The year in dataSetName is outside 1500-2030, which usually means a template placeholder.",
            f, dsn, as.character(yr))
      }
      if (!identical(paste0(dsn, ".lpd"), f)) {
        add("name_file_mismatch", "info",
            sprintf("Will be written as %s.lpd, not %s.", dsn, f), f, dsn, dsn)
      }
      # A name already in LiPDverse is only acceptable when this file is that
      # dataset; otherwise two different records would share a name.
      hit <- match(dsn, index$datasets$fileDataSetName)
      if (!is.na(hit)) {
        did <- as_chr1(m$datasetId) %||% NA_character_
        if (is.na(did) || identical(did, index$datasets$datasetId[hit])) {
          add("name_existing_update", "info",
              "Matches an existing dataset; will be treated as an update.", f, dsn, dsn)
        } else {
          add("name_collision", "error",
              "dataSetName already belongs to a different dataset in LiPDverse.", f, dsn,
              index$datasets$datasetId[hit])
        }
      }
    }

    # ---- required metadata -------------------------------------------------
    if (is.null(as_chr1(m$archiveType))) {
      add("archiveType_missing", "error",
          "No archiveType. Every dataset in LiPDverse has one.", f, dsn)
    }
    # Coordinates live in GeoJSON in the database, but a submission may put them
    # directly on geo. Accept either rather than reporting a file as having no
    # position when it plainly does.
    # Always length one: an absent key unlists to NULL, and is.na(numeric(0))
    # is logical(0), which makes the guard below error rather than report.
    num <- function(x) {
      v <- suppressWarnings(as.numeric(unlist(x)[1]))
      if (length(v) != 1) NA_real_ else v
    }
    co <- m$geo$geometry$coordinates
    lon <- if (length(co) >= 1) num(co[1]) else num(m$geo$longitude)
    lat <- if (length(co) >= 2) num(co[2]) else num(m$geo$latitude)
    if (is.na(lat) || is.na(lon)) {
      add("coordinates_missing", "error",
          "No usable coordinates. Every dataset in LiPDverse has them.", f, dsn)
    } else {
      if (abs(lat) > 90) {
        add("latitude_out_of_range", "error", "Latitude is outside -90 to 90.", f, dsn, as.character(lat))
      }
      if (lon < -180 || lon > 360) {
        add("longitude_out_of_range", "error", "Longitude is outside -180 to 360.", f, dsn, as.character(lon))
      }
    }
    if (!length(m$pub)) {
      add("no_publication", "warn", "No publication recorded.", f, dsn)
    }
  }
  issues
}
