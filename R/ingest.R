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
