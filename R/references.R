#' The bibliographic reference database
#'
#' A publication's record in a LiPD file is usually thin -- of hydroclimate2k's
#' 1,073 publication entries, 967 carry a DOI but only 696 a title -- so a
#' bibliography built from the files alone is missing a third of its titles.
#'
#' lipdverseR solved this by resolving each DOI through crossref once and
#' accumulating the results, so the network was paid for a reference the first
#' time it appeared and never again. That is the right shape and this keeps it.
#' What changes is where the accumulation lives: it was a Google Sheet, which is
#' the pattern this rewrite exists to replace -- no history, no review, and a
#' hard stop telling a human to go and fix duplicate citekeys by hand. Here it is
#' a table in the QC store, git-tracked like everything else, so a correction is
#' a reviewable commit and a bad edit can be undone.
#'
#' @section Three tiers, in order:
#' \describe{
#'   \item{`crossref`}{Resolved from the DOI. 4,822 of these were inherited from
#'     the legacy database rather than re-resolved, which is 78% of
#'     hydroclimate2k's publications and worth 337 titles and 381 journals it
#'     would otherwise lack.}
#'   \item{`curated`}{Entered by hand, for works with no DOI -- books, reports,
#'     theses. The legacy equivalent was `additionalLipdverse.bib`.}
#'   \item{the file}{Whatever the LiPD record holds, with `Missing Title` and
#'     friends where it holds nothing. lipdverseR did this too: a visible gap is
#'     worth more than a silent one.}
#' }
#'
#' @name references
NULL

# The BibTeX fields worth keeping. The legacy sheet had 44 columns of which 23
# were entirely empty and several (x_arraytype_, refid, data) were never
# bibliographic at all.
LV_BIB_FIELDS <- c("bibtype", "author", "title", "year", "journal", "booktitle",
                   "publisher", "volume", "number", "pages", "month", "doi",
                   "url", "editor", "issn", "language", "keywords", "note")

#' Where the reference database lives
#' @param store A QC store.
#' @return A path.
#' @export
lv_references_path <- function(store = qc_store()) {
  fs::path(store$path, "references", "references.csv")
}

#' Read the reference database
#'
#' @param store A QC store.
#' @return A tibble of `citekey`, `source` and the BibTeX fields; empty when
#'   nothing has been imported yet.
#' @export
lv_references <- function(store = qc_store()) {
  p <- lv_references_path(store)
  empty <- tibble::as_tibble(stats::setNames(
    rep(list(character()), length(LV_BIB_FIELDS) + 3L),
    c("citekey", "source", "added_at", LV_BIB_FIELDS)))
  if (!fs::file_exists(p)) return(empty)
  x <- readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE)
  for (k in names(empty)) if (!k %in% names(x)) x[[k]] <- NA_character_
  x[, names(empty), drop = FALSE]
}

#' Add references to the database
#'
#' Appends what is new and leaves what is there. A citekey already present is
#' not overwritten: the stored copy may carry a correction, and silently
#' replacing it would undo by import what somebody fixed by hand.
#'
#' @param refs A tibble carrying at least `citekey`.
#' @param store A QC store.
#' @param source Provenance for the new rows: `crossref`, `curated` or `lipd`.
#' @param dry_run Report without writing.
#' @return A list of `added`, `kept` and `path`.
#' @export
lv_references_add <- function(refs, store = qc_store(), source = "crossref",
                              dry_run = TRUE) {
  have <- lv_references(store)
  refs <- tibble::as_tibble(refs)
  if (!"citekey" %in% names(refs)) {
    cli::cli_abort("References need a {.field citekey}.", class = "lv_error_references")
  }
  for (k in c("source", "added_at", LV_BIB_FIELDS)) {
    if (!k %in% names(refs)) refs[[k]] <- NA_character_
  }
  refs <- refs[, c("citekey", "source", "added_at", LV_BIB_FIELDS), drop = FALSE]
  refs[] <- lapply(refs, function(v) {
    v <- as.character(v); v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_; v
  })
  refs$source[is.na(refs$source)] <- source
  refs$added_at[is.na(refs$added_at)] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  refs <- refs[!is.na(refs$citekey) & nzchar(refs$citekey), , drop = FALSE]
  refs <- refs[!duplicated(refs$citekey), , drop = FALSE]
  new <- refs[!refs$citekey %in% have$citekey, , drop = FALSE]

  out <- dplyr::bind_rows(have, new)
  out <- out[order(out$citekey), , drop = FALSE]
  if (!dry_run) {
    fs::dir_create(fs::path_dir(lv_references_path(store)))
    readr::write_csv(out, lv_references_path(store), na = "")
  }
  list(added = nrow(new), kept = nrow(have), path = lv_references_path(store))
}

#' Parse a BibTeX file into reference rows
#'
#' Enough of BibTeX to read the hand-curated overrides: entries, a citekey, and
#' `field = {value}` pairs. Not a general parser, and it does not need to be --
#' the file it exists for has 16 entries.
#'
#' @param path A `.bib` file.
#' @return A tibble of reference rows.
#' @export
lv_references_read_bib <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  starts <- gregexpr("@[A-Za-z]+\\s*\\{", txt)[[1]]
  if (starts[1] == -1) return(tibble::tibble(citekey = character()))
  ends <- c(starts[-1] - 1L, nchar(txt))
  rows <- lapply(seq_along(starts), function(i) {
    chunk <- substr(txt, starts[i], ends[i])
    bibtype <- sub("^@([A-Za-z]+).*$", "\\1", chunk)
    key <- sub("^@[A-Za-z]+\\s*\\{\\s*([^,]+),.*$", "\\1", sub("\n", " ", chunk))
    mm <- gregexpr("([A-Za-z]+)\\s*=\\s*\\{", chunk)
    m <- mm[[1]]
    if (m[1] == -1) return(NULL)
    labels <- regmatches(chunk, mm)[[1]]
    fields <- list()
    for (j in seq_along(m)) {
      nm <- tolower(sub("\\s*=\\s*\\{$", "", labels[j]))
      # Walk the braces, so a value containing {} is not truncated at the first
      # closing brace.
      pos <- m[j] + attr(m, "match.length")[j]; depth <- 1L; buf <- ""
      while (pos <= nchar(chunk) && depth > 0L) {
        ch <- substr(chunk, pos, pos)
        if (ch == "{") depth <- depth + 1L else if (ch == "}") depth <- depth - 1L
        if (depth > 0L) buf <- paste0(buf, ch)
        pos <- pos + 1L
      }
      fields[[nm]] <- trimws(gsub("\\s+", " ", buf))
    }
    tibble::as_tibble(c(list(citekey = trimws(key), bibtype = bibtype), fields))
  })
  out <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  keep <- intersect(c("citekey", LV_BIB_FIELDS), names(out))
  out[, keep, drop = FALSE]
}

#' Resolve publications against the reference database
#'
#' @param publications The `publications` table.
#' @param refs From [lv_references()].
#' @return `publications` with the stored fields filled in where the DOI matched,
#'   plus `citekey` and `ref_source` columns saying which tier answered.
#' @export

#' Which references a dataset cites
#'
#' A reference resolves to a publication by DOI where there is one, and 1,623
#' publication rows have no usable DOI at all. The link that answers for those
#' is per dataset rather than per publication: the legacy site published
#' `bibDsid.json`, a map of datasetId to the citekeys that dataset cites, and
#' 817 of the 1,308 datasets with no DOI have a curated citation waiting behind
#' it.
#'
#' Stored beside the references themselves so the store answers both questions:
#' what a reference says, and which datasets cite it.
#'
#' @param store A QC store.
#' @return A tibble of `datasetId`, `citekey`, `rank`, `source`, `added_at`.
#' @export
lv_reference_links <- function(store = qc_store()) {
  p <- fs::path(store$path, "references", "dataset-citekeys.csv")
  if (!fs::file_exists(p)) {
    return(tibble::tibble(datasetId = character(), citekey = character(),
                          rank = integer(), source = character(), added_at = character()))
  }
  readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                  progress = FALSE) |>
    dplyr::mutate(rank = suppressWarnings(as.integer(.data$rank)))
}

#' @param links A tibble of `datasetId` and `citekey`, in citation order.
#' @param source Where the link came from.
#' @rdname lv_reference_links
#' @export
lv_reference_links_add <- function(links, store = qc_store(), source = "bibDsid") {
  stopifnot(all(c("datasetId", "citekey") %in% names(links)))
  cur <- lv_reference_links(store)
  new <- tibble::as_tibble(links)
  if (is.null(new$rank)) {
    new <- new |> dplyr::group_by(.data$datasetId) |>
      dplyr::mutate(rank = dplyr::row_number()) |> dplyr::ungroup()
  }
  new$source <- source
  new$added_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out <- dplyr::bind_rows(cur, new)
  # Last write wins per (dataset, citekey), so re-importing is idempotent.
  out <- out[!duplicated(paste(out$datasetId, out$citekey), fromLast = TRUE), , drop = FALSE]
  out <- out[order(out$datasetId, out$rank), , drop = FALSE]
  p <- fs::path(store$path, "references", "dataset-citekeys.csv")
  fs::dir_create(fs::path_dir(p))
  readr::write_csv(out, p, na = "")
  invisible(out)
}

lv_resolve_references <- function(publications, refs, links = NULL) {
  p <- tibble::as_tibble(publications)
  # NA must stay NA. as.character(NA) is "NA", which lowercases to "na" and then
  # matches every other missing DOI: with 1,081 references carrying no DOI, a
  # publication without one was joining to whichever of them came first and
  # taking its author, title and year. Blank counts as missing too.
  norm_doi <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x[is.na(x) | !nzchar(x) | x %in% c("na", "nan", "none", "null")] <- NA_character_
    sub("^https?://(dx\\.)?doi\\.org/", "", x)
  }
  p$ref_source <- NA_character_
  p$citekey <- NA_character_
  if (!nrow(p) || !nrow(refs)) return(p)

  # incomparables: match() pairs NA with NA by default, so a publication with no
  # DOI would join to a reference with no DOI -- 1,081 of them in the store --
  # and take its author, title and year. A missing identifier identifies nothing.
  i <- match(norm_doi(p$doi), norm_doi(refs$doi), incomparables = NA)

  # Where the DOI cannot answer -- 1,623 publication rows carry none -- fall
  # back to what the DATASET cites. lv_reference_links() holds datasetId ->
  # citekey in citation order, so publication N of a dataset takes that
  # dataset's Nth citation. That is the same ordering the files use (pub1,
  # pub2, ...), and it is the only link available for a record with no DOI.
  if (!is.null(links) && nrow(links) && !is.null(p$datasetId) && !is.null(p$pubIndex)) {
    ord <- links[order(links$datasetId, links$rank), , drop = FALSE]
    key <- paste(ord$datasetId, ord$rank)
    want <- paste(p$datasetId, suppressWarnings(as.integer(p$pubIndex)))
    j <- match(want, key)
    ck <- ifelse(is.na(j), NA_character_, ord$citekey[j])
    k <- match(ck, refs$citekey, incomparables = NA)
    use <- is.na(i) & !is.na(k)
    if (any(use)) i[use] <- k[use]
  }
  hit <- !is.na(i)
  # A stored record answers for the fields the file lacks; the file is not
  # overwritten where it has something, because the file is what a curator edits.
  for (k in c("title", "journal", "year")) {
    if (!k %in% names(p)) next
    have <- !is.na(p[[k]]) & (!is.character(p[[k]]) | nzchar(as.character(p[[k]])))
    take <- hit & !have & !is.na(refs[[k]][i])
    if (any(take)) {
      p[[k]][take] <- if (identical(k, "year")) {
        suppressWarnings(as.integer(refs[[k]][i][take]))
      } else refs[[k]][i][take]
    }
  }
  # Authors only when the file has none: a list column, so it is replaced whole.
  # Arrow hands back a list_of<character>, which refuses a plain list assigned
  # into it, so the column becomes an ordinary list first.
  if (!is.null(p$authors)) p$authors <- as.list(p$authors)
  no_authors <- lengths(p$authors) == 0
  take <- hit & no_authors & !is.na(refs$author[i])
  if (any(take)) {
    p$authors[take] <- lapply(refs$author[i][take],
                              function(a) trimws(strsplit(a, "\\band\\b")[[1]]))
  }
  p$citekey[hit] <- refs$citekey[i][hit]
  p$ref_source[hit] <- refs$source[i][hit]
  p
}
