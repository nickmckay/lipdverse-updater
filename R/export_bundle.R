#' The download artefacts a compilation's page offers
#'
#' The tables are what a site queries; these are what a visitor clicks. The
#' legacy site shipped both from every version directory, and dropping them
#' would be a visible regression however good the parquet is.
#'
#' \describe{
#'   \item{the bundle}{`<compilation><version>.zip`, the member `.lpd` files,
#'     flat, exactly as lipdverse.org has always served them.}
#'   \item{the bibliography}{`<compilation>-<version>.bib`, one BibTeX entry per
#'     publication, for the reference page and for anyone citing the
#'     compilation.}
#' }
#'
#' The R, Python and MATLAB serializations the legacy site also carried are not
#' reproduced: they are derivable from the bundle, and Nick's call (2026-08-13)
#' is that they are luxuries rather than requirements.
#'
#' @section Members, not the considered set:
#' Both are scoped to datasets actually **in** the compilation. An export covers
#' the considered set, which is wider -- hydroclimate2k 0_6_2 exports 823
#' datasets of which 341 are members -- because the QC tab needs candidates
#' visible. A download called `hydroclimate2k.zip` containing 482 datasets that
#' are not in hydroclimate2k would be wrong in a way nobody would notice until
#' they used it.
#'
#' @name export_bundle
NULL

#' Zip a compilation's member files
#'
#' @param datasets Dataset names to include.
#' @param lipd_dir Directory holding the `.lpd` files.
#' @param path Output `.zip` path.
#' @param progress Show progress.
#' @return The path, invisibly, or `NULL` when there is nothing to bundle.
#' @export
lv_export_bundle <- function(datasets, lipd_dir = lv_path("database"), path,
                             progress = TRUE) {
  if (!length(datasets)) return(invisible(NULL))
  all <- fs::dir_ls(lipd_dir, glob = "*.lpd", type = "file")
  # NFC on both sides; a filename is decomposed and a dataset name is composed.
  keep <- lv_nfc(sub("\\.lpd$", "", fs::path_file(all))) %in% lv_nfc(datasets)
  files <- all[keep]
  missing <- setdiff(lv_nfc(datasets), lv_nfc(sub("\\.lpd$", "", fs::path_file(all))))
  if (length(missing)) {
    cli::cli_alert_warning("{length(missing)} dataset{?s} named for the bundle {?is/are} not in {.path {lipd_dir}}")
  }
  if (!length(files)) return(invisible(NULL))

  fs::dir_create(fs::path_dir(path))
  if (fs::file_exists(path)) fs::file_delete(path)
  if (progress) cli::cli_alert_info("Bundling {length(files)} file{?s} into {.path {fs::path_file(path)}}")
  # Flat, junking the paths, because that is how the site has always served it:
  # a visitor unzips into a directory of .lpd files, not into a copy of the
  # database's directory tree.
  utils::zip(path.expand(path), path.expand(as.character(files)), flags = "-qj")
  invisible(path)
}

#' Write a BibTeX bibliography for a compilation
#'
#' Built from the publications the export already carries rather than fetched
#' from DOIs. The legacy `.bib` was resolved through crossref, which is why it
#' has publishers and months this one does not; against that, this one needs no
#' network, cannot drift from the metadata it describes, and says exactly what
#' LiPDverse holds. A consumer wanting the richer record has the DOI.
#'
#' @param publications The `publications` table.
#' @param path Output `.bib` path.
#' @param datasets Optional dataset ids to restrict to, so the bibliography
#'   matches the bundle.
#' @param progress Show progress.
#' @return The path, invisibly.
#' @export
lv_export_bib <- function(publications, path, datasets = NULL, progress = TRUE) {
  p <- publications
  if (!is.null(datasets)) p <- p[p$datasetId %in% datasets, , drop = FALSE]
  nz <- function(x) !is.na(x) & nzchar(as.character(x))
  # A record with no author, no title and no DOI is not a reference, it is an
  # empty publication slot -- 377 of 1,073 have no title at all, which is what
  # issue #3 is about.
  authors_chr <- vapply(p$authors, function(a) paste(a[nzchar(a)], collapse = " and "), character(1))
  keep <- nz(authors_chr) | nz(p$title) | nz(p$doi)
  p <- p[keep, , drop = FALSE]; authors_chr <- authors_chr[keep]
  if (!nrow(p)) {
    fs::dir_create(fs::path_dir(path)); writeLines(character(), path)
    return(invisible(path))
  }

  # One entry per publication, not per dataset: the same paper describes several
  # datasets and a bibliography that repeated it would be unusable.
  sig <- paste(tolower(trimws(dplyr::coalesce(p$doi, ""))),
               tolower(trimws(dplyr::coalesce(p$title, ""))))
  first <- !duplicated(sig)
  p <- p[first, , drop = FALSE]; authors_chr <- authors_chr[first]

  key <- lv_bib_keys(authors_chr, p$year, p$title, p$datasetId, p$pubIndex)
  out <- character()
  for (i in seq_len(nrow(p))) {
    kind <- if (nz(p$journal[i])) "Article" else "Misc"
    fields <- c(
      author  = if (nz(authors_chr[i])) authors_chr[i] else NA_character_,
      title   = if (nz(p$title[i])) p$title[i] else NA_character_,
      journal = if (nz(p$journal[i])) p$journal[i] else NA_character_,
      year    = if (!is.na(p$year[i])) as.character(p$year[i]) else NA_character_,
      doi     = if (nz(p$doi[i])) p$doi[i] else NA_character_,
      url     = if (nz(p$doi[i])) paste0("https://doi.org/", p$doi[i]) else NA_character_)
    fields <- fields[!is.na(fields)]
    body <- paste0("  ", names(fields), " = {", lv_bib_escape(unname(fields)), "},")
    out <- c(out, paste0("@", kind, "{", key[i], ","), body, "}", "")
  }
  fs::dir_create(fs::path_dir(path))
  writeLines(out, path)
  if (progress) cli::cli_alert_info("Wrote {nrow(p)} reference{?s} to {.path {fs::path_file(path)}}")
  invisible(path)
}

# surname + year + a slug of the title, which is the shape the legacy keys took
# and is stable across runs. Uniqueness is enforced with a suffix rather than
# left to chance: two papers by one author in one year is ordinary.
lv_bib_keys <- function(authors, year, title, datasetId, pubIndex) {
  surname <- vapply(strsplit(dplyr::coalesce(authors, ""), "[,;]|\\band\\b"), function(x) {
    # strsplit("") returns nothing, so x[1] is NA for a publication with no
    # author. Left alone that produced the literal key "NA" -- non-empty, so the
    # fallback below never fired, and two author-less entries collided.
    if (!length(x) || is.na(x[1])) return("")
    s <- sub(".*\\s", "", trimws(x[1]))   # "N. J. Abram" -> "Abram"
    tolower(gsub("[^A-Za-z]", "", s))
  }, character(1))
  slug <- tolower(gsub("[^A-Za-z0-9]", "", substr(dplyr::coalesce(title, ""), 1, 60)))
  y <- ifelse(is.na(year), "", as.character(year))
  key <- paste0(surname, y, slug)
  # Nothing to build a key from at all: fall back to the identity, which always
  # exists and is what the entry describes.
  bare <- !nzchar(key)
  key[bare] <- paste0("lipdverse", tolower(gsub("[^A-Za-z0-9]", "", datasetId[bare])), pubIndex[bare])
  make.unique(key, sep = "")
}

# BibTeX's special characters. UTF-8 is left alone: biber handles it, and the
# legacy file's {\"o} escapes came from crossref rather than from any need.
lv_bib_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  for (ch in c("&", "%", "$", "#", "_")) x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  x <- gsub("}", "\\}", x, fixed = TRUE)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  trimws(gsub("[\r\n]+", " ", x))
}
