#' The PaST thesaurus
#'
#' NOAA's Paleoenvironmental Standard Terms thesaurus, as SKOS JSON-LD. The
#' vocabulary alignment sheets carry `paleoData_pastName` and `paleoData_pastId`
#' columns, so a term proposed for the vocabulary is more useful with a PaST
#' alignment attached than without one.
#'
#' Parsed once into a flat tibble and cached, because `past.json` is 3.3 MB of
#' deeply nested JSON and re-parsing it per lookup is what made
#' `getStandardTables()` slow in lipdverseR.
#'
#' @param path The `past.json` file.
#' @param cache Cache file; `NULL` to skip caching.
#' @return A tibble of `pastId`, `pastName`, `altLabels`, `definition`, `broader`.
#' @export
lv_past <- function(path = lv_past_path(),
                    cache = fs::path(lv_path("state"), "cache", "past.rds")) {
  if (!is.null(cache) && fs::file_exists(cache)) {
    p <- readRDS(cache)
    if (identical(attr(p, "source_md5"), unname(tools::md5sum(path)))) return(p)
  }
  if (!fs::file_exists(path)) {
    cli::cli_abort(c("PaST thesaurus not found at {.path {path}}.",
                     i = "Set {.envvar LIPDVERSE_PAST} to its location."),
                   class = "lv_error_vocab")
  }
  g <- jsonlite::fromJSON(path, simplifyVector = FALSE)[["@graph"]]
  S <- function(x) paste0("http://www.w3.org/2004/02/skos/core#", x)

  val <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (!is.null(x[["@value"]])) return(as.character(x[["@value"]]))
    # altLabel is sometimes a list of label objects rather than one.
    v <- vapply(x, function(z) if (is.list(z) && !is.null(z[["@value"]]))
      as.character(z[["@value"]]) else NA_character_, character(1))
    v <- stats::na.omit(v)
    if (!length(v)) NA_character_ else paste(v, collapse = " | ")
  }
  id_of <- function(u) {
    if (is.null(u)) return(NA_character_)
    if (is.list(u)) u <- u[["@id"]]
    sub("^.*termId=", "", as.character(u)[1])
  }

  keep <- vapply(g, function(e) identical(e[["@type"]], S("Concept")), logical(1))
  g <- g[keep]

  out <- tibble::tibble(
    pastId     = vapply(g, function(e) id_of(e[["@id"]]), character(1)),
    pastName   = vapply(g, function(e) val(e[[S("prefLabel")]]), character(1)),
    altLabels  = vapply(g, function(e) val(e[[S("altLabel")]]), character(1)),
    definition = vapply(g, function(e) val(e[[S("definition")]]), character(1)),
    broader    = vapply(g, function(e) id_of(e[[S("broader")]]), character(1)))
  out <- out[!is.na(out$pastName), , drop = FALSE]

  attr(out, "source_md5") <- unname(tools::md5sum(path))
  if (!is.null(cache)) {
    fs::dir_create(fs::path_dir(cache))
    saveRDS(out, cache)
  }
  out
}

#' @rdname lv_past
#' @export
lv_past_path <- function() {
  Sys.getenv("LIPDVERSE_PAST", unset = path.expand("~/GitHub/lipdverseR/past.json"))
}

#' Find PaST terms matching a value
#'
#' Searches preferred and alternate labels. Exact and case-insensitive matches
#' rank first, then containment, then edit distance, so a compound value like
#' `Palmer Hydrological Drought Index` finds the concept its words name rather
#' than the shortest string in the thesaurus.
#'
#' @param value A single value.
#' @param n How many matches.
#' @param past From [lv_past()].
#' @return A tibble of `pastId`, `pastName`, `rule`, `definition`.
#' @export
lv_past_match <- function(value, n = 3, past = lv_past()) {
  if (is.na(value) || !nzchar(value)) return(past[0, c("pastId", "pastName", "definition")])
  lv <- tolower(trimws(value))

  labs <- tolower(past$pastName)
  alts <- strsplit(ifelse(is.na(past$altLabels), "", tolower(past$altLabels)), " \\| ")

  exact <- labs == lv
  alt_exact <- vapply(alts, function(a) lv %in% a, logical(1))
  contain <- grepl(lv, labs, fixed = TRUE) | vapply(alts, function(a)
    any(grepl(lv, a, fixed = TRUE)), logical(1))
  rev_contain <- nchar(lv) > 5 & vapply(labs, function(l)
    nchar(l) > 3 && grepl(l, lv, fixed = TRUE), logical(1))

  d <- utils::adist(lv, labs)[1, ] / pmax(nchar(labs), nchar(lv))
  score <- d - 10 * exact - 8 * alt_exact - 1.2 * contain - 0.8 * rev_contain

  o <- order(score)[seq_len(min(n, nrow(past)))]
  # Edit distance alone is meaningless once nothing actually matches: PaST has no
  # concept resembling the proxy term "Documents", and ranking by distance
  # answers "volume unit". A suggestion that bad is worse than none, so anything
  # that neither matched a label nor came close is dropped.
  keep <- exact[o] | alt_exact[o] | contain[o] | rev_contain[o] | d[o] <= 0.45
  o <- o[keep]
  if (!length(o)) return(past[0, c("pastId", "pastName", "definition")] |>
                           (\(x) { x$rule <- character(); x[, c("pastId","pastName","rule","definition")] })())
  rule <- ifelse(exact[o], "exact",
          ifelse(alt_exact[o], "altLabel",
          ifelse(contain[o] | rev_contain[o], "contains", "similar")))
  res <- past[o, c("pastId", "pastName", "definition")]
  res$rule <- rule
  res[, c("pastId", "pastName", "rule", "definition")]
}

#' Map contributed datasets to their source directory and papers
#'
#' Submissions arrive as a directory per dataset, typically holding the `.lpd`
#' and a PDF of the paper. The `.lpd` files get flattened into one staging
#' directory for ingest, which loses that association; this recovers it, so a
#' review row can point at the paper that answers it.
#'
#' A value like `HHI` cannot be resolved from the vocabulary at all. It can be
#' resolved from what the authors wrote.
#'
#' @param dir Root of the downloaded submission tree.
#' @param recurse How deep to look for PDFs beside each `.lpd`.
#' @return A tibble of `dataSetName`, `dir`, `lpd`, `pdfs`, `n_pdf`.
#' @export
lv_ingest_sources <- function(dir, recurse = 2) {
  dir <- path.expand(dir)
  if (!fs::dir_exists(dir)) {
    cli::cli_abort("Source directory not found: {.path {dir}}", class = "lv_error_ingest")
  }
  lpds <- fs::dir_ls(dir, glob = "*.lpd", type = "file", recurse = TRUE)
  if (!length(lpds)) {
    cli::cli_alert_warning("No .lpd files under {.path {dir}}")
    return(tibble::tibble(dataSetName = character(), dir = character(),
                          lpd = character(), pdfs = list(), n_pdf = integer()))
  }
  out <- lapply(lpds, function(p) {
    d <- fs::path_dir(p)
    pdf <- fs::dir_ls(d, regexp = "[.][Pp][Dd][Ff]$", type = "file", recurse = recurse)
    # A dataset directory one level down from a shared folder of PDFs still
    # counts: look up one level when the dataset's own directory has none.
    if (!length(pdf) && !identical(fs::path_norm(d), fs::path_norm(dir))) {
      up <- fs::path_dir(d)
      pdf <- fs::dir_ls(up, regexp = "[.][Pp][Dd][Ff]$", type = "file")
    }
    tibble::tibble(dataSetName = sub("\\.lpd$", "", fs::path_file(p)),
                   dir = as.character(d), lpd = as.character(p),
                   pdfs = list(as.character(pdf)), n_pdf = length(pdf))
  })
  out <- dplyr::bind_rows(out)
  cli::cli_alert_info(
    "{nrow(out)} dataset{?s} under {.path {dir}}; {sum(out$n_pdf > 0)} with a PDF.")
  out
}
