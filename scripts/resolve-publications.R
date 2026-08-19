#!/usr/bin/env Rscript
#
# Fill missing publication metadata from the DOI the record already carries.
#
#   ./scripts/resolve-publications.R              # resolve, write review/
#   ./scripts/resolve-publications.R --limit=50   # a taste first
#
# Issue #3 is framed as reading the PDFs bundled with submissions, and some of
# it is. But measured on the 2026-08-18 export, of 6,535 publication rows
# missing an author or a title, **4,912 (75%) carry a usable DOI** -- and a DOI
# resolves to author, year, title and journal without anyone opening a PDF. So
# this closes the cheap three quarters first and leaves the genuinely hard 1,623
# rows to be worked by hand. (5,611 rows have something in the doi field; 699 of
# those hold no DOI at all -- see extract_doi below.)
#
# Two registries, because our DOIs come from two worlds:
#
#   Crossref   journal articles. 10.1016 (380), 10.1029 (242), 10.1038 (102)...
#   DataCite   data publications. 10.1594 is PANGAEA (85), 10.25921 is NOAA
#              NCEI. Crossref does not know these and returns 404.
#
# Output is `review/publication-metadata.csv`, one row per DOI, carrying what
# the record has now beside what the registry says. NOTHING IS WRITTEN TO ANY
# FILE HERE -- a resolved title is a claim about someone else's paper, and it
# gets read before it is applied. Apply with apply-publications.R once reviewed.

suppressPackageStartupMessages({library(dplyr); library(arrow)})
suppressMessages(devtools::load_all(quiet = TRUE))

args  <- commandArgs(trailingOnly = TRUE)
limit <- suppressWarnings(as.integer(sub("^--limit=", "", grep("^--limit=", args, value = TRUE))))
limit <- if (length(limit) && !is.na(limit)) limit else Inf
export <- sub("^--export=", "", grep("^--export=", args, value = TRUE))
if (!length(export)) {
  d <- path.expand("~/lipdverse-export/_database")
  export <- file.path(d, sort(list.files(d), decreasing = TRUE)[1])
}
cli::cli_alert_info("Reading {.path {export}}")

p <- arrow::read_parquet(file.path(export, "publications.parquet"))
nz <- function(x) !is.na(x) & nzchar(as.character(x))
au <- vapply(p$authors, function(x) paste(as.character(x), collapse = "; "), "")
gap <- p[!nz(au) | !nz(p$title), , drop = FALSE]
gap$authors_now <- au[!nz(au) | !nz(p$title)]

# A "doi" field in this database holds a DOI, a DOI inside a publisher URL, an
# FTP path, or the word "palmod". Pull a DOI out of whatever is there; anything
# with no DOI in it at all is reported rather than guessed at.
extract_doi <- function(x) {
  s <- tolower(trimws(as.character(x)))
  s <- sub("^doi:\\s*", "", s)
  m <- regmatches(s, regexpr("10[.][0-9]{4,9}/[^[:space:]\"<>]+", s))
  out <- rep(NA_character_, length(s))
  out[lengths(regmatches(s, gregexpr("10[.][0-9]{4,9}/", s))) > 0] <- m
  sub("[.,;)]+$", "", out)
}

gap$doi_clean <- extract_doi(gap$doi)
dois <- unique(stats::na.omit(gap$doi_clean))
if (is.finite(limit)) dois <- utils::head(dois, limit)

cli::cli_alert_info("{nrow(gap)} row{?s} missing author or title; {length(dois)} distinct DOI{?s} to resolve")
noneed <- sum(is.na(gap$doi_clean))
if (noneed) cli::cli_alert_warning("{noneed} row{?s} carry no usable DOI and need the PDF route (issue #3 proper)")

# ---- resolve ---------------------------------------------------------------
#
# Politely: Crossref asks for a mailto so it can route you to the fast pool, and
# a pause between calls. This is someone else's free service.

UA <- "lipdverse-updater/0.1 (https://lipdverse.org; mailto:nick@nau.edu)"
get_json <- function(url) {
  r <- tryCatch(curl::curl_fetch_memory(url, curl::new_handle(
    useragent = UA, timeout = 30L, followlocation = TRUE)), error = function(e) NULL)
  if (is.null(r) || r$status_code != 200) return(NULL)
  tryCatch(jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE),
           error = function(e) NULL)
}


# `x[[1]] %||% NA` is not safe: a Crossref record with no journal has
# container-title as an EMPTY list, and [[1]] on that errors before %||% is
# ever consulted. A record without a journal is normal -- data publications,
# books, reports -- so this crashed 4% into the first full run.
first_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- x[[1]]
  if (is.null(v) || length(v) == 0) return(NA_character_)
  as.character(v)[1]
}

from_crossref <- function(doi) {
  j <- get_json(paste0("https://api.crossref.org/works/", utils::URLencode(doi, TRUE)))
  m <- j$message
  if (is.null(m)) return(NULL)
  a <- vapply(m$author %||% list(), function(x)
    trimws(paste0(x$family %||% x$name %||% "", if (!is.null(x$given)) paste0(", ", x$given) else "")), "")
  list(source = "crossref",
       authors = paste(a[nzchar(a)], collapse = "; "),
       year = first_chr(m$issued$`date-parts`[[1]]),
       title = first_chr(m$title),
       journal = first_chr(m$`container-title`),
       kind = first_chr(m$type))
}

from_datacite <- function(doi) {
  j <- get_json(paste0("https://api.datacite.org/dois/", utils::URLencode(doi, TRUE)))
  d <- j$data$attributes
  if (is.null(d)) return(NULL)
  a <- vapply(d$creators %||% list(), function(x) x$name %||% "", "")
  list(source = "datacite",
       authors = paste(a[nzchar(a)], collapse = "; "),
       year = first_chr(d$publicationYear),
       title = if (length(d$titles)) first_chr(d$titles[[1]]$title) else NA_character_,
       journal = first_chr(d$publisher),
       kind = first_chr(d$types$resourceTypeGeneral))
}

rows <- vector("list", length(dois))
pb <- cli::cli_progress_bar("Resolving", total = length(dois))
for (i in seq_along(dois)) {
  doi <- dois[i]
  # Crossref first for journal prefixes, DataCite first for the data ones, then
  # fall back to the other. Saves a guaranteed 404 on ~14% of lookups.
  data_prefix <- grepl("^10[.](1594|25921|5061|17632|7910|26008|15784)/", doi)
  r <- if (data_prefix) from_datacite(doi) %||% from_crossref(doi)
       else from_crossref(doi) %||% from_datacite(doi)
  rows[[i]] <- tibble::tibble(
    doi = doi,
    resolved = !is.null(r),
    source = r$source %||% NA_character_,
    authors_found = r$authors %||% NA_character_,
    year_found = as.character(r$year %||% NA),
    title_found = r$title %||% NA_character_,
    journal_found = r$journal %||% NA_character_,
    kind_found = r$kind %||% NA_character_)
  Sys.sleep(0.06)
  cli::cli_progress_update(id = pb)
}
cli::cli_progress_done(id = pb)
res <- purrr::list_rbind(rows)

out <- gap |>
  select(datasetId, pubIndex, doi_raw = doi, doi = doi_clean,
         authors_now, year_now = year, title_now = title, journal_now = journal) |>
  left_join(res, by = "doi") |>
  mutate(decision = NA_character_) |>
  arrange(is.na(doi), desc(resolved), datasetId)

fs::dir_create("review")
readr::write_csv(out, "review/publication-metadata.csv", na = "")
cli::cli_alert_success("Wrote {.path review/publication-metadata.csv} -- {nrow(out)} row{?s}")

cat("\n")
print(as.data.frame(res |> count(resolved, source)), right = FALSE)
cat(sprintf("\nrows that would gain an author: %d\nrows that would gain a title : %d\n",
            sum(!nz(out$authors_now) & nz(out$authors_found)),
            sum(!nz(out$title_now) & nz(out$title_found))))
