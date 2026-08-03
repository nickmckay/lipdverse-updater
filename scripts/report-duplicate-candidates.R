#!/usr/bin/env Rscript
#
# Find datasets that may be the same record, and say what to do about each.
#
#   ./scripts/report-duplicate-candidates.R                       # audit the database
#   ./scripts/report-duplicate-candidates.R --incoming=~/newfiles # screen a batch
#
# Ingestion's hard problem is not detecting an exact match -- datasetId and
# dataSetName already do that -- it is the same record arriving under a
# different name, from NOAA rather than the author, or as a longer version of a
# core already held. None of those compare equal on any identifier.
#
# So several independent signals are computed and reported with their evidence,
# and each candidate pair is given a disposition a compilation group can act on.
# Nothing is auto-merged: the point is to hand back a recommendation.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) path.expand(v[1]) else d }
db       <- getarg("src", lv_path("database"))
incoming <- getarg("incoming")
out      <- getarg("out", "review/duplicate-candidates.csv")
km       <- as.numeric(getarg("km", "25"))

# ---- extraction ------------------------------------------------------------
# One row per dataset: everything the signals need, read from the JSON member
# only so the whole database is a couple of minutes rather than an hour.

extract <- function(paths, label) {
  cache <- fs::path(lv_path("state"), "cache", paste0("dupe-extract-", label, ".rds"))
  md5 <- lv_scan(dirname(paths[1]))$fingerprint
  if (fs::file_exists(cache)) {
    prior <- readRDS(cache)
    if (identical(prior$fingerprint, md5)) return(prior$data)
  }
  cli::cli_alert_info("Extracting duplicate signals from {length(paths)} file{?s}")
  rows <- lapply(paths, function(p) {
    nm <- tryCatch(utils::unzip(p, list = TRUE)$Name, error = function(e) NULL)
    j <- grep("jsonld$", nm, value = TRUE)
    if (!length(j)) return(NULL)
    con <- unz(p, j[1])
    m <- tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                     simplifyVector = FALSE), error = function(e) NULL)
    close(con)
    if (is.null(m)) return(NULL)

    co <- m$geo$geometry$coordinates
    num <- function(x) { v <- suppressWarnings(as.numeric(unlist(x)[1])); if (length(v)) v else NA_real_ }
    dois <- unlist(lapply(m$pub, function(e) as_chr1(e$doi)))
    vars <- character(); tsids <- character(); yr <- c(NA_real_, NA_real_)
    for (pd in m$paleoData) for (tb in pd$measurementTable) {
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      for (cl in cols) {
        if (!is.list(cl) || is.null(cl$TSid)) next
        tsids <- c(tsids, as.character(cl$TSid)[1])
        vn <- as_chr1(cl$variableName)
        if (!is.null(vn)) vars <- c(vars, tolower(vn))
      }
    }
    tibble::tibble(
      file = basename(p),
      dataSetName = as_chr1(m$dataSetName) %||% NA_character_,
      datasetId   = as_chr1(m$datasetId) %||% NA_character_,
      archiveType = tolower(as_chr1(m$archiveType) %||% NA_character_),
      lon = num(co[1]), lat = num(co[2]), elev = num(co[3]),
      minYear = num(m$minYear), maxYear = num(m$maxYear),
      siteName = tolower(as_chr1(m$geo$siteName) %||% NA_character_),
      doi  = paste(sort(unique(tolower(stats::na.omit(dois)))), collapse = ";"),
      vars = paste(sort(unique(vars)), collapse = ";"),
      n_ts = length(tsids),
      tsids = paste(sort(unique(tsids)), collapse = ";"))
  })
  data <- bind_rows(rows)
  fs::dir_create(fs::path_dir(cache))
  saveRDS(list(fingerprint = md5, data = data), cache)
  data
}

haversine <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

overlap <- function(a1, a2, b1, b2) {
  lo <- pmax(pmin(a1, a2), pmin(b1, b2))
  hi <- pmin(pmax(a1, a2), pmax(b1, b2))
  ifelse(is.na(lo) | is.na(hi), NA_real_, pmax(0, hi - lo))
}

jaccard <- function(a, b) {
  mapply(function(x, y) {
    x <- strsplit(x, ";", fixed = TRUE)[[1]]; y <- strsplit(y, ";", fixed = TRUE)[[1]]
    x <- x[nzchar(x)]; y <- y[nzchar(y)]
    if (!length(x) || !length(y)) return(NA_real_)
    length(intersect(x, y)) / length(union(x, y))
  }, a, b, USE.NAMES = FALSE)
}

# ---- candidate pairs -------------------------------------------------------
# Bucket by a coarse coordinate grid and compare only within a bucket and its
# neighbours: 7,177 datasets is 25.7M pairs brute force, and almost all of them
# are on different continents.

pairs_near <- function(x, km) {
  x <- x[!is.na(x$lat) & !is.na(x$lon), , drop = FALSE]
  step <- max(km / 111, 0.05)
  x$bi <- floor(x$lat / step); x$bj <- floor(x$lon / step)
  out <- list()
  for (di in -1:1) for (dj in -1:1) {
    y <- x; y$bi <- y$bi + di; y$bj <- y$bj + dj
    j <- inner_join(x, y, by = c("bi", "bj"), suffix = c("", "_b"),
                    relationship = "many-to-many")
    j <- j[j$file < j$file_b, , drop = FALSE]
    if (nrow(j)) out[[length(out) + 1L]] <- j
  }
  if (!length(out)) return(NULL)
  j <- distinct(bind_rows(out), file, file_b, .keep_all = TRUE)
  j$km <- haversine(j$lat, j$lon, j$lat_b, j$lon_b)
  j[!is.na(j$km) & j$km <= km, , drop = FALSE]
}

files <- list.files(db, "[.]lpd$", full.names = TRUE)
have <- extract(files, "database")
cat(sprintf("database: %d datasets, %d with coordinates, %d with a doi\n",
            nrow(have), sum(!is.na(have$lat)), sum(nzchar(have$doi))))

if (!is.null(incoming)) {
  inc <- extract(list.files(incoming, "[.]lpd$", full.names = TRUE), "incoming")
  cat(sprintf("incoming: %d datasets\n", nrow(inc)))
  cand <- pairs_near(bind_rows(mutate(have, .side = "db"), mutate(inc, .side = "new")), km)
  cand <- cand[cand$.side != cand$.side_b, , drop = FALSE]
} else {
  cand <- pairs_near(have, km)
}
if (is.null(cand) || !nrow(cand)) { cat("no candidate pairs\n"); quit(save = "no") }

# ---- signals and disposition ----------------------------------------------

cand <- cand |>
  mutate(
    same_doi   = nzchar(doi) & doi == doi_b,
    same_arch  = !is.na(archiveType) & archiveType == archiveType_b,
    same_site  = !is.na(siteName) & siteName == siteName_b,
    var_overlap = jaccard(vars, vars_b),
    ts_overlap  = jaccard(tsids, tsids_b),
    yr_overlap  = overlap(minYear, maxYear, minYear_b, maxYear_b),
    span   = pmax(maxYear - minYear, 0, na.rm = TRUE),
    span_b = pmax(maxYear_b - minYear_b, 0, na.rm = TRUE))

# Disposition is the recommendation handed back to a compilation group. Ordered
# most to least certain; the first that matches wins.
cand <- cand |>
  mutate(disposition = case_when(
    ts_overlap > 0                      ~ "already_present",
    same_doi & var_overlap >= 0.8       ~ "already_present",
    same_doi & var_overlap > 0          ~ "partial_overlap",
    km < 1 & same_arch & var_overlap >= 0.8 ~ "already_present",
    km < 1 & same_arch & var_overlap > 0    ~ "partial_overlap",
    km < 1 & same_arch                  ~ "same_site_different_record",
    same_site & same_arch               ~ "same_site_different_record",
    TRUE                                ~ "nearby_unrelated"),
    recommendation = case_when(
      disposition == "already_present" & span_b > span ~
        "This record is already in LiPDverse, and the incoming file covers a longer period. Update the existing dataset from it rather than adding a second copy.",
      disposition == "already_present" ~
        "This record is already in LiPDverse. Do not ingest; add the existing dataset to the compilation through the datasetsInCompilation tab instead.",
      disposition == "partial_overlap" ~
        "Part of this record is already in LiPDverse. Update the existing dataset with the columns it does not have, rather than adding a second copy.",
      disposition == "same_site_different_record" ~
        "Same site and archive, different measurements. Probably a genuine addition; match the existing dataset's site naming so the two are findable together.",
      TRUE ~ "Nearby but unrelated. No action."))

cand$link   <- sprintf("https://lipdverse.org/data/%s", cand$datasetId)
cand$link_b <- sprintf("https://lipdverse.org/data/%s", cand$datasetId_b)

res <- cand |>
  transmute(disposition, recommendation,
            dataSetName, datasetId, link,
            match_dataSetName = dataSetName_b, match_datasetId = datasetId_b, match_link = link_b,
            km = round(km, 2), same_doi, same_arch, same_site,
            var_overlap = round(var_overlap, 2), ts_overlap = round(ts_overlap, 2),
            yr_overlap = round(yr_overlap), n_ts, n_ts_b,
            archiveType, archiveType_b, doi, doi_b) |>
  arrange(factor(disposition, levels = c("already_present", "partial_overlap",
                                         "same_site_different_record", "nearby_unrelated")),
          km)

fs::dir_create(fs::path_dir(out))
write_csv(res, out, na = "")
cat(sprintf("\n%d candidate pair%s within %g km\n\n", nrow(res), if (nrow(res) == 1) "" else "s", km))
print(as.data.frame(count(res, disposition, name = "pairs")))
cat(sprintf("\nreport: %s\n", out))
