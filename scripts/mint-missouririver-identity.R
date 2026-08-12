#!/usr/bin/env Rscript
#
# Give the Gangopadhyay 2026 Missouri River chronologies their own identity.
#
#   ./scripts/mint-missouririver-identity.R            # report, write nothing
#   ./scripts/mint-missouririver-identity.R --commit   # rewrite the .lpd files
#
# The 183 chronologies were delivered sharing ONE TSid and ONE datasetId between
# them, so nothing could ingest them: lv_validate_identity() rejects the set, and
# anything keyed by TSid collapses 183 series into one cell.
#
# This mints a unique datasetId per file and a unique TSid per column, in place,
# and changes nothing else. dataSetName is deliberately left alone -- see the
# note in the issue: the 6-letter site codes have no fixed grammar (RW/EW/LW
# accounts for only 84 of 183 trailing pairs, and the middle pair is sometimes a
# species and sometimes a US state), so a decoded name cannot be derived from a
# filename without a lookup from the authors.
#
# The reconstruction, MissouriRiver.Gangopadhyay.2026, is skipped: it is already
# in the database under its own datasetId, and whether it belongs there at all is
# an open question.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

commit <- "--commit" %in% commandArgs(trailingOnly = TRUE)
D <- file.path("/Users/nicholas/Library/CloudStorage/GoogleDrive-nick.mckay2@gmail.com",
               ".shortcut-targets-by-id/1L5QdzHegYzesG6NOxgXwlXPOTYe8CO0t/Hydroclimate2k",
               " 4. InLipdPendingLipdverse/MissouriRiver.Gangopadhyay.2026")
SKIP <- "MissouriRiver.Gangopadhyay.2026"

files <- list.files(D, "[.]lpd$", full.names = TRUE)
cat(sprintf("files: %d (skipping %s)\nmode : %s\n\n", length(files), SKIP,
            if (commit) "COMMIT" else "dry run"))

read_meta <- function(p) {
  nms <- utils::unzip(p, list = TRUE)$Name
  j <- grep("\\.jsonld$", nms, value = TRUE)[1]
  con <- unz(p, j); on.exit(close(con), add = TRUE)
  list(json = j,
       meta = jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                 simplifyVector = FALSE))
}

# Identifiers already in the database, so a minted one cannot collide with an
# existing record as well as with its siblings.
idx <- lv_db_index(lv_scan(lv_path("database")), cache = TRUE)
taken_ds <- stats::na.omit(idx$datasets$datasetId)
taken_ts <- stats::na.omit(idx$timeseries$TSid)
mint <- function(taken) {
  repeat {
    id <- lipdR::createTSid()
    if (!id %in% taken) return(id)
  }
}

before <- list(); plan <- list()
for (p in files) {
  r <- read_meta(p)
  m <- r$meta
  dsn <- as.character(m$dataSetName)[1]
  if (identical(dsn, SKIP)) next
  tsids <- character()
  for (pd in m$paleoData) for (tb in pd$measurementTable) {
    cols <- if (!is.null(tb$columns)) tb$columns else tb
    for (col in cols) if (is.list(col) && !is.null(col$TSid)) tsids <- c(tsids, as.character(col$TSid)[1])
  }
  before[[length(before) + 1L]] <- tibble::tibble(dsn = dsn, datasetId = as.character(m$datasetId)[1],
                                                  n_ts = length(tsids))
  plan[[dsn]] <- list(path = p, json = r$json, meta = m, n_ts = length(tsids))
}
b <- bind_rows(before)
cat(sprintf("chronologies      : %d\ndistinct datasetIds now: %d\ncolumns with a TSid    : %d\n\n",
            nrow(b), dplyr::n_distinct(b$datasetId), sum(b$n_ts)))

if (!commit) {
  cat("would mint", nrow(b), "datasetIds and", sum(b$n_ts), "TSids\n")
  quit(save = "no")
}

# Mint first, so a collision inside the batch is impossible before anything is
# written, then rewrite. The .lpd is a zip; only the jsonld entry changes, and
# the CSVs are copied through untouched so the data cannot be disturbed.
new_ids <- character(); new_ts <- list()
for (dsn in names(plan)) {
  id <- mint(c(taken_ds, new_ids)); new_ids[dsn] <- id; taken_ds <- c(taken_ds, id)
  k <- character()
  for (i in seq_len(plan[[dsn]]$n_ts)) {
    t <- mint(c(taken_ts, k)); k <- c(k, t)
  }
  new_ts[[dsn]] <- k; taken_ts <- c(taken_ts, k)
}

written <- 0L
for (dsn in names(plan)) {
  el <- plan[[dsn]]; m <- el$meta
  m$datasetId <- new_ids[[dsn]]
  k <- new_ts[[dsn]]; n <- 0L
  for (pi in seq_along(m$paleoData)) {
    for (ti in seq_along(m$paleoData[[pi]]$measurementTable)) {
      tb <- m$paleoData[[pi]]$measurementTable[[ti]]
      named <- !is.null(tb$columns)
      cols <- if (named) tb$columns else tb
      for (ci in seq_along(cols)) {
        if (!is.list(cols[[ci]]) || is.null(cols[[ci]]$TSid)) next
        n <- n + 1L
        cols[[ci]]$TSid <- k[n]
      }
      if (named) tb$columns <- cols else tb <- cols
      m$paleoData[[pi]]$measurementTable[[ti]] <- tb
    }
  }
  # Rewrite the archive entry in place rather than rebuilding the package, so
  # the CSV payload and the bag layout are exactly what the authors delivered.
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(el$path, exdir = tmp)
  jf <- file.path(tmp, el$json)
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, null = "null", pretty = TRUE), jf)
  old <- getwd(); setwd(tmp)
  utils::zip(el$path, list.files(tmp, recursive = FALSE), flags = "-rq")
  setwd(old); unlink(tmp, recursive = TRUE)
  written <- written + 1L
}
cat(sprintf("rewrote %d file%s\n", written, if (written == 1) "" else "s"))
