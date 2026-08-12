#!/usr/bin/env Rscript
#
# Give the Gangopadhyay 2026 chronologies a placeholder geo.siteName.
#
#   ./scripts/placeholder-sitenames-missouririver.R            # report only
#   ./scripts/placeholder-sitenames-missouririver.R --commit   # rewrite
#
# 166 of 184 files carry siteName = "" -- an empty string rather than an absent
# key, so an is.null() test finds none of them. Until the authors supply the real
# chronology names, the site code from the filename stands in: it is what the
# authors themselves use to identify each series, and it is at least a true
# label rather than an invented one.
#
# Real names already present are left alone, with two exceptions. 1BDTDRW and
# 2BDTDEW say "Hermann, Missouri", which is the streamflow gauge the
# reconstruction targets, not a tree-ring site. Their own coordinates say
# otherwise: both sit at (-91.2833, 34.8333), which is Bayou Deview in Arkansas
# and matches their sibling 3BDTDLW, about 430 km from Hermann. The name was
# copy-pasted from the reconstruction, so a placeholder is more honest than a
# false locality.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

commit <- "--commit" %in% commandArgs(trailingOnly = TRUE)
D <- file.path("/Users/nicholas/Library/CloudStorage/GoogleDrive-nick.mckay2@gmail.com",
               ".shortcut-targets-by-id/1L5QdzHegYzesG6NOxgXwlXPOTYe8CO0t/Hydroclimate2k",
               " 4. InLipdPendingLipdverse/MissouriRiver.Gangopadhyay.2026")
SKIP <- "MissouriRiver.Gangopadhyay.2026"
WRONG <- c("1BDTDRW.Gangopadhyay.2026", "2BDTDEW.Gangopadhyay.2026")

files <- list.files(D, "[.]lpd$", full.names = TRUE)
acted <- list()
for (p in files) {
  nms <- utils::unzip(p, list = TRUE)$Name
  j <- grep("\\.jsonld$", nms, value = TRUE)[1]
  con <- unz(p, j)
  m <- jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  close(con)
  dsn <- as.character(m$dataSetName)[1]
  if (identical(dsn, SKIP)) next
  code <- sub("\\..*$", "", dsn)
  # 166 of these carry siteName = "" rather than no siteName at all, so an
  # is.null() test finds none of them. Treat blank as absent, which is what the
  # merge does everywhere else.
  cur <- if (is.null(m$geo[["siteName"]])) NA_character_ else as.character(m$geo[["siteName"]])[1]
  blank <- is.na(cur) || !nzchar(trimws(cur))
  why <- if (blank) "empty" else if (dsn %in% WRONG) "wrong (coordinates are Bayou Deview, not Hermann)" else NA_character_
  if (is.na(why)) next
  acted[[length(acted) + 1L]] <- tibble::tibble(dataSetName = dsn, was = cur, now = code, reason = why)
  if (!commit) next

  if (is.null(m$geo)) m$geo <- list()
  m$geo$siteName <- code
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(p, exdir = tmp)
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, null = "null", pretty = TRUE), file.path(tmp, j))
  old <- getwd(); setwd(tmp)
  utils::zip(p, list.files(tmp, recursive = FALSE), flags = "-rq")
  setwd(old); unlink(tmp, recursive = TRUE)
}
a <- bind_rows(acted)
cat(sprintf("mode: %s\nfiles given a placeholder siteName: %d\n", if (commit) "COMMIT" else "dry run", nrow(a)))
if (nrow(a)) print(as.data.frame(count(a, reason)), right = FALSE)
if (nrow(a)) print(as.data.frame(filter(a, reason != "empty")), right = FALSE)
