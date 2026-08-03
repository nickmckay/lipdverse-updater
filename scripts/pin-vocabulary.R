#!/usr/bin/env Rscript
#
# Pin the LiPDverse standard tables into the repo.
#
#   ./scripts/pin-vocabulary.R            # report what would change
#   ./scripts/pin-vocabulary.R --commit   # write the new pin
#
# lipdverseR calls getStandardTables() at runtime, which is
# readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS")) -- a
# network fetch in the hot path of every standardisation. Two runs a month apart
# are therefore not comparable, and a run cannot be reproduced at all once the
# tables move on. Plan section 11.
#
# Pinned as one CSV per key rather than the RDS, so a vocabulary change shows up
# as a readable diff: the whole thing is about 3,200 rows.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
commit <- "--commit" %in% args
url    <- "https://lipdverse.org/lipdverse/standardTables.RDS"
dest   <- file.path("inst", "extdata", "vocab")

cat(sprintf("fetching %s\n", url))
st <- readRDS(url(url), "rb")
cat(sprintf("tables: %d\n\n", length(st)))

new <- lapply(st, function(x) {
  x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
  # paleoData_proxyGeneral carries a definition rather than synonyms.
  ord <- if ("synonym" %in% names(x)) order(x$lipdName, x$synonym) else order(x$lipdName)
  x[ord, , drop = FALSE]
})
names(new) <- names(st)

old <- if (dir.exists(dest)) lv_vocab(validate = FALSE) else NULL
for (k in names(new)) {
  o <- if (!is.null(old) && !is.null(old[[k]])) nrow(old[[k]]) else 0L
  cat(sprintf("  %-32s %5d rows (was %d)\n", k, nrow(new[[k]]), o))
}

pin <- digest::digest(new, algo = "md5")
cat(sprintf("\npin: %s\n", pin))
if (!is.null(old) && identical(attr(old, "pin"), pin)) {
  cat("unchanged from the current pin; nothing to do.\n")
  quit(save = "no")
}

if (!commit) { cat("\npass --commit to write the new pin.\n"); quit(save = "no") }

dir.create(dest, recursive = TRUE, showWarnings = FALSE)
for (k in names(new)) write_csv(new[[k]], file.path(dest, paste0(k, ".csv")), na = "")
writeLines(jsonlite::toJSON(list(
  pin = pin, source = url,
  fetched_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  tables = as.list(vapply(new, nrow, integer(1)))), auto_unbox = TRUE, pretty = TRUE),
  file.path(dest, "vocab-pin.json"))
cat(sprintf("wrote %s\n", dest))
