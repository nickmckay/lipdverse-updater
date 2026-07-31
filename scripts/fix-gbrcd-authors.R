#!/usr/bin/env Rscript
#
# Give GBRCD's publication authors the structure the rest of the corpus uses.
#
#   ./scripts/fix-gbrcd-authors.R --src=~/lipdverse-staging/csm-GBRCD \
#                                 --out=~/lipdverse-staging/csm-GBRCD-authors
#
# All 208 GBRCD files store `pub[[i]]$author` as a flat string:
#
#   "Alibert, C., Kinsley, L., Fallon, S.J., McCulloch, M.T., & McAllister, F."
#
# so every one of them fails validLipd() with "author field should be a list".
# This predates the csm work; the source files are invalid too.
#
# The fix wraps the string as `list(list(name = <string>))`, matching
# lipdverseR's `fixPubAuthorList()`. Names are deliberately **not** split into
# separate entries: a sample of 106 datasets from the main database found 99%
# of structured author lists carry exactly one entry, so a single entry holding
# the whole string is the corpus convention rather than a compromise. Splitting
# would also require parsing "Surname, I." against the commas that separate
# authors, which is exactly the kind of guess that corrupts names.

suppressPackageStartupMessages(library(dplyr))
suppressMessages(devtools::load_all(Sys.getenv("LIPDR_PATH", path.expand("~/GitHub/lipdR")), quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) path.expand(v[1]) else d }
src <- getarg("src", path.expand("~/lipdverse-staging/csm-GBRCD"))
out <- getarg("out", path.expand("~/lipdverse-staging/csm-GBRCD-authors"))
dry <- "--dry-run" %in% args

stopifnot(dir.exists(src))
files <- list.files(src, "[.]lpd$", full.names = TRUE)
cat(sprintf("source: %s (%d files)\n", src, length(files)))
if (!dry) dir.create(out, recursive = TRUE, showWarnings = FALSE)

fixed <- 0L; already <- 0L; none <- 0L
for (p in files) {
  L <- tryCatch(readLipd(p), error = function(e) NULL)
  if (is.null(L)) { cat("  unreadable:", basename(p), "\n"); next }
  changed <- FALSE
  for (i in seq_along(L$pub)) {
    a <- L$pub[[i]]$author
    if (is.null(a)) { none <- none + 1L; next }
    if (is.character(a)) {
      L$pub[[i]]$author <- list(list(name = paste(a, collapse = "; ")))
      changed <- TRUE
    } else already <- already + 1L
  }
  if (changed) fixed <- fixed + 1L
  if (!dry) writeLipd(L, path = out, removeNamesFromLists = TRUE)
}

cat(sprintf("\ndatasets with an author rewritten: %d\n", fixed))
cat(sprintf("pub entries already structured    : %d\n", already))
cat(sprintf("pub entries with no author        : %d\n", none))
if (dry) cat("\nDRY RUN -- nothing written\n") else cat(sprintf("\nwrote %s\n", out))
