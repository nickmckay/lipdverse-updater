#!/usr/bin/env Rscript
#
# Import the legacy bibliographic database into the QC store.
#
#   ./scripts/import-reference-database.R            # report
#   ./scripts/import-reference-database.R --commit   # write to the store
#
# lipdverseR resolved each DOI through crossref once and accumulated the result
# in a Google Sheet, so the network was paid for a reference the first time it
# appeared. That is the right shape; the sheet is not. This moves the
# accumulation into the store, where it is git-tracked and a correction is a
# reviewable commit rather than an untraceable edit.
#
# Two sources: the sheet (4,822 entries, all with author/title/year) and
# additionalLipdverse.bib (16 hand-entered works with no DOI). Nothing is
# re-resolved -- the point is to inherit what was already paid for.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

commit <- "--commit" %in% commandArgs(trailingOnly = TRUE)
SHEET <- "1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg"
ADDITIONAL <- path.expand("~/Dropbox/lipdverse/html/lipdverse/additionalLipdverse.bib")
store <- qc_store()

gs <- sheet_read(sheet_backend_google(), SHEET, "bib database")
cat(sprintf("sheet      : %d row%s, %d distinct citekey%s\n", nrow(gs),
            if (nrow(gs) == 1) "" else "s", dplyr::n_distinct(gs$citekey),
            if (dplyr::n_distinct(gs$citekey) == 1) "" else "s"))
dup <- gs$citekey[duplicated(gs$citekey)]
if (length(dup)) {
  # The legacy code stopped here and told a human to fix the sheet. In the store
  # a duplicate is just a row that loses, and the loss is visible in the diff.
  cat(sprintf("  %d duplicate citekey%s, first occurrence kept\n", length(dup),
              if (length(dup) == 1) "" else "s"))
}
from_sheet <- gs[, intersect(c("citekey", LV_BIB_FIELDS), names(gs)), drop = FALSE]
from_sheet$source <- "crossref"

curated <- if (file.exists(ADDITIONAL)) lv_references_read_bib(ADDITIONAL) else NULL
if (!is.null(curated) && nrow(curated)) {
  curated$source <- "curated"
  cat(sprintf("overrides  : %d hand-entered work%s from %s\n", nrow(curated),
              if (nrow(curated) == 1) "" else "s", basename(ADDITIONAL)))
}

# Curated first, so a hand-entered record wins a citekey collision with the
# resolved one: somebody typed it for a reason.
refs <- bind_rows(curated, from_sheet)
r <- lv_references_add(refs, store, dry_run = !commit)
cat(sprintf("\nstore      : %s\n", r$path))
cat(sprintf("%s %d reference%s (%d already present)\n",
            if (commit) "added" else "would add", r$added,
            if (r$added == 1) "" else "s", r$kept))

if (commit) {
  have <- lv_references(store)
  cat("\nby source:\n"); print(as.data.frame(count(have, source)), right = FALSE)
  cat("\nfilled:\n")
  for (k in c("title", "author", "year", "journal", "doi")) {
    cat(sprintf("  %-8s %d of %d\n", k, sum(!is.na(have[[k]])), nrow(have)))
  }
  cat("\nCommit the store so the import is reviewable.\n")
}
