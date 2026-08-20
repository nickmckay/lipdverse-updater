#!/usr/bin/env Rscript
#
# Import the legacy datasetId -> citekey map into the reference store.
#
#   ./scripts/import-bibdsid.R                 # from the live site
#   ./scripts/import-bibdsid.R path.json --commit
#
# A reference resolves to a publication by DOI, and 1,623 publication rows have
# none. The link that answers for those is per dataset: the legacy site
# publishes bibDsid.json, datasetId -> the citekeys that dataset cites, in
# citation order. 817 of the 1,308 datasets with no DOI have a curated citation
# behind it.
#
# This is a legacy artefact, produced by the old pipeline. Importing it into the
# store means the link survives the old site rather than depending on it.

suppressPackageStartupMessages({library(dplyr); library(jsonlite)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
src    <- args[!grepl("^--", args)][1]
commit <- "--commit" %in% args
if (is.na(src)) src <- "https://lipdverse.org/lipdverse/bibDsid.json"

cli::cli_alert_info("Reading {.path {src}}")
bd <- jsonlite::fromJSON(src, simplifyVector = FALSE)

rows <- list()
for (id in names(bd)) {
  k <- unlist(bd[[id]]$key)
  k <- k[!is.na(k) & nzchar(k)]
  if (!length(k)) next
  rows[[length(rows) + 1L]] <- tibble::tibble(datasetId = id, citekey = k,
                                              rank = seq_along(k))
}
links <- purrr::list_rbind(rows)
refs <- lv_references(qc_store())

cat(sprintf("datasets      : %d\ncitekey links : %d (%d distinct keys)\n",
            dplyr::n_distinct(links$datasetId), nrow(links),
            dplyr::n_distinct(links$citekey)))
known <- links$citekey %in% refs$citekey
cat(sprintf("keys the store can resolve: %d of %d (%.1f%%)\n",
            sum(known), nrow(links), 100 * mean(known)))
miss <- sort(unique(links$citekey[!known]))
if (length(miss)) {
  cat(sprintf("unresolvable keys: %d, e.g. %s\n", length(miss),
              paste(utils::head(miss, 2), collapse = ", ")))
}

if (!commit) {
  cat("\ndry run; re-run with --commit to write to the store\n")
} else {
  lv_reference_links_add(links, qc_store(), source = "bibDsid")
  cat(sprintf("\nwrote %d link%s to the reference store\n", nrow(links),
              if (nrow(links) == 1) "" else "s"))
}
