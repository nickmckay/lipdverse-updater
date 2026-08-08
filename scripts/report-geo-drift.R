#!/usr/bin/env Rscript
# Coordinates that disagree between a compilation's QC sheet and its files.
#
# These were invisible until 306aa30: values_equal() compared numbers at the
# lesser of their two precisions, so a coordinate rounded to one decimal matched
# the unrounded one it came from. Reports only -- which side is right is a
# judgement, and the finer value is usually but not always the correct one.
#
#   Rscript scripts/report-geo-drift.R                  every configured compilation
#   Rscript scripts/report-geo-drift.R iso2k Temp12k    just these
suppressMessages(devtools::load_all(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])), ".."), quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
comps <- if (length(args)) args else lv_compilations()$compilation
bk <- sheet_backend_google()

out <- list()
for (comp in comps) {
  r <- tryCatch(lv_geo_drift(lv_config(comp), bk), error = function(e) {
    cat(sprintf("%-24s skipped: %s\n", comp, substr(conditionMessage(e), 1, 60))); NULL })
  if (is.null(r)) next
  cat(sprintf("%-24s %d disagreement%s\n", comp, nrow(r), if (nrow(r) == 1) "" else "s"))
  flush(stdout())
  if (nrow(r)) out[[comp]] <- r
}

res <- dplyr::bind_rows(out)
if (!nrow(res)) { cat("\nNo coordinate drift.\n"); quit() }

cat("\n=== which side carries more precision ===\n")
print(as.data.frame(count(res, finer, is_rounding)), right = FALSE)
cat("\n=== a rounding is a precision difference; the rest are not ===\n")
print(as.data.frame(res |> filter(!is_rounding) |>
  select(compilation, dataSetName, field, sheet, file)), right = FALSE)

p <- file.path("review", "geo-drift.csv")
fs::dir_create("review")
readr::write_csv(res, p, na = "")
cat(sprintf("\nWrote %s (%d rows)\n", p, nrow(res)))
