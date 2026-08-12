#!/usr/bin/env Rscript
#
# Run one compilation through the whole pipeline.
#
#   ./scripts/run-compilation.R lipdverseTest            # dry run
#   ./scripts/run-compilation.R lipdverseTest --commit   # write files and store
#   ./scripts/run-compilation.R lipdverseTest --patch    # patch the sheet
#
# The pipeline itself is lv_update(); this is only the command line around it,
# so the same code runs from a script, from a test, and from the shadow harness.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
comp <- args[!grepl("^--", args)][1]
if (is.na(comp)) stop("usage: run-compilation.R <compilation> [--commit] [--patch] [--stage-dir=...]")

getarg <- function(f, d = NULL) {
  v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE))
  if (length(v)) v[1] else d
}

res <- lv_update(
  comp,
  commit = "--commit" %in% args,
  stage  = getarg("stage-dir"),
  patch  = "--patch" %in% args)

invisible(res)
