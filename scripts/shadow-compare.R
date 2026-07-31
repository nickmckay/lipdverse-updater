#!/usr/bin/env Rscript
#
# Compare two directories of .lpd files field by field.
#
#   ./scripts/shadow-compare.R <old_dir> <new_dir> [--out=diff.csv] [--no-ignore]
#
# Example: what the CoralHydro2k fork and the integrated copies disagree about.
#
#   ./scripts/shadow-compare.R \
#       ~/Dropbox/lipdverse/CoralHydro2k \
#       ~/Dropbox/lipdverse/database \
#       --out=review/shadow-coralhydro2k.csv
#
# Only datasets present in both directories are compared; anything unique to
# one side is reported as a count rather than thousands of one-sided rows.

suppressMessages(devtools::load_all(dirname(dirname(normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))), quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
pos  <- args[!grepl("^--", args)]
out  <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (length(pos) != 2) stop("usage: shadow-compare.R <old_dir> <new_dir> [--out=diff.csv] [--no-ignore]")

old <- path.expand(pos[1]); new <- path.expand(pos[2])
ig  <- if ("--no-ignore" %in% args) NULL else shadow_ignore()

a_files <- basename(list.files(old, "[.]lpd$"))
b_files <- basename(list.files(new, "[.]lpd$"))
both <- intersect(a_files, b_files)
cat(sprintf("old: %d files   new: %d files   in both: %d\n",
            length(a_files), length(b_files), length(both)))
if (!length(both)) stop("no datasets in common")
if (length(setdiff(a_files, both))) cat(sprintf("  only in old: %d (not compared)\n", length(setdiff(a_files, both))))
if (length(setdiff(b_files, both))) cat(sprintf("  only in new: %d (not compared)\n", length(setdiff(b_files, both))))

stage <- function(dir, files) {
  d <- file.path(tempfile("shadow"), basename(dir)); dir.create(d, recursive = TRUE)
  file.copy(file.path(dir, files), d)
  d
}

d <- shadow_diff(
  shadow_normalize(stage(old, both), ignore = ig),
  shadow_normalize(stage(new, both), ignore = ig)
)
shadow_report(d, if (length(out)) out[1] else NULL)
