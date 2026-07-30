#!/usr/bin/env Rscript
#
# Report fields where two compilations' QC sheets disagree about the same TSid.
#
# Compilations are partially overlapping views of one file collection: 56% of
# datasets belong to two or more. But some QC fields are stored in the .lpd
# file itself, so they are GLOBAL -- shared by every compilation containing
# that dataset. When two compilations hold different values for such a field,
# the compilation that runs last silently overwrites the other. No conflict is
# raised and no changelog records the reversal, which is precisely the shape of
# the "data gremlins" compilation leads have reported.
#
# Fields that are compilation-SCOPED (inThisCompilation, QC certification, QC
# comments) cannot conflict this way and are excluded.
#
# Input is the QC store written by snapshot-qc-sheets.R; no network needed.
#
#   ./scripts/report-cross-compilation-conflicts.R
#   ./scripts/report-cross-compilation-conflicts.R --out=conflicts.csv
#
# Env: LIPDVERSE_QCSTORE

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
})

args    <- commandArgs(trailingOnly = TRUE)
out     <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
qcstore <- Sys.getenv("LIPDVERSE_QCSTORE", path.expand("~/GitHub/lipdverse-qcstore"))
snapdir <- file.path(qcstore, "snapshots")

if (!dir.exists(snapdir)) stop("no snapshots at ", snapdir, " -- run snapshot-qc-sheets.R first")

# Fields whose value lives in the .lpd file and is therefore shared across
# every compilation that contains the dataset. Keep this list conservative:
# a false entry here produces noise, a missing one only means less coverage.
global_fields <- c(
  "archiveType", "variableName", "units", "proxy",
  "lat", "lon", "elevation", "siteName",
  "pub1_doi", "pub2_doi"
)

dirs <- setdiff(list.dirs(snapdir, recursive = FALSE), file.path(snapdir, "_shared"))

long <- list()
for (d in dirs) {
  f <- file.path(d, "QC.csv")
  if (!file.exists(f)) next
  x <- suppressWarnings(read_csv(f, col_types = cols(.default = col_character()),
                                 progress = FALSE, name_repair = "minimal"))
  x <- x[, !duplicated(names(x)), drop = FALSE]
  if (!"TSid" %in% names(x)) next
  keep <- intersect(global_fields, names(x))
  if (!length(keep)) next
  y <- x[!is.na(x$TSid), c("TSid", keep), drop = FALSE]
  long[[basename(d)]] <- pivot_longer(y, all_of(keep), names_to = "field", values_to = "value") |>
    mutate(compilation = basename(d))
}

L <- bind_rows(long) |> filter(!is.na(value), value != "")
message(sprintf("%d compilations, %d populated cells, %d TSids",
                length(long), nrow(L), length(unique(L$TSid))))

conf <- L |>
  group_by(TSid, field) |>
  summarise(n_comp = n_distinct(compilation),
            n_val  = n_distinct(value),
            # Normalised comparison separates real disagreements from
            # "coral" vs "Coral" style vocabulary drift.
            n_norm = n_distinct(tolower(trimws(gsub("[^A-Za-z0-9]", "", value)))),
            values = paste(sort(unique(value)), collapse = " | "),
            compilations = paste(sort(unique(compilation)), collapse = ","),
            .groups = "drop") |>
  filter(n_comp > 1, n_val > 1) |>
  mutate(kind = ifelse(n_norm == 1, "cosmetic", "substantive")) |>
  arrange(desc(kind), field, TSid)

shared <- L |> group_by(TSid, field) |> summarise(n = n_distinct(compilation), .groups = "drop") |> filter(n > 1)

cat("\n")
cat("(TSid, field) pairs curated in 2+ compilations: ", nrow(shared), "\n", sep = "")
cat("  in agreement: ", nrow(shared) - nrow(conf), "\n", sep = "")
cat("  CONFLICTING:  ", nrow(conf),
    sprintf("  (%s substantive, %s cosmetic)\n",
            sum(conf$kind == "substantive"), sum(conf$kind == "cosmetic")), sep = "")
cat("  TSids affected: ", length(unique(conf$TSid)), "\n\n", sep = "")

if (nrow(conf)) {
  cat("by field:\n")
  print(as.data.frame(count(conf, field, kind, name = "n") |> arrange(desc(n))), row.names = FALSE)
  cat("\nby compilation pair:\n")
  print(head(as.data.frame(count(conf, compilations, name = "n") |> arrange(desc(n))), 12), row.names = FALSE)
}

if (length(out) > 0) {
  write_csv(conf, out[1])
  message("wrote ", out[1], " (", nrow(conf), " rows)")
}

invisible(NULL)
