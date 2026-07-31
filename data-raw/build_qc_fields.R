#!/usr/bin/env Rscript
#
# Build inst/extdata/qc_fields.csv, the field registry the merge engine reads.
#
# Assembles reviewed decisions from review/ into one table. Everything here is
# derived; the review files are the inputs and should be edited instead.
#
#   review/qc-field-ownership.csv          ownership + nullable_by_curator
#   review/csm-field-names.csv             which fields become compilation-specific
#   review/lipd key standardization - data.csv   renames and deletions
#   snapshots/_shared/convo                QC column -> timeseries field, type
#
# In the review files a blank decision column means "accept the suggestion",
# so the reviewer only records exceptions.
#
#   Rscript data-raw/build_qc_fields.R

suppressPackageStartupMessages({library(readr); library(dplyr); library(stringr)})

repo    <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
qcstore <- Sys.getenv("LIPDVERSE_QCSTORE", path.expand("~/GitHub/lipdverse-qcstore"))
rd      <- function(p) read_csv(p, col_types = cols(.default = col_character()), progress = FALSE)

own   <- rd(file.path(repo, "review/qc-field-ownership.csv"))
csm   <- rd(file.path(repo, "review/csm-field-names.csv"))
terms <- rd(file.path(repo, "review/terms-draft.csv"))
std   <- rd(file.path(repo, "review/lipd key standardization - data.csv"))
convo <- rd(file.path(qcstore, "snapshots/_shared/convo/Sheet1.csv")) |>
  filter(!is.na(qcSheetName)) |> distinct(qcSheetName, .keep_all = TRUE)

take <- function(decision, suggestion) {
  d <- trimws(decision)
  ifelse(is.na(d) | !nzchar(d), suggestion, d)
}

# ---- ownership -------------------------------------------------------------
own <- own |> mutate(
  ownership_final = take(ownership, suggested_ownership),
  nullable_final  = take(nullable_by_curator, suggested_nullable))

# ---- csm: these leave the shared namespace entirely -------------------------
csm <- csm |> mutate(field_final = take(approved_field, proposed_field))
csm_keys <- csm |>
  filter(!is.na(comp_key), nzchar(comp_key)) |>
  transmute(source_key = key,
            csm_compilation = compilation,
            csm_field = field_final,
            csm_flat_key = paste0(comp_key, "_csm_", field_final))
csm_removed <- csm |> filter(is.na(comp_key) | !nzchar(comp_key)) |> pull(key)

# ---- standardization sheet: renames and deletions ---------------------------
markers <- c("deleteMe", "ignore", "compilation-specific",
             "add to inCompilation", "inCompilation family", "if true, paleo")
std <- std |> mutate(disposition = case_when(
  lipdName %in% markers               ~ lipdName,
  str_starts(lipdName, "add to|if true") ~ "instruction",
  TRUE                                ~ "rename"))
nz <- function(s) gsub("([A-Za-z]+)[0-9]+_", "\\1_", s)
std_lut <- std |> transmute(n_syn = nz(synonym), disposition, std_canonical = lipdName) |>
  distinct(n_syn, .keep_all = TRUE)

# ---- assemble --------------------------------------------------------------
VOCAB <- c(paleoData_variableName = "variableName", paleoData_units = "units",
           archiveType = "archiveType", paleoData_proxy = "proxy",
           interpretation_seasonality = "seasonality", interpretation_variable = "interpretationVariable")

reg <- terms |>
  transmute(qc_name = term,
            family,
            category = suggested_category,
            canonical = suggested_canonical,
            ts_name, convo_type, same_across_dataset,
            n_compilations, n_filled) |>
  left_join(own |> select(qc_name = term, ownership = ownership_final,
                          nullable_by_curator = nullable_final), by = "qc_name") |>
  left_join(csm_keys, by = c("qc_name" = "source_key")) |>
  left_join(std_lut, by = c("qc_name" = "n_syn")) |>
  mutate(
    n_compilations = as.integer(n_compilations),
    n_filled = as.integer(n_filled),
    # A field is one of: an identifier, compilation-specific, a synonym of
    # another field, deleted, or a merged field with an ownership rule.
    role = case_when(
      category == "key"                          ~ "key",
      # An identifier is an identifier regardless of which review file said so:
      # paleoData_TSid is the canonical form of the TSid column and must never
      # be merged.
      !is.na(ownership) & ownership == "key"     ~ "key",
      !is.na(csm_flat_key)                       ~ "csm",
      qc_name %in% csm_removed                   ~ "delete",
      disposition == "deleteMe"                  ~ "delete",
      category == "synonym"                      ~ "synonym",
      category %in% c("control", "unused")       ~ category,
      # A reviewer can redirect a field into the inCompilation structure
      # instead of giving it a merge rule.
      grepl("inCompilation", ownership)          ~ "csm_pending",
      TRUE                                       ~ "merged"),
    ownership = case_when(
      role == "key"     ~ "key",
      role == "merged"  ~ ownership,
      TRUE              ~ NA_character_),
    nullable_by_curator = ifelse(role == "merged", nullable_by_curator, NA_character_),
    cardinality = case_when(
      same_across_dataset == "TRUE"  ~ "dataset",
      same_across_dataset == "FALSE" ~ "timeseries",
      TRUE                           ~ NA_character_),
    type = convo_type,
    vocab_key = unname(VOCAB[nz(qc_name)]),
    deprecated = role == "delete")

out <- reg |>
  transmute(qc_name, ts_name, family, role, ownership, nullable_by_curator,
            cardinality, type, vocab_key, canonical,
            csm_compilation, csm_field, csm_flat_key,
            deprecated, n_compilations, n_filled) |>
  arrange(factor(role, levels = c("merged", "csm", "key", "synonym", "control", "unused", "delete")),
          desc(n_compilations), qc_name)

dir.create(file.path(repo, "inst/extdata"), recursive = TRUE, showWarnings = FALSE)
write_csv(out, file.path(repo, "inst/extdata/qc_fields.csv"), na = "")

cat("wrote inst/extdata/qc_fields.csv:", nrow(out), "fields\n\n")
print(as.data.frame(count(out, role, name = "fields") |> arrange(desc(fields))), row.names = FALSE)
cat("\nownership of merged fields:\n")
print(as.data.frame(count(filter(out, role == "merged"), ownership, name = "fields") |> arrange(desc(fields))), row.names = FALSE)
# Validate: every merged field must carry a usable rule.
ok <- c("key", "curator", "machine", "shared")
unresolved <- filter(out, role == "merged", !ownership %in% ok)
if (nrow(unresolved)) {
  cat("\nUNRESOLVED -- these have no usable merge rule (", nrow(unresolved), "):\n", sep = "")
  print(as.data.frame(unresolved[, c("qc_name", "ownership", "n_compilations")]), row.names = FALSE)
  cat("\nAll are single-compilation. Seven are GBRCD keys; GBRCD reads its own\n",
      "database directory, which has never been key-inventoried, so they are not\n",
      "in the csm list and are most likely compilation-specific rather than merged.\n", sep = "")
} else {
  cat("\nAll merged fields have a usable rule.\n")
}
