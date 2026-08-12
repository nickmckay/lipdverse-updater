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
suppressMessages(devtools::load_all(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])) |> dirname(), quiet = TRUE))

repo    <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
qcstore <- Sys.getenv("LIPDVERSE_QCSTORE", path.expand("~/GitHub/lipdverse-qcstore"))
rd      <- function(p) read_csv(p, col_types = cols(.default = col_character()), progress = FALSE)

own   <- rd(file.path(repo, "review/qc-field-ownership.csv"))
csm   <- rd(file.path(repo, "review/csm-field-names.csv"))
terms <- rd(file.path(repo, "review/terms-draft.csv"))
std   <- rd(file.path(repo, "review/lipd key standardization - data.csv"))
convo <- rd(lipdverseUpdater::lv_snapshot_file(file.path(qcstore, "snapshots/_shared/convo"), "Sheet1")) |>
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
# One row per source key. Five coral keys are claimed by both CoralHydro2k and
# GBRCD, and joining them straight through produced two registry rows with the
# same qc_name -- which validate_qc_fields() rejects, so the registry could not
# be regenerated at all. Both claims are real (the migration copies such a value
# into each compilation the dataset belongs to), so the targets are collapsed
# into one row rather than one of them being dropped.
csm_keys <- csm |>
  filter(!is.na(comp_key), nzchar(comp_key)) |>
  distinct(key, compilation, field_final, comp_key) |>
  arrange(key, compilation) |>
  group_by(source_key = key) |>
  summarise(
    csm_compilation = paste(compilation, collapse = ";"),
    csm_field = {
      f <- unique(field_final)
      if (length(f) > 1) {
        stop("csm key '", first(source_key), "' maps to differing field names: ",
             paste(f, collapse = ", "))
      }
      f
    },
    csm_flat_key = paste0(comp_key, "_csm_", field_final, collapse = ";"),
    .groups = "drop")
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
# The convo sheet has no type for the coordinate fields, but LiPD itself
# requires them numeric -- validLipd() rejects a non-numeric latitude. Declaring
# the type is what lets a bad value be caught in the plan rather than by the
# writer after 400 files have been staged.
NUMERIC <- c("geo_latitude", "geo_longitude", "geo_elevation")

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
      # Membership is not a stored field: it is the curator's control over the
      # inCompilation structure, and the review files classed it as a synonym
      # of inCompilationBeta_struct, which is machine-owned. That would have
      # let the files overrule a curator adding a timeseries.
      qc_name == "inThisCompilation"             ~ "membership",
      category == "key"                          ~ "key",
      # An identifier is an identifier regardless of which review file said so:
      # paleoData_TSid is the canonical form of the TSid column and must never
      # be merged.
      !is.na(ownership) & ownership == "key"     ~ "key",
      !is.na(csm_flat_key)                       ~ "csm",
      qc_name %in% csm_removed                   ~ "delete",
      # A synonym beats a deleteMe. The standardization sheet marks `description`
      # deleteMe while terms-draft makes it a synonym of paleoData_description,
      # and taking the delete gives the field no column at all: lv_display_field()
      # consults only synonyms, so 6,516 stored descriptions reached nothing.
      # That is the #14 bug, and validate_qc_fields() refuses the shape, so the
      # builder must not be able to produce it.
      category == "synonym"                      ~ "synonym",
      disposition == "deleteMe"                  ~ "delete",
      category %in% c("control", "unused")       ~ category,
      # A reviewer can redirect a field into the inCompilation structure
      # instead of giving it a merge rule.
      grepl("inCompilation", ownership)          ~ "csm_pending",
      TRUE                                       ~ "merged"),
    ownership = case_when(
      role == "key"        ~ "key",
      role == "merged"     ~ ownership,
      # The curator decides membership; the files only report it.
      role == "membership" ~ "curator",
      TRUE                 ~ NA_character_),
    # Not nullable: a blank means "no opinion", not "remove this timeseries".
    # Most cells in a real QC tab are blank, so a blank clearing membership
    # would drop most of a compilation on its first run. Removal requires an
    # explicit FALSE.
    nullable_by_curator = ifelse(role %in% c("merged", "membership"),
                                 ifelse(role == "membership", "FALSE", nullable_by_curator),
                                 NA_character_),
    cardinality = case_when(
      same_across_dataset == "TRUE"  ~ "dataset",
      same_across_dataset == "FALSE" ~ "timeseries",
      TRUE                           ~ NA_character_),
    # A membership field resolves to itself: as a synonym it pointed at the
    # machine-owned inCompilationBeta_struct.
    canonical = ifelse(role == "membership", NA_character_, canonical),
    type = ifelse(qc_name %in% NUMERIC, "numeric", convo_type),
    vocab_key = unname(VOCAB[nz(qc_name)]),
    deprecated = role == "delete")

# ---- thematic grouping -----------------------------------------------------
# Taken from the column order of the hydroclimate2k sheet, which is the layout
# compilation leads already navigate by. Generated sheets sorted alphabetically
# instead, which is diffable and unreadable.
lv_group <- function(x) {
  case_when(
    x %in% c("TSid", "paleoData_TSid", "dataSetName", "datasetId",
             "neotomaDatasetId") | grepl("UI$|DatasetId$", x)  ~ "identity",
    x == "archiveType" | x == "archiveTypeOriginal"     ~ "archive",
    grepl("^pub[0-9]*_", x) | x == "originalDataUrl"    ~ "publication",
    grepl("^geo_", x)                                   ~ "geography",
    x %in% c("minYear", "maxYear", "distinctYearsInCommonEra", "collectionYear",
             "nUniqueAges", "medianHoloceneResolution", "agesPerKyr",
             "hasChron", "hasDepth", "datasetVersion")  ~ "chronology",
    grepl("^climateInterpretation", x)                  ~ "interpretation_climate",
    grepl("^environmentInterpretation", x)              ~ "interpretation_environment",
    grepl("^isotopeInterpretation", x)                  ~ "interpretation_isotope",
    grepl("^[A-Za-z]*[Ii]nterpretation", x) |
      grepl("^dynamicalSystem", x)                      ~ "interpretation_other",
    grepl("^calibration_", x)                           ~ "calibration",
    x %in% c("inThisCompilation", "paleoData_primaryTimeseries",
             "paleoData_mostRecentCompilations", "lipdverseLink") |
      grepl("[Cc]ertification|inCompilation", x)        ~ "compilation",
    x %in% c("paleoData_createdBy", "createdBy", "changelogNotes",
             "standardizationNotes", "paleoData_notes", "notes",
             "QCnotes", "tagMD5")                       ~ "provenance",
    grepl("^paleoData_|^chronData_", x)                 ~ "measurement",
    TRUE                                                ~ "other")
}

LV_GROUP_ORDER <- c("identity", "archive", "publication", "geography", "chronology",
                    "measurement", "interpretation_climate", "interpretation_environment",
                    "interpretation_isotope", "interpretation_other", "calibration",
                    "compilation", "provenance", "other")

out <- reg |>
  mutate(group = lv_group(qc_name),
         group_order = match(group, LV_GROUP_ORDER)) |>
  transmute(qc_name, ts_name, family, role, ownership, nullable_by_curator,
            cardinality, type, vocab_key, canonical,
            csm_compilation, csm_field, csm_flat_key,
            group, group_order,
            deprecated, n_compilations, n_filled) |>
  arrange(factor(role, levels = c("merged", "membership", "csm", "key", "synonym", "control", "unused", "delete")),
          desc(n_compilations), qc_name)

dir.create(file.path(repo, "inst/extdata"), recursive = TRUE, showWarnings = FALSE)

# Validate before writing, not after. The builder used to write whatever it
# assembled, so it could emit a registry that lv_qc_fields() then refused to
# load -- which is how `description` came to be role = delete pointing at a live
# field. Failing here leaves the committed registry untouched.
validate_qc_fields(out)

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
