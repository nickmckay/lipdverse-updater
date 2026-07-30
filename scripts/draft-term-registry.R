#!/usr/bin/env Rscript
#
# Draft a reviewable classification of every QC sheet term.
#
# Enumerates every column appearing in any compilation's QC sheet, gathers
# evidence about how it is used, and proposes one of:
#
#   standard     -- belongs in the LiPDverse standard; one canonical value per
#                   dataset, shared by every compilation containing it
#   compilation  -- compilation-specific; may legitimately differ, or is a field
#                   only one compilation wants
#   synonym      -- a variant/alias of another term (canonical named in
#                   suggested_canonical)
#   review       -- evidence is ambiguous; needs a human decision
#
# Output is a CSV for review. Nothing here is authoritative: the `suggested_*`
# columns are proposals and the evidence columns exist so they can be checked.
#
#   ./scripts/draft-term-registry.R
#   ./scripts/draft-term-registry.R --out=terms-draft.csv
#
# Reads only the local QC store; no network.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr)
})

args    <- commandArgs(trailingOnly = TRUE)
out     <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (!length(out)) out <- "terms-draft.csv"
qcstore <- Sys.getenv("LIPDVERSE_QCSTORE", path.expand("~/GitHub/lipdverse-qcstore"))
snapdir <- file.path(qcstore, "snapshots")
if (!dir.exists(snapdir)) stop("no snapshots at ", snapdir)

# ---- convo: the existing QC-column -> timeseries-field mapping -------------
# Sheet1 is the superset (231 rows) and matches the cached convo.csv; convoR
# (195) is a strict subset.
convo <- read_csv(file.path(snapdir, "_shared/convo/Sheet1.csv"),
                  col_types = cols(.default = col_character()), progress = FALSE) |>
  filter(!is.na(qcSheetName), nzchar(qcSheetName)) |>
  distinct(qcSheetName, .keep_all = TRUE)

# ---- read every compilation's QC tab --------------------------------------
dirs <- setdiff(list.dirs(snapdir, recursive = FALSE), file.path(snapdir, "_shared"))
qc <- list()
for (d in dirs) {
  f <- file.path(d, "QC.csv")
  if (!file.exists(f)) next
  x <- suppressWarnings(read_csv(f, col_types = cols(.default = col_character()),
                                 progress = FALSE, name_repair = "minimal"))
  x <- x[, !duplicated(names(x)) & nzchar(names(x)) & !is.na(names(x)), drop = FALSE]
  if (!"TSid" %in% names(x)) next
  qc[[basename(d)]] <- x[!is.na(x$TSid), , drop = FALSE]
}
message(sprintf("%d compilations with a QC tab", length(qc)))

# ---- per-term usage evidence ----------------------------------------------
usage <- imap_dfr(qc, function(x, comp) {
  tibble(
    term        = names(x),
    compilation = comp,
    n_rows      = nrow(x),
    n_filled    = map_int(x, ~ sum(!is.na(.x) & .x != "")),
    n_distinct  = map_int(x, ~ dplyr::n_distinct(.x[!is.na(.x) & .x != ""]))
  )
})

per_term <- usage |>
  group_by(term) |>
  summarise(
    n_compilations = n_distinct(compilation),
    compilations   = paste(sort(unique(compilation)), collapse = ";"),
    n_filled       = sum(n_filled),
    n_rows         = sum(n_rows),
    max_distinct   = max(n_distinct),
    .groups = "drop"
  ) |>
  mutate(fill_rate = round(n_filled / pmax(n_rows, 1), 3))

n_comp_total <- length(qc)

# ---- does the value ever differ between compilations for the same TSid? ----
# Only meaningful for terms present in 2+ compilations.
shared_terms <- per_term$term[per_term$n_compilations > 1]
shared_terms <- setdiff(shared_terms, c("TSid"))

long <- imap_dfr(qc, function(x, comp) {
  k <- intersect(shared_terms, names(x))
  if (!length(k)) return(tibble())
  pivot_longer(x[, c("TSid", k), drop = FALSE], all_of(k),
               names_to = "term", values_to = "value") |>
    filter(!is.na(value), value != "") |>
    mutate(compilation = comp)
})

variance <- long |>
  group_by(term, TSid) |>
  summarise(n_comp = n_distinct(compilation),
            n_val  = n_distinct(value),
            n_norm = n_distinct(tolower(trimws(gsub("[^A-Za-z0-9]", "", value)))),
            .groups = "drop") |>
  filter(n_comp > 1) |>
  group_by(term) |>
  summarise(n_shared_tsids   = n(),
            n_conflict       = sum(n_val > 1),
            n_conflict_subst = sum(n_norm > 1),
            .groups = "drop") |>
  mutate(conflict_rate = round(n_conflict / pmax(n_shared_tsids, 1), 4))

examples <- long |>
  group_by(term) |>
  slice_head(n = 400) |>
  summarise(example_values = paste(head(unique(value), 4), collapse = " | "), .groups = "drop")

# Terms present in only one compilation get examples too.
solo <- usage |> filter(n_filled > 0) |> anti_join(examples, by = "term")
solo_ex <- map_dfr(unique(solo$term), function(tm) {
  v <- unlist(lapply(qc, function(x) if (tm %in% names(x)) x[[tm]] else NULL), use.names = FALSE)
  v <- unique(v[!is.na(v) & v != ""])
  tibble(term = tm, example_values = paste(head(v, 4), collapse = " | "))
})

# ---- synonym detection -----------------------------------------------------
# convo is 1:1 (no two qcSheetNames map to one tsName), so aliases have to be
# found by normalising away the prefixes LiPD uses for the same concept.
# Strip only the INDEX digits and punctuation, never the semantic prefix:
# climateInterpretation2_* and environmentInterpretation1_* are different
# scopes, and collapsing both to "interpretationApplies" would declare them
# synonyms of each other. Legacy short names (basis -> climateInterpretation1_basis)
# are already covered by explicit convo entries, so the normaliser can afford
# to be conservative.
norm <- function(s) {
  s <- tolower(s)
  s <- gsub("([a-z])[0-9]+", "\\1", s)   # pub1_doi -> pub_doi
  gsub("[^a-z0-9]", "", s)
}
convo_norm <- convo |> mutate(nm = norm(qcSheetName), nm_ts = norm(tsName))
lookup <- setNames(convo$qcSheetName, convo_norm$nm)
lookup_ts <- setNames(convo$qcSheetName, convo_norm$nm_ts)

# ---- canonical vs alias ----------------------------------------------------
# convo is already the field-name synonym table: 82 of its 231 rows have
# qcSheetName != tsName, meaning the QC sheet column is a display/short name
# for a differently-named canonical field (lat -> geo_latitude, basis ->
# climateInterpretation1_basis). Those QC columns are synonyms, and the
# canonical term is the tsName.
#
# Usage has to be attributed to the canonical: geo_latitude never appears as a
# QC column, but its alias `lat` appears in every compilation, so geo_latitude
# is a ubiquitous standard term.
alias_map <- convo |>
  filter(!is.na(tsName), nzchar(tsName), qcSheetName != tsName) |>
  select(alias = qcSheetName, canonical = tsName)

canon_of <- function(x) {
  i <- match(x, alias_map$alias)
  ifelse(is.na(i), x, alias_map$canonical[i])
}

# Roll every column's usage up to the term it actually populates.
per_canon <- usage |>
  mutate(term = canon_of(term)) |>
  group_by(term) |>
  summarise(
    n_compilations = n_distinct(compilation),
    compilations   = paste(sort(unique(compilation)), collapse = ";"),
    n_filled       = sum(n_filled),
    n_rows         = sum(n_rows),
    max_distinct   = max(n_distinct),
    .groups = "drop"
  ) |>
  mutate(fill_rate = round(n_filled / pmax(n_rows, 1), 3))

# Universe: everything observed, plus every canonical name reachable from it.
all_terms <- union(per_term$term, per_canon$term)

per_term <- tibble(term = all_terms) |>
  left_join(per_canon, by = "term") |>
  mutate(across(c(n_compilations, n_filled, n_rows, max_distinct), ~ tidyr::replace_na(.x, 0)),
         compilations = tidyr::replace_na(compilations, ""),
         fill_rate    = tidyr::replace_na(fill_rate, 0)) |>
  left_join(alias_map |> select(term = alias, alias_for = canonical), by = "term") |>
  left_join(alias_map |> group_by(canonical) |> summarise(aliases = paste(alias, collapse = ";")) |>
              rename(term = canonical), by = "term") |>
  # An alias's own usage, for the synonym rows.
  left_join(usage |> group_by(term) |> summarise(alias_n_filled = sum(n_filled),
                                                 alias_compilations = paste(sort(unique(compilation)), collapse = ";"),
                                                 .groups = "drop"),
            by = "term")

# ---- assemble --------------------------------------------------------------
terms <- per_term |>
  left_join(variance, by = "term") |>
  left_join(bind_rows(examples, solo_ex) |> distinct(term, .keep_all = TRUE), by = "term") |>
  left_join(convo |> select(term = qcSheetName, ts_name = tsName,
                            convo_type = type, same_across_dataset = sameAcrossDataset),
            by = "term") |>
  mutate(
    is_canonical = term %in% convo$tsName,
    # convo knows a term either as a QC column name or as a canonical tsName.
    # Joining only on qcSheetName would report paleoData_description as unknown
    # when convo lists it as the canonical target of `description`.
    in_convo    = !is.na(ts_name) | is_canonical,
    nm          = norm(term),
    # Heuristic alias detection, for columns convo does not already map. Never
    # applied to a canonical term: matching geo_latitude against the reverse
    # lookup would otherwise declare it an alias of its own alias `lat`.
    alias_of = ifelse(!in_convo & !is_canonical & nm %in% names(lookup),    unname(lookup[nm]),
               ifelse(!in_convo & !is_canonical & nm %in% names(lookup_ts), unname(lookup_ts[nm]), NA_character_)),
    ubiquity = round(n_compilations / n_comp_total, 2),
    # Indexed terms (climateInterpretation1_*, pub2_doi, ...) are one decision,
    # not twelve. Grouping them cuts the review from ~200 rows to ~90 families.
    family = gsub("([A-Za-z]+)[0-9]+([_.])", "\\1*\\2", term),
    # A term whose name carries a compilation's name is almost certainly local
    # to it (iso2kUI, isotopeInterpretation* which only iso2k and its
    # descendants use).
    name_flags_compilation = grepl(
      paste0("(?i)(", paste(gsub("[^A-Za-z0-9]", "", names(qc)), collapse = "|"), ")"),
      gsub("[^A-Za-z0-9]", "", term), perl = TRUE)
  )

# ---- proposal --------------------------------------------------------------
# Ordered rules; first match wins. Deliberately conservative: anything that
# does not fit a clear pattern is sent to `review` rather than guessed.
terms <- terms |>
  mutate(
    suggested_category = case_when(
      term %in% c("TSid", "dataSetName", "datasetId")  ~ "key",
      # Instructions to the updater, not dataset metadata. Always empty because
      # the pipeline consumes and clears them.
      term %in% c("changelogNotes", "standardizationNotes",
                  "instructions")                      ~ "control",
      # Recorded in convo as a display name for a differently-named canonical
      # field. These are the bulk of the field-name synonyms.
      !is.na(alias_for)                                ~ "synonym",
      !is.na(alias_of)                                 ~ "synonym",
      # Never populated anywhere: a column that exists but carries no data.
      n_filled == 0                                    ~ "unused",
      # Broad adoption is the strongest evidence of a corpus-wide term,
      # whether or not convo happens to list it.
      n_compilations >= 0.5 * n_comp_total             ~ "standard",
      in_convo & n_compilations >= 4                   ~ "standard",
      # Local to one or two compilations, and carrying real data.
      name_flags_compilation                           ~ "compilation",
      n_compilations <= 2                              ~ "compilation",
      TRUE                                             ~ "review"
    ),
    suggested_canonical = coalesce(alias_for, alias_of),
    # For standard terms, does the evidence say it can vary per compilation?
    suggested_scope = case_when(
      suggested_category != "standard"          ~ NA_character_,
      is.na(n_conflict_subst)                   ~ "global",
      n_conflict_subst > 0                      ~ "global (has conflicts -- see note)",
      TRUE                                      ~ "global"
    ),
    rationale = case_when(
      suggested_category == "key"      ~ "identifier",
      suggested_category == "control"  ~ "pipeline instruction column, not dataset metadata",
      suggested_category == "synonym" & !is.na(alias_for) ~
        paste0("convo maps this QC column to canonical '", alias_for, "'"),
      suggested_category == "synonym"  ~ paste0("normalises to convo term '", alias_of, "'"),
      suggested_category == "unused"   ~ paste0("present in ", n_compilations, " sheet(s), never populated",
                                                ifelse(in_convo, "; in convo", "; not in convo")),
      suggested_category == "standard" ~ paste0("used by ", n_compilations, "/", n_comp_total,
                                                " compilations", ifelse(in_convo, "; in convo", "; NOT in convo")),
      suggested_category == "compilation" & name_flags_compilation ~
        paste0("term name references a compilation; used by ", compilations),
      suggested_category == "compilation" ~ paste0("only ", compilations,
                                                   ifelse(in_convo, "; in convo but unadopted", "; not in convo")),
      TRUE ~ paste0("used by ", n_compilations, "/", n_comp_total,
                    " compilations", ifelse(in_convo, "; in convo", "; NOT in convo"))
    ),
    note = case_when(
      !is.na(n_conflict_subst) & n_conflict_subst > 0 ~
        paste0(n_conflict_subst, " substantive cross-compilation conflicts"),
      TRUE ~ ""
    )
  )

final <- terms |>
  transmute(
    family, term,
    suggested_category,
    suggested_canonical,
    suggested_scope,
    decision = "",
    canonical_if_synonym = "",
    nick_notes = "",
    rationale, note,
    aliases, alias_n_filled, alias_compilations,
    in_convo, ts_name, convo_type, same_across_dataset,
    n_compilations, ubiquity, compilations,
    n_filled, fill_rate, max_distinct,
    n_shared_tsids, n_conflict, n_conflict_subst, conflict_rate,
    example_values
  ) |>
  # Group each indexed family together and put the least certain first, so a
  # top-down pass hits the decisions that actually need a human.
  arrange(factor(suggested_category,
                 levels = c("review", "compilation", "synonym", "standard", "control", "unused", "key")),
          family, term)

write_csv(final, out, na = "")

cat("\n")
cat("terms found: ", nrow(final), "  across ", n_comp_total, " compilations\n\n", sep = "")
print(as.data.frame(count(final, suggested_category, name = "n") |> arrange(desc(n))), row.names = FALSE)
cat("\nwrote ", out, "\n", sep = "")
