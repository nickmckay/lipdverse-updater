# Extracted from test-qc-merge.R:243

# prequel ----------------------------------------------------------------------
test_registry <- function() {
  tibble::tribble(
    ~qc_name,                            ~role,     ~ownership, ~nullable_by_curator,
    "environmentInterpretation1_variable", "merged", "curator",  "TRUE",
    "QC Certification",                    "merged", "curator",  "TRUE",
    "paleoData_createdBy",                 "merged", "machine",  "FALSE",
    "minYear",                             "merged", "machine",  "FALSE",
    "collectionYear",                      "merged", "curator",  "FALSE",
    "archiveType",                         "merged", "shared",   "FALSE",
    "paleoData_variableName",              "merged", "shared",   "FALSE",
    "TSid",                                "key",    "key",      NA_character_
  ) |>
    dplyr::mutate(ts_name = qc_name, family = qc_name, cardinality = "timeseries",
                  type = "character", vocab_key = NA_character_, canonical = NA_character_,
                  csm_compilation = NA_character_, csm_field = NA_character_,
                  csm_flat_key = NA_character_, deprecated = FALSE,
                  n_compilations = 1L, n_filled = 1L)
}
cells <- function(...) {
  x <- tibble::tribble(~tsid, ~field, ~value, ...)
  x$present <- TRUE
  x$dataset_id <- "DS1"
  x
}
merge3 <- function(base, sheet, frame, strict = FALSE) {
  qc_merge(base, sheet, frame, registry = test_registry(),
           policy = qc_merge_policy(strict = strict))
}
res <- function(plan, tsid, field) {
  i <- plan$cells$tsid == tsid & plan$cells$field == field
  list(resolution = plan$cells$resolution[i], value = plan$cells$value[i])
}

# test -------------------------------------------------------------------------
b <- cells("T1", "collectionYear", "1770.7917")
s <- cells("T1", "collectionYear", "1770.79167")
p <- merge3(b, s, b)
expect_equal(res(p, "T1", "collectionYear")$resolution, "unchanged")
