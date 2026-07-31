# Merge rules, and regressions for the losses that motivated the rewrite.

test_registry <- function() {
  tibble::tribble(
    ~qc_name,                            ~role,     ~ownership, ~nullable_by_curator,
    "environmentInterpretation1_variable", "merged", "curator",  "TRUE",
    "QC Certification",                    "merged", "curator",  "TRUE",
    "paleoData_createdBy",                 "merged", "machine",  "FALSE",
    "minYear",                             "merged", "machine",  "FALSE",
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

# ---- the rule table --------------------------------------------------------

test_that("nothing changes when all three agree", {
  a <- cells("T1", "archiveType", "coral")
  p <- merge3(a, a, a)
  expect_equal(res(p, "T1", "archiveType")$resolution, "unchanged")
  expect_equal(p$summary$n_changed, 0)
})

test_that("a curator edit wins when the file is unchanged", {
  b <- cells("T1", "archiveType", "coral")
  s <- cells("T1", "archiveType", "Coral")
  p <- merge3(b, s, b)
  expect_equal(res(p, "T1", "archiveType")$resolution, "sheet")
  expect_equal(res(p, "T1", "archiveType")$value, "Coral")
})

test_that("a file change wins when the sheet is unchanged", {
  b <- cells("T1", "archiveType", "coral")
  f <- cells("T1", "archiveType", "Coral")
  p <- merge3(b, b, f)
  expect_equal(res(p, "T1", "archiveType")$resolution, "file")
  expect_equal(res(p, "T1", "archiveType")$value, "Coral")
})

test_that("both sides changing to the same value converges", {
  b <- cells("T1", "archiveType", "coral")
  n <- cells("T1", "archiveType", "Coral")
  p <- merge3(b, n, n)
  expect_equal(res(p, "T1", "archiveType")$resolution, "converged")
})

test_that("divergence resolves by ownership", {
  b <- cells("T1", "QC Certification", "A")
  expect_equal(res(merge3(b, cells("T1","QC Certification","B"),
                             cells("T1","QC Certification","C")), "T1", "QC Certification")$value, "B")

  b2 <- cells("T1", "minYear", "1000")
  expect_equal(res(merge3(b2, cells("T1","minYear","1100"),
                              cells("T1","minYear","1200")), "T1", "minYear")$value, "1200")
})

test_that("a shared field diverging is a conflict, and base is retained", {
  b <- cells("T1", "archiveType", "coral")
  p <- merge3(b, cells("T1","archiveType","Coral"), cells("T1","archiveType","CORAL"))
  expect_equal(res(p, "T1", "archiveType")$resolution, "conflict")
  expect_equal(res(p, "T1", "archiveType")$value, "coral")
  expect_equal(p$summary$n_conflicts, 1)
})

test_that("a key field disagreeing is an error", {
  p <- merge3(cells("T1","TSid","T1"), cells("T1","TSid","T9"), cells("T1","TSid","T8"))
  expect_equal(res(p, "T1", "TSid")$resolution, "error")
  expect_error(qc_plan_check(p), class = "lv_error_conflict")
})

test_that("strict mode aborts on conflicts and writes the report", {
  b <- cells("T1", "archiveType", "coral")
  p <- merge3(b, cells("T1","archiveType","Coral"), cells("T1","archiveType","CORAL"), strict = TRUE)
  f <- withr::local_tempfile(fileext = ".csv")
  expect_error(qc_plan_check(p, f), class = "lv_error_conflict")
  expect_true(file.exists(f))
})

# ---- the clear rule --------------------------------------------------------

# This is the defect. daff read NA arriving from the files as "b deleted this"
# and wiped curated environmentInterpretation values. lipdverseR papered over it
# with an NA-backfill pass before every merge.
test_that("a blank from the files never deletes a curated value", {
  b <- cells("T1", "environmentInterpretation1_variable", "precipitation")
  f <- cells("T1", "environmentInterpretation1_variable", NA_character_)
  p <- merge3(b, b, f)

  expect_equal(res(p, "T1", "environmentInterpretation1_variable")$resolution, "unchanged")
  expect_equal(res(p, "T1", "environmentInterpretation1_variable")$value, "precipitation")
})

test_that("a field absent from the files entirely never deletes", {
  b <- cells("T1", "environmentInterpretation1_variable", "precipitation")
  p <- merge3(b, b, qc_cells_empty())
  expect_equal(res(p, "T1", "environmentInterpretation1_variable")$value, "precipitation")
})

test_that("a blank from the sheet clears only where the curator may clear it", {
  b <- cells("T1", "QC Certification", "A")
  s <- cells("T1", "QC Certification", NA_character_)
  p <- merge3(b, s, b)
  expect_true(p$cells$sheet_clears[1])
  expect_true(is.na(res(p, "T1", "QC Certification")$value))

  # A machine-owned field is not clearable, so blank means unchanged.
  b2 <- cells("T1", "minYear", "1000")
  p2 <- merge3(b2, cells("T1","minYear",NA_character_), b2)
  expect_false(p2$cells$sheet_clears[1])
  expect_equal(res(p2, "T1", "minYear")$value, "1000")
})

test_that("the sentinel clears a field that is otherwise not nullable", {
  b <- cells("T1", "archiveType", "coral")
  p <- merge3(b, cells("T1","archiveType","<<CLEAR>>"), b)
  expect_true(p$cells$sheet_clears[1])
  expect_true(is.na(res(p, "T1", "archiveType")$value))
})

# ---- incident regressions --------------------------------------------------

# 24 paleoData_createdBy values were destroyed by a broken run. lipdverseR's fix
# was a post-merge `sticky_fields` restore; here the clear rule covers it,
# because a machine-owned field cannot be cleared by a blank.
test_that("paleoData_createdBy survives blanks on both sides", {
  b <- cells("T1", "paleoData_createdBy", "nicholas")
  p <- merge3(b, cells("T1","paleoData_createdBy",NA_character_),
                 cells("T1","paleoData_createdBy",NA_character_))
  expect_equal(res(p, "T1", "paleoData_createdBy")$value, "nicholas")
  expect_equal(res(p, "T1", "paleoData_createdBy")$resolution, "unchanged")
})

test_that("paleoData_createdBy is not resurrected once genuinely rewritten", {
  b <- cells("T1", "paleoData_createdBy", "nicholas")
  p <- merge3(b, b, cells("T1","paleoData_createdBy","pipeline"))
  expect_equal(res(p, "T1", "paleoData_createdBy")$value, "pipeline")
})

# hydroclimate2k v0.4.0: records LS12THAY and LS14FEZA came back with
# interpretation metadata that did not match what the curators had entered.
test_that("H2k v0.4.0: curated interpretation survives an empty file side", {
  for (ts in c("LS12THAY", "LS14FEZA")) {
    b <- cells(ts, "environmentInterpretation1_variable", "effectivePrecipitation")
    s <- cells(ts, "environmentInterpretation1_variable", "effectivePrecipitation")
    f <- cells(ts, "environmentInterpretation1_variable", "")
    p <- merge3(b, s, f)
    expect_equal(res(p, ts, "environmentInterpretation1_variable")$value,
                 "effectivePrecipitation",
                 info = ts)
  }
})

test_that("a curator edit is not reverted by a stale file value", {
  b <- cells("LS12THAY", "QC Certification", "")
  s <- cells("LS12THAY", "QC Certification", "GF")
  f <- cells("LS12THAY", "QC Certification", "")
  p <- merge3(b, s, f)
  expect_equal(res(p, "LS12THAY", "QC Certification")$value, "GF")
})

# ---- properties ------------------------------------------------------------

test_that("merging a state with itself changes nothing", {
  x <- dplyr::bind_rows(
    cells("T1", "archiveType", "coral"),
    cells("T1", "QC Certification", "A"),
    cells("T2", "minYear", "1000")
  )
  p <- merge3(x, x, x)
  expect_equal(p$summary$n_changed, 0)
  expect_equal(p$summary$n_conflicts, 0)
  expect_setequal(qc_plan_state(p)$value, x$value)
})

test_that("a curator value is never lost across the whole rule table", {
  vals <- list("A", "B", NA_character_, "")
  lost <- 0
  for (bv in vals) for (sv in vals) for (fv in vals) {
    b <- cells("T1", "environmentInterpretation1_variable", bv)
    s <- cells("T1", "environmentInterpretation1_variable", sv)
    f <- cells("T1", "environmentInterpretation1_variable", fv)
    out <- res(merge3(b, s, f), "T1", "environmentInterpretation1_variable")$value
    had <- !is.na(bv) && nzchar(bv)
    sheet_cleared <- is.na(sv) || !nzchar(sv)
    # A populated base may only end up empty when the sheet cleared it, which
    # for a nullable curator field is a legitimate blank.
    if (had && (is.na(out) || !nzchar(out)) && !sheet_cleared) lost <- lost + 1
  }
  expect_equal(lost, 0)
})

test_that("fields outside the merge roles are ignored", {
  reg <- dplyr::mutate(test_registry(),
                       role = ifelse(qc_name == "archiveType", "csm", role))
  p <- qc_merge(cells("T1","archiveType","coral"),
                cells("T1","archiveType","Coral"),
                cells("T1","archiveType","coral"),
                registry = reg, policy = qc_merge_policy(strict = FALSE))
  expect_equal(nrow(p$cells), 0)
})

test_that("a field missing from the registry is reported, not silently merged", {
  p <- merge3(cells("T1","notAField","x"), cells("T1","notAField","y"), cells("T1","notAField","x"))
  expect_equal(p$summary$n_unknown_fields, 1)
})
