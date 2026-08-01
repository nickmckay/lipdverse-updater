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

# ---- regressions found by running against real data ------------------------

# The sheet writes 2001.54167 and the file 2001.5417: the same measurement
# rounded differently. Comparing as text made every run report thousands of
# changes on minYear/maxYear/lat/lon, which would churn the store and the sheet
# and bury real edits.
test_that("values differing only in precision are not changes", {
  b <- cells("T1", "minYear", "1770.7917")
  s <- cells("T1", "minYear", "1770.79167")
  p <- merge3(b, s, b)
  expect_equal(res(p, "T1", "minYear")$resolution, "unchanged")
})

test_that("a real numeric change at the shared precision is still detected", {
  b <- cells("T1", "minYear", "1770.7917")
  s <- cells("T1", "minYear", "1770.7918")
  expect_equal(res(merge3(b, s, b), "T1", "minYear")$resolution, "sheet")
})

# Google Sheets drops leading and trailing whitespace, so a paleoData_description
# ending in a newline came back from the sheet without it. Four cells in
# lipdverseTest conflicted on the very first run for no reason a curator would
# recognise, and on a curator-owned field the sheet would have written its
# trimmed value into the file.
test_that("whitespace the sheet cannot represent is not a change", {
  b <- cells("T1", "paleoData_description", "modeled age error\n")
  s <- cells("T1", "paleoData_description", "modeled age error")
  expect_equal(res(merge3(b, s, b), "T1", "paleoData_description")$resolution, "unchanged")

  b2 <- cells("T1", "paleoData_description", "\tLoisel et al. 2014")
  s2 <- cells("T1", "paleoData_description", "Loisel et al. 2014")
  expect_equal(res(merge3(b2, s2, b2), "T1", "paleoData_description")$resolution, "unchanged")
})

test_that("a change in interior whitespace is still a change", {
  b <- cells("T1", "paleoData_description", "modeled  age error")
  s <- cells("T1", "paleoData_description", "modeled age error")
  expect_equal(res(merge3(b, s, b), "T1", "paleoData_description")$resolution, "sheet")
})

test_that("text values that are not numbers still compare as text", {
  b <- cells("T1", "archiveType", "coral")
  expect_equal(res(merge3(b, cells("T1","archiveType","Coral"), b), "T1", "archiveType")$resolution,
               "sheet")
})

# A cell the sheet does not carry is not a cell the curator emptied. Without
# this distinction every absent cell reads as a deletion, which is the same
# conflation that made daff destroy curated values.
test_that("a cell absent from the sheet does not clear a curated value", {
  b <- cells("T1", "QC Certification", "A")
  p <- merge3(b, qc_cells_empty(), b)            # sheet has no such cell at all
  expect_false(any(p$cells$sheet_clears))
  expect_equal(res(p, "T1", "QC Certification")$value, "A")
})

test_that("a cell the sheet carries but leaves blank does clear it", {
  b <- cells("T1", "QC Certification", "A")
  s <- cells("T1", "QC Certification", NA_character_)   # present, empty
  p <- merge3(b, s, b)
  expect_true(any(p$cells$sheet_clears))
  expect_true(is.na(res(p, "T1", "QC Certification")$value))
})

# Merging a settled state against unchanged inputs must do nothing, or every
# run rewrites the store and the sheet.
test_that("the merge is idempotent", {
  s <- dplyr::bind_rows(cells("T1","minYear","1770.79167"), cells("T1","archiveType","coral"))
  f <- dplyr::bind_rows(cells("T1","minYear","1770.7917"),  cells("T1","archiveType","coral"))
  p1 <- merge3(qc_cells_empty(), s, f)
  p2 <- merge3(qc_plan_state(p1), s, f)
  expect_equal(p2$summary$n_changed, 0)
  expect_equal(p2$summary$n_cleared, 0)
})

# The frame emitted datasetId as a cell while the sheet read it into the key
# column, so on lipdverseTest it resolved to "file" on all 2,689 timeseries --
# one spurious event per column, on every run, forever.
test_that("identity fields are not cells", {
  d <- withr::local_tempdir()
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  f <- qc_frame(d, progress = FALSE)
  expect_false(any(c("TSid", "datasetId") %in% f$field))
})

# Interpretation is a column-level concept, but 624 datasets carry flattened
# copies at the dataset root. Reading those replicated one column's
# interpretation onto every column, and shadowed the real per-column values.
test_that("interpretations are read per column, with scope-aware indices", {
  d <- withr::local_tempdir()
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))

  L <- lipdR::readLipd(fs::path(d, "A.Author.2001.lpd"))
  L$environmentInterpretation1_variable <- "fromTheRoot"
  tab <- L$paleoData[[1]]$measurementTable[[1]]
  cn <- names(Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tab))[1]
  L$paleoData[[1]]$measurementTable[[1]][[cn]]$interpretation <- list(
    list(scope = "climate", variable = "temperature"),
    list(scope = "isotope", variable = "precipitationIsotope"))
  out <- withr::local_tempdir()
  lipdR::writeLipd(L, path = out, removeNamesFromLists = TRUE)

  f <- qc_frame(out, progress = FALSE)
  get <- function(ts, fld) f$value[f$tsid == ts & f$field == fld]

  # The isotope interpretation is the second entry but the first of its scope.
  expect_equal(get("T1", "climateInterpretation1_variable"), "temperature")
  expect_equal(get("T1", "isotopeInterpretation1_variable"), "precipitationIsotope")
  # The root copy is not read, so it does not leak onto the other column.
  expect_length(get("T2", "environmentInterpretation1_variable"), 0)
})

# The root copies are being removed from the files, but a dataset that has not
# been migrated yet must still not have them read: a root value describes the
# whole dataset, so reading it puts one column's interpretation onto every
# column, which is how the per-column read gap stayed hidden.
test_that("a root interpretation key is ignored even when no column has one", {
  d <- withr::local_tempdir()
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  L <- lipdR::readLipd(fs::path(d, "A.Author.2001.lpd"))
  L$environmentInterpretation1_variable <- "effectiveMoisture"
  L$Interpretation1_scope <- "climate"
  out <- withr::local_tempdir()
  lipdR::writeLipd(L, path = out, removeNamesFromLists = TRUE)

  f <- qc_frame(out, progress = FALSE)
  expect_false(any(grepl("nterpretation", f$field)))
})
