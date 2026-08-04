test_that("lv_field_path decomposes every field shape", {
  p <- lv_field_path(c("archiveType", "geo_latitude", "pub1_doi", "pub3_author",
                       "paleoData_units", "chronData_depth", "calibration_method",
                       "climateInterpretation1_basis", "interpretation2_variable"))

  expect_equal(p$container, c("root", "geo", "pub", "pub", "column", "column",
                              "calibration", "interpretation", "interpretation"))
  expect_equal(p$key, c("archiveType", "latitude", "doi", "author", "units", "depth",
                        "method", "basis", "variable"))
  expect_equal(p$index, c(NA, NA, 1L, 3L, NA, NA, NA, 1L, 2L))
  # The scope lives in the name for scoped interpretations, and lipdR keeps it
  # as a field on the interpretation.
  expect_equal(p$scope[8], "climate")
  expect_true(is.na(p$scope[9]))
  expect_equal(p$level, c("dataset", "dataset", "dataset", "dataset", "column",
                          "column", "column", "column", "column"))
})

test_that("an unrecognised field falls through to the dataset root", {
  p <- lv_field_path("somethingNew")
  expect_equal(p$container, "root")
  expect_equal(p$key, "somethingNew")
})

reg_for <- function(fields, ownership = "curator") {
  tibble::tibble(qc_name = fields, role = "merged", ownership = ownership,
                 nullable_by_curator = "TRUE", ts_name = fields, family = fields,
                 cardinality = "timeseries", type = "character",
                 vocab_key = NA_character_, canonical = NA_character_,
                 csm_compilation = NA_character_, csm_field = NA_character_,
                 csm_flat_key = NA_character_, deprecated = FALSE,
                 n_compilations = 1L, n_filled = 1L)
}

cellset <- function(...) {
  x <- tibble::tribble(~tsid, ~field, ~value, ...)
  x$present <- TRUE; x$dataset_id <- "ID"; x
}

apply_to <- function(cells, fields, tsids = c("T1", "T2")) {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir(.local_envir = parent.frame()))
  src <- withr::local_tempdir(.local_envir = parent.frame())
  out <- withr::local_tempdir(.local_envir = parent.frame())
  write_lpd(src, "A.Author.2001", tsids = tsids)
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  iss <- lv_apply_qc(cells, src, out, registry = reg_for(fields), index = idx, progress = FALSE)
  f <- fs::path(out, "A.Author.2001.lpd")
  # Nothing is written when no cell applies, so the caller may get no file.
  list(L = if (fs::file_exists(f)) lipdR::readLipd(f) else NULL, issues = iss)
}

test_that("a column-level value is written to the right column", {
  r <- apply_to(cellset("T1", "paleoData_units", "permil"), "paleoData_units")
  tb <- r$L$paleoData[[1]]$measurementTable[[1]]
  hit <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tb)
  expect_equal(hit[[1]]$units, "permil")
  expect_equal(nrow(r$issues), 0)
})

test_that("a dataset-level value is written once, not per column", {
  r <- apply_to(cellset("T1", "archiveType", "Coral",
                        "T2", "archiveType", "Coral"), "archiveType")
  expect_equal(r$L$archiveType, "Coral")
})

test_that("geo and pub values land in their containers", {
  r <- apply_to(cellset("T1", "geo_siteName", "Somewhere",
                        "T1", "pub1_doi", "10.1234/x"),
                c("geo_siteName", "pub1_doi"))
  expect_equal(r$L$geo$siteName, "Somewhere")
  expect_equal(r$L$pub[[1]]$doi, "10.1234/x")
})

test_that("a scoped interpretation is created with its scope", {
  r <- apply_to(cellset("T1", "climateInterpretation1_variable", "temperature"),
                "climateInterpretation1_variable")
  tb <- r$L$paleoData[[1]]$measurementTable[[1]]
  hit <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tb)
  interp <- hit[[1]]$interpretation[[1]]
  expect_equal(interp$variable, "temperature")
  expect_equal(interp$scope, "climate")
})

test_that("the next publication slot is appended", {
  # The fixture has one publication, so pub2 is the next slot.
  r <- apply_to(cellset("T1", "pub2_doi", "10.1234/second"), "pub2_doi")
  expect_length(r$L$pub, 2)
  expect_equal(r$L$pub[[2]]$doi, "10.1234/second")
})

# Reaching a distant index would mean inventing empty publications, and lipdR
# drops those on write -- so the value would silently land on a different
# publication than the one it names.
test_that("a gap in publication indices is reported, not fabricated", {
  r <- apply_to(cellset("T1", "pub4_doi", "10.1234/fourth"), "pub4_doi")
  expect_true("pub_index_gap" %in% r$issues$check)
  expect_length(r$L$pub, 1)
})

test_that("values are written with a sensible type, not as quoted strings", {
  expect_type(lv_coerce_value("1850"), "double")
  expect_type(lv_coerce_value("TRUE"), "logical")
  expect_type(lv_coerce_value("coral"), "character")
  # A version-like string must not be mangled into a number.
  expect_type(lv_coerce_value("1_0_3"), "character")
  expect_type(lv_coerce_value("2026-01-01"), "character")
  # Clearing a cell removes the field.
  expect_null(lv_coerce_value(NA_character_))
})

test_that("a cleared cell removes the field from the file", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = c("T1", "T2"))
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_apply_qc(cellset("T1", "paleoData_units", NA_character_), src, out,
              registry = reg_for("paleoData_units"), index = idx, progress = FALSE)
  L <- lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))
  tb <- L$paleoData[[1]]$measurementTable[[1]]
  hit <- Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tb)
  expect_null(hit[[1]]$units)
})

test_that("a TSid that is not in the database is reported, not silently dropped", {
  r <- apply_to(cellset("NOPE", "paleoData_units", "permil"), "paleoData_units")
  expect_equal(r$issues$check, "tsid_not_in_database")
  expect_equal(r$issues$severity, "warn")
})

test_that("fields outside the merge roles are ignored", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001")
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  reg <- dplyr::mutate(reg_for("paleoData_units"), role = "csm")
  iss <- lv_apply_qc(cellset("T1", "paleoData_units", "permil"), src, out,
                     registry = reg, index = idx, progress = FALSE)
  expect_equal(nrow(iss), 0)
  expect_equal(length(fs::dir_ls(out, glob = "*.lpd")), 0)
})

test_that("apply never writes in place", {
  expect_error(lv_apply_qc(cellset("T1", "archiveType", "x")), "never writes in place")
})

# The output has to survive the writer's own gate, or the pipeline cannot
# complete a cycle.
test_that("applied output passes verification", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = c("T1", "T2"))
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_apply_qc(cellset("T1", "paleoData_units", "permil"), src, out,
              registry = reg_for("paleoData_units"), index = idx, progress = FALSE)
  expect_equal(nrow(lv_verify_file(fs::path(out, "A.Author.2001.lpd"), "A.Author.2001")), 0)
})

# A column can hold three climate interpretations followed by three isotope
# ones. The index in a QC field name counts within a scope, but lipdR stores
# one flat list -- so writing environmentInterpretation1 to interpretation[[1]]
# overwrote a climate interpretation's variable on IC00OE02.
test_that("a scoped interpretation never lands on another scope's slot", {
  interp <- list(list(scope = "climate", variable = "temperature"),
                 list(scope = "climate"),
                 list(scope = "isotope", variable = "precipitationIsotope"))

  expect_equal(lv_interp_slot(interp, "climate", 1L), 1L)
  expect_equal(lv_interp_slot(interp, "climate", 2L), 2L)
  expect_equal(lv_interp_slot(interp, "isotope", 1L), 3L)
  # No environment interpretation exists yet, so the caller must append.
  expect_true(is.na(lv_interp_slot(interp, "environment", 1L)))

  col <- lv_apply_to_column(
    list(TSid = "T1", interpretation = interp),
    cellset("T1", "environmentInterpretation1_variable", "effectiveMoisture") |>
      dplyr::mutate(container = "interpretation", index = 1L, key = "variable",
                    scope = "environment", level = "column"))

  expect_length(col$interpretation, 4)
  expect_equal(col$interpretation[[1]]$variable, "temperature")   # untouched
  expect_equal(col$interpretation[[4]]$scope, "environment")
  expect_equal(col$interpretation[[4]]$variable, "effectiveMoisture")
})

test_that("an existing scoped slot is updated in place, not appended", {
  col <- lv_apply_to_column(
    list(TSid = "T1", interpretation = list(list(scope = "climate", variable = "temperature"),
                                            list(scope = "isotope", variable = "d18O"))),
    cellset("T1", "isotopeInterpretation1_variable", "precipitationIsotope") |>
      dplyr::mutate(container = "interpretation", index = 1L, key = "variable",
                    scope = "isotope", level = "column"))
  expect_length(col$interpretation, 2)
  expect_equal(col$interpretation[[2]]$variable, "precipitationIsotope")
})

# A dataset-level field repeats across every row of its dataset in the QC sheet.
# When two rows disagree, taking the first is a coin toss: hydroclimate2k's
# sheet holds two rows for CO07CAFR whose pub1_citation differs by an en-dash
# mangled into three characters, and the mangled row won on ordering alone.
test_that("a dataset-level field that disagrees across rows is reported, not guessed", {
  cells <- cellset("T1", "pub1_citation", "Smith 2001, pages 190-201",
                   "T2", "pub1_citation", "Smith 2001, pages 190?201")
  r <- apply_to(cells, "pub1_citation")

  expect_true("dataset_field_disagrees" %in% r$issues$check)
  expect_equal(r$issues$severity[r$issues$check == "dataset_field_disagrees"][1], "warn")
  # The file is still staged, but neither value is applied: it keeps what it had.
  expect_null(r$L$pub[[1]]$citation)
})

test_that("a dataset-level field agreeing across rows is applied once", {
  r <- apply_to(cellset("T1", "geo_siteName", "Somewhere",
                        "T2", "geo_siteName", "Somewhere"), "geo_siteName")
  expect_equal(r$L$geo$siteName, "Somewhere")
  expect_equal(nrow(r$issues), 0)
})
