# Compilation-specific metadata, from file to merge and back.
#
# The bug these guard against is not a wrong value, it is a missing one: csm was
# declared in the registry, put on new sheets, and then read by nothing and
# written by nothing, so a curator's certification reached no file at all.

csm_reg <- function() {
  tibble::tibble(
    qc_name = c("paleoData_hydroclimate2kCertification", "paleoData_iso2kCertification",
                "paleoData_coralExtensionRate", "paleoData_units"),
    role = c("csm", "csm", "csm", "merged"),
    ownership = c("curator", "curator", "curator", "curator"),
    nullable_by_curator = c("FALSE", "FALSE", "FALSE", "TRUE"),
    ts_name = NA_character_, family = qc_name,
    cardinality = "timeseries", type = "character",
    vocab_key = NA_character_, canonical = NA_character_,
    csm_compilation = c("hydroclimate2k", "iso2k", "CoralHydro2k;GBRCD", NA),
    csm_field = c("QCCertification", "QCCertification", "coralExtensionRate", NA),
    csm_flat_key = c("hydroclimate2k_csm_QCCertification", "iso2k_csm_QCCertification",
                     "CoralHydro2k_csm_coralExtensionRate;GBRCD_csm_coralExtensionRate", NA),
    deprecated = FALSE, n_compilations = 1L, n_filled = 1L)
}

in_comp <- function(...) list(inCompilation = list(...))

comp_entry <- function(name, csm = NULL) {
  e <- list(compilationName = name, compilationVersion = list("1_0_0"))
  if (!is.null(csm)) e$csm <- csm
  e
}

csm_db <- function(env = parent.frame(), ...) {
  d <- withr::local_tempdir(.local_envir = env)
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"), ...)
  d
}

test_that("a field claimed by two compilations belongs to each of them", {
  reg <- csm_reg()
  expect_equal(lv_csm_fields("GBRCD", reg)$qc_name, "paleoData_coralExtensionRate")
  expect_equal(lv_csm_fields("CoralHydro2k", reg)$csm_field, "coralExtensionRate")
  # A compilation with no csm fields is not an error, it is a compilation that
  # asserts nothing of its own.
  expect_equal(nrow(lv_csm_fields("Temp12k", reg)), 0)
})

test_that("another compilation's csm column never enters the merge", {
  reg <- csm_reg()
  sheet <- tibble::tibble(
    tsid = "T1",
    field = c("paleoData_hydroclimate2kCertification", "paleoData_iso2kCertification",
              "paleoData_units"),
    value = c("NPM", "AAA", "permil"), present = TRUE, dataset_id = "ID1")

  s <- lv_csm_scope(sheet, "hydroclimate2k", reg)
  expect_equal(s$foreign$field, "paleoData_iso2kCertification")
  # Merged fields are not csm and are none of this function's business.
  expect_equal(sort(s$cells$field),
               c("paleoData_hydroclimate2kCertification", "paleoData_units"))

  # And the point of doing it here: the foreign cell reaches neither the plan
  # nor the state, so it cannot reach the store or the sheet push either.
  plan <- qc_merge(base = s$cells[0, ], sheet = s$cells, frame = s$cells[0, ],
                   registry = reg)
  expect_false("paleoData_iso2kCertification" %in% qc_plan_state(plan)$field)
})

test_that("scoping an empty or csm-free sheet is a no-op", {
  reg <- csm_reg()
  empty <- qc_cells_empty()
  expect_equal(nrow(lv_csm_scope(empty, "hydroclimate2k", reg)$cells), 0)

  only_merged <- tibble::tibble(tsid = "T1", field = "paleoData_units", value = "permil",
                                present = TRUE, dataset_id = "ID1")
  s <- lv_csm_scope(only_merged, "Temp12k", reg)
  expect_equal(nrow(s$cells), 1)
  expect_equal(nrow(s$foreign), 0)
})

test_that("csm is read out of the compilation's own inCompilation entry", {
  d <- csm_db(col_extra = in_comp(
    comp_entry("iso2k", list(QCCertification = "AAA")),
    comp_entry("hydroclimate2k", list(QCCertification = "NPM"))))

  f <- lv_csm_frame(d, "hydroclimate2k", registry = csm_reg(), progress = FALSE)
  expect_equal(sort(unique(f$tsid)), c("T1", "T2"))
  expect_equal(unique(f$field), "paleoData_hydroclimate2kCertification")
  expect_equal(unique(f$value), "NPM")

  # The other compilation's entry is a different compilation's business, and
  # reading it here is how one curator's initials become another's.
  g <- lv_csm_frame(d, "iso2k", registry = csm_reg(), progress = FALSE)
  expect_equal(unique(g$value), "AAA")
  expect_equal(unique(g$field), "paleoData_iso2kCertification")
})

test_that("a compilation with no entry on the column reads nothing", {
  d <- csm_db(col_extra = in_comp(comp_entry("iso2k", list(QCCertification = "AAA"))))
  expect_equal(nrow(lv_csm_frame(d, "hydroclimate2k", registry = csm_reg(), progress = FALSE)), 0)
})

test_that("membership without csm is not a cell", {
  d <- csm_db(col_extra = in_comp(comp_entry("hydroclimate2k")))
  expect_equal(nrow(lv_csm_frame(d, "hydroclimate2k", registry = csm_reg(), progress = FALSE)), 0)
})

test_that("the merge keeps csm cells instead of dropping them", {
  reg <- csm_reg()
  cell <- function(v) tibble::tibble(
    tsid = "T1", field = "paleoData_hydroclimate2kCertification", value = v,
    present = TRUE, dataset_id = "ID1")

  # A curator typing a certification is a change, resolved to the sheet.
  plan <- qc_merge(base = cell("NPM"), sheet = cell("DE"), frame = cell("NPM"),
                   registry = reg)
  expect_equal(nrow(plan$cells), 1)
  expect_equal(plan$cells$resolution, "sheet")
  expect_equal(plan$cells$value, "DE")

  # And a file that already agrees with the store is not one.
  quiet <- qc_merge(base = cell("NPM"), sheet = cell("NPM"), frame = cell("NPM"),
                    registry = reg)
  expect_equal(quiet$summary$n_changed, 0)
})

test_that("csm diverging between sheet and file goes to the sheet, not to a conflict", {
  reg <- csm_reg()
  cell <- function(v) tibble::tibble(
    tsid = "T1", field = "paleoData_hydroclimate2kCertification", value = v,
    present = TRUE, dataset_id = "ID1")
  plan <- qc_merge(base = cell("A"), sheet = cell("B"), frame = cell("C"), registry = reg)
  expect_equal(plan$cells$resolution, "sheet")
  expect_equal(nrow(plan$conflicts), 0)
})

apply_csm <- function(cells, compilation = "hydroclimate2k", env = parent.frame(), ...) {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir(.local_envir = env),
                      .local_envir = env)
  src <- csm_db(env = env, ...)
  out <- withr::local_tempdir(.local_envir = env)
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  res <- lv_apply_csm(cells, idx, compilation, dir = src, out = out,
                      registry = csm_reg(), progress = FALSE)
  f <- fs::path(out, "A.Author.2001.lpd")
  res$L <- if (fs::file_exists(f)) lipdR::readLipd(f) else NULL
  res
}

column_of <- function(L, tsid) {
  tb <- L$paleoData[[1]]$measurementTable[[1]]
  cols <- if (!is.null(tb$columns)) tb$columns else tb
  hit <- Filter(function(c) is.list(c) && identical(as.character(c$TSid)[1], tsid), cols)
  hit[[1]]
}

entry_of <- function(col, name) {
  i <- which(vapply(col$inCompilation,
                    function(e) identical(as.character(e$compilationName)[1], name),
                    logical(1)))
  col$inCompilation[[i[1]]]
}

csm_cell <- function(tsid, field, value) {
  tibble::tibble(tsid = tsid, field = field, value = value,
                 present = TRUE, dataset_id = "IDA.Author.2001")
}

test_that("a csm value is written into its own compilation's entry", {
  r <- apply_csm(csm_cell("T1", "paleoData_hydroclimate2kCertification", "NPM"),
                 col_extra = in_comp(comp_entry("iso2k", list(QCCertification = "AAA")),
                                     comp_entry("hydroclimate2k")))
  expect_equal(r$n, 1)
  col <- column_of(r$L, "T1")
  expect_equal(as.character(entry_of(col, "hydroclimate2k")$csm$QCCertification), "NPM")
  # The neighbouring compilation is untouched. Writing a partial view back over
  # the whole structure is the failure that lost 24 createdBy values.
  expect_equal(as.character(entry_of(col, "iso2k")$csm$QCCertification), "AAA")
})

test_that("a column with no membership gets no csm, and says so", {
  r <- apply_csm(csm_cell("T1", "paleoData_hydroclimate2kCertification", "NPM"),
                 col_extra = in_comp(comp_entry("iso2k")))
  expect_equal(r$n, 0)
  expect_equal(r$issues$check, "csm_without_membership")
  expect_equal(lv_n_issues(r$issues, "error"), 0)
  # Nothing was written at all, so no file was staged.
  expect_null(r$L)
})

test_that("another compilation's csm field is reported, never written", {
  r <- apply_csm(csm_cell("T1", "paleoData_iso2kCertification", "XYZ"),
                 compilation = "hydroclimate2k",
                 col_extra = in_comp(comp_entry("iso2k"), comp_entry("hydroclimate2k")))
  expect_equal(r$n, 0)
  expect_equal(r$issues$check, "csm_other_compilation")
  expect_null(r$L)
})

test_that("a cleared csm value removes the key rather than emptying it", {
  r <- apply_csm(csm_cell("T1", "paleoData_hydroclimate2kCertification", NA_character_),
                 col_extra = in_comp(comp_entry("hydroclimate2k", list(QCCertification = "NPM"))))
  col <- column_of(r$L, "T1")
  expect_null(entry_of(col, "hydroclimate2k")$csm$QCCertification)
})

test_that("what apply writes is what the frame reads back", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- csm_db(col_extra = in_comp(comp_entry("hydroclimate2k")))
  out <- withr::local_tempdir()
  idx <- lv_db_index(lv_scan(src, cache = FALSE), cache = FALSE)
  lv_apply_csm(csm_cell("T1", "paleoData_hydroclimate2kCertification", "NPM"),
               idx, "hydroclimate2k", dir = src, out = out, registry = csm_reg(),
               progress = FALSE)

  back <- lv_csm_frame(out, "hydroclimate2k", registry = csm_reg(), progress = FALSE)
  expect_equal(back$tsid, "T1")
  expect_equal(back$field, "paleoData_hydroclimate2kCertification")
  expect_equal(back$value, "NPM")
})

test_that("lv_csm_frame reads an accented dataset name too", {
  # Same NFD/NFC trap as qc_frame(); the dataset filter is the same shape and
  # was copied with the same defect.
  nfd <- stringi::stri_trans_nfd("CentralEurope.Büntgen.2011")
  d <- withr::local_tempdir()
  write_lpd(d, nfd, tsids = "T1",
            col_extra = in_comp(comp_entry("hydroclimate2k", list(QCCertification = "NPM"))))
  f <- lv_csm_frame(d, "hydroclimate2k", registry = csm_reg(),
                    datasets = "CentralEurope.Büntgen.2011", progress = FALSE)
  expect_equal(f$value, "NPM")
})
