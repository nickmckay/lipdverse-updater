reg <- function(...) {
  base <- tibble::tibble(
    qc_name = "archiveType", ts_name = "archiveType", family = "archiveType",
    role = "merged", ownership = "shared", nullable_by_curator = "FALSE",
    cardinality = "dataset", type = "character", vocab_key = "archiveType",
    canonical = NA_character_, csm_compilation = NA_character_,
    csm_field = NA_character_, csm_flat_key = NA_character_,
    deprecated = FALSE, n_compilations = 18L, n_filled = 100L
  )
  mods <- list(...)
  for (nm in names(mods)) base[[nm]] <- mods[[nm]]
  base
}

test_that("a well-formed registry validates", {
  expect_silent(validate_qc_fields(reg()))
})

test_that("missing columns are rejected", {
  expect_error(validate_qc_fields(reg()[, 1:3]), class = "lv_error_registry")
})

test_that("duplicate field names are rejected", {
  expect_error(validate_qc_fields(dplyr::bind_rows(reg(), reg())), class = "lv_error_registry")
})

test_that("unknown roles are rejected", {
  expect_error(validate_qc_fields(reg(role = "vibes")), class = "lv_error_registry")
})

# A merged field with no rule would silently fall through the merge, which is
# how a curated value gets lost.
test_that("a merged field without a usable ownership is rejected", {
  expect_error(validate_qc_fields(reg(ownership = "REVIEW")), class = "lv_error_registry")
  expect_error(validate_qc_fields(reg(ownership = NA_character_)), class = "lv_error_registry")
})

test_that("a merged field must say whether a blank cell clears it", {
  expect_error(validate_qc_fields(reg(nullable_by_curator = NA_character_)),
               class = "lv_error_registry")
})

# The rule that makes the NA-as-deletion loss impossible: a blank cell may only
# clear a value the curator owns. Anywhere else blank means "unchanged".
test_that("only curator-owned fields may be nullable", {
  expect_error(validate_qc_fields(reg(ownership = "machine", nullable_by_curator = "TRUE")),
               class = "lv_error_registry")
  expect_silent(validate_qc_fields(reg(ownership = "curator", nullable_by_curator = "TRUE")))
})

test_that("non-merged roles need no ownership", {
  expect_silent(validate_qc_fields(reg(role = "synonym", ownership = NA_character_,
                                       nullable_by_curator = NA_character_,
                                       canonical = "archiveType")))
  expect_silent(validate_qc_fields(reg(role = "unused", ownership = NA_character_,
                                       nullable_by_curator = NA_character_)))
})

test_that("csm fields must carry a well-formed flat key", {
  expect_error(validate_qc_fields(reg(role = "csm", ownership = NA_character_,
                                      nullable_by_curator = NA_character_,
                                      csm_flat_key = "not a csm key")),
               class = "lv_error_registry")
  expect_silent(validate_qc_fields(reg(role = "csm", ownership = NA_character_,
                                       nullable_by_curator = NA_character_,
                                       csm_compilation = "iso2k", csm_field = "certification",
                                       csm_flat_key = "iso2k_csm_certification")))
})

test_that("a synonym pointing nowhere warns but does not abort", {
  expect_warning(validate_qc_fields(reg(role = "synonym", ownership = NA_character_,
                                        nullable_by_curator = NA_character_,
                                        canonical = "nonexistent")),
                 "canonical is absent")
})

test_that("lv_field_rule looks up rules and flags unknown fields", {
  r <- lv_field_rule(c("archiveType", "notAField"), registry = reg())
  expect_equal(r$ownership[1], "shared")
  expect_false(r$nullable_by_curator[1])
  expect_true(r$known[1])
  expect_false(r$known[2])
})

test_that("the shipped registry parses and its shape is sane", {
  x <- lv_qc_fields(validate = FALSE)
  expect_gt(nrow(x), 200)
  expect_equal(anyDuplicated(x$qc_name), 0)
  expect_true(all(x$role %in% c("merged", "membership", "csm", "csm_pending", "key",
                                "synonym", "control", "unused", "delete")))
  # Membership must stay curator-owned: as a synonym of the machine-owned
  # inCompilationBeta_struct, the files would have overruled a curator adding a
  # timeseries to the compilation.
  m <- x[x$role == "membership", ]
  expect_equal(m$qc_name, "inThisCompilation")
  expect_equal(m$ownership, "curator")
  # Not nullable: a blank is "no opinion", not "remove this timeseries".
  expect_equal(m$nullable_by_curator, "FALSE")
  # Every field the merge engine will actually consult should be classified.
  expect_gt(sum(x$role == "merged"), 100)
})

# The registry is derived from review/, but the build script had stopped
# reproducing it: five coral keys claimed by both CoralHydro2k and GBRCD became
# two rows with the same qc_name, which validation rejects. The shipped file and
# the script drifted apart silently, so nobody could regenerate the registry.
test_that("a csm key may target more than one compilation", {
  base <- lv_qc_fields(validate = FALSE)[1, ]
  row <- base
  row$qc_name <- "paleoData_x"; row$role <- "csm"
  row$csm_compilation <- "CoralHydro2k;GBRCD"
  row$csm_field <- "x"
  row$csm_flat_key <- "CoralHydro2k_csm_x;GBRCD_csm_x"
  expect_silent(validate_qc_fields(row))

  # A malformed second target must still be caught, not hidden by the first.
  bad <- row; bad$csm_flat_key <- "CoralHydro2k_csm_x;nonsense"
  expect_error(validate_qc_fields(bad), class = "lv_error_registry")

  # Naming two compilations but one target is a build error.
  mismatch <- row; mismatch$csm_flat_key <- "CoralHydro2k_csm_x"
  expect_error(validate_qc_fields(mismatch), class = "lv_error_registry")
})

test_that("the shipped registry has one row per qc_name", {
  x <- lv_qc_fields(validate = FALSE)
  expect_equal(anyDuplicated(x$qc_name), 0)
})
