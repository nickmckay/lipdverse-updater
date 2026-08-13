# The bibliographic reference database: resolve a DOI once, keep the answer.

ref_rows <- function() {
  tibble::tibble(
    citekey = c("abram2008recent", "smith1975report"),
    source = c("crossref", "curated"),
    added_at = "2026-08-13T00:00:00Z",
    bibtype = c("Article", "TechReport"),
    author = c("Abram, N. J. and Gagan, M. K.", "Smith, A."),
    title = c("Recent intensification", "A report with no DOI"),
    year = c("2008", "1975"),
    journal = c("Nature Geoscience", NA_character_),
    doi = c("10.1038/ngeo357", NA_character_))
}

local_ref_store <- function(envir = parent.frame()) {
  qc_store(withr::local_tempdir(.local_envir = envir))
}

test_that("an empty store reads as an empty database", {
  s <- local_ref_store()
  r <- lv_references(s)
  expect_equal(nrow(r), 0)
  expect_true(all(c("citekey", "source", "doi", "title") %in% names(r)))
})

test_that("references are added and then kept", {
  s <- local_ref_store()
  a <- lv_references_add(ref_rows(), s, dry_run = FALSE)
  expect_equal(a$added, 2)
  expect_equal(nrow(lv_references(s)), 2)

  # A citekey already present is not overwritten: the stored copy may carry a
  # correction, and an import must not undo by machine what somebody fixed by
  # hand.
  edited <- ref_rows()
  edited$title[1] <- "A title somebody corrected"
  b <- lv_references_add(edited, s, dry_run = FALSE)
  expect_equal(b$added, 0)
  expect_equal(lv_references(s)$title[lv_references(s)$citekey == "abram2008recent"],
               "Recent intensification")
})

test_that("a dry run adds nothing", {
  s <- local_ref_store()
  r <- lv_references_add(ref_rows(), s, dry_run = TRUE)
  expect_equal(r$added, 2)
  expect_equal(nrow(lv_references(s)), 0)
})

test_that("a publication is resolved by DOI, and the file wins where it has a value", {
  pubs <- tibble::tibble(
    datasetId = c("D1", "D2"), pubIndex = 1:2, pubKind = NA_character_,
    authors = list(character(), "File, A."),
    year = c(NA_integer_, 2008L),
    title = c(NA_character_, "The title the file already has"),
    journal = NA_character_,
    doi = c("10.1038/ngeo357", "10.1038/ngeo357"),
    citeKey = NA_character_)
  r <- lv_resolve_references(pubs, ref_rows())

  # The gap is filled from the store.
  expect_equal(r$title[1], "Recent intensification")
  expect_equal(r$journal[1], "Nature Geoscience")
  expect_equal(r$year[1], 2008L)
  expect_equal(r$authors[[1]], c("Abram, N. J.", "Gagan, M. K."))
  # What the file holds is left alone, because the file is what a curator edits.
  expect_equal(r$title[2], "The title the file already has")
  expect_equal(r$authors[[2]], "File, A.")
  # And both carry the citekey and the tier that answered.
  expect_equal(r$citekey, rep("abram2008recent", 2))
  expect_equal(r$ref_source, rep("crossref", 2))
})

test_that("a DOI written as a URL still matches", {
  pubs <- tibble::tibble(
    datasetId = "D1", pubIndex = 1L, pubKind = NA_character_,
    authors = list(character()), year = NA_integer_, title = NA_character_,
    journal = NA_character_, doi = "https://doi.org/10.1038/NGEO357",
    citeKey = NA_character_)
  expect_equal(lv_resolve_references(pubs, ref_rows())$citekey, "abram2008recent")
})

test_that("an unresolvable publication is left as it was", {
  pubs <- tibble::tibble(
    datasetId = "D1", pubIndex = 1L, pubKind = NA_character_,
    authors = list("Nobody, A."), year = 2001L, title = "Untraceable",
    journal = NA_character_, doi = "10.9999/nothing", citeKey = NA_character_)
  r <- lv_resolve_references(pubs, ref_rows())
  expect_true(is.na(r$citekey))
  expect_true(is.na(r$ref_source))
  expect_equal(r$title, "Untraceable")
})

test_that("the curated overrides file parses", {
  p <- withr::local_tempfile(fileext = ".bib")
  writeLines(c(
    "@Book{knight1975pearson,",
    "  author = {Knight, R.},",
    "  title = {A late Holocene pollen record from {Pearson's} Pond},",
    "  year = {1975},",
    "  publisher = {USGS},",
    "}",
    "",
    "@Misc{other2001thing,",
    "  author = {Other, B.},",
    "  title = {Something else},",
    "  year = {2001},",
    "}"), p)
  b <- lv_references_read_bib(p)
  expect_equal(nrow(b), 2)
  expect_equal(b$citekey, c("knight1975pearson", "other2001thing"))
  # A braced value inside the title is not truncated at the first closing brace.
  expect_match(b$title[1], "Pearson's. Pond")
  expect_equal(b$bibtype, c("Book", "Misc"))
})

test_that("the bibliography prefers the store and says so when it cannot", {
  pubs <- tibble::tibble(
    datasetId = c("D1", "D2"), pubIndex = 1:2, pubKind = NA_character_,
    authors = list(character(), character()),
    year = c(NA_integer_, NA_integer_),
    title = c(NA_character_, NA_character_), journal = NA_character_,
    doi = c("10.1038/ngeo357", "10.9999/unknown"), citeKey = NA_character_)
  p <- withr::local_tempfile(fileext = ".bib")
  lv_export_bib(pubs, p, references = ref_rows(), progress = FALSE)
  txt <- paste(readLines(p), collapse = "\n")

  # Resolved: full record under the legacy citekey.
  expect_match(txt, "@Article\\{abram2008recent")
  expect_match(txt, "Recent intensification")
  # Unresolved: a visible gap rather than a silent omission, as lipdverseR did.
  expect_match(txt, "Missing Title")
  expect_match(txt, "Missing Author")
})
