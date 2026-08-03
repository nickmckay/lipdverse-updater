tv <- function() {
  list(archiveType = tibble::tibble(
         lipdName = c("LakeSediment", "LakeSediment", "LakeSediment", "GlacierIce", "GlacierIce"),
         synonym  = c("LakeSediment", "Lagoon", "lake sediment", "GlacierIce", "glacier ice")),
       paleoData_proxyGeneral = tibble::tibble(
         lipdName = c("isotope", "chemical"), definition = c("a", "b")))
}

test_that("a value already canonical is reported as such and unchanged", {
  r <- vocab_standardize("LakeSediment", "archiveType", tv())
  expect_equal(r$value, "LakeSediment")
  expect_true(r$matched)
  expect_equal(r$rule, "canonical")
})

test_that("a listed synonym resolves to the canonical name", {
  r <- vocab_standardize("Lagoon", "archiveType", tv())
  expect_equal(r$value, "LakeSediment")
  expect_equal(r$rule, "synonym")
})

test_that("case and whitespace differences resolve, and say so", {
  expect_equal(vocab_standardize("lakesediment", "archiveType", tv())$rule, "case")
  expect_equal(vocab_standardize("lakesediment", "archiveType", tv())$value, "LakeSediment")
  expect_equal(vocab_standardize("  Lagoon  ", "archiveType", tv())$rule, "trim")
  expect_equal(vocab_standardize("  Lagoon  ", "archiveType", tv())$value, "LakeSediment")
})

# Guessing is what loses data. An unrecognised value is left exactly as it is.
test_that("an unmatched value is left alone and flagged", {
  r <- vocab_standardize("Sediment from a lake, probably", "archiveType", tv())
  expect_false(r$matched)
  expect_equal(r$rule, "none")
  expect_equal(r$value, "Sediment from a lake, probably")
})

test_that("blanks and NA pass through untouched", {
  r <- vocab_standardize(c(NA, "", "Lagoon"), "archiveType", tv())
  expect_equal(r$matched, c(FALSE, FALSE, TRUE))
  expect_true(is.na(r$value[1]))
  expect_equal(r$value[2], "")
})

test_that("a table with definitions rather than synonyms still matches canonically", {
  r <- vocab_standardize(c("isotope", "Isotope", "nonsense"), "paleoData_proxyGeneral", tv())
  expect_equal(r$rule, c("canonical", "case", "none"))
})

test_that("an unknown key is an error, not a silent pass", {
  expect_error(vocab_standardize("x", "notAKey", tv()), class = "lv_error_vocab")
})

test_that("vocab_check reports without changing anything", {
  iss <- vocab_check(c("Lagoon", "mystery", "mystery"), "archiveType", tv())
  expect_equal(nrow(iss), 1)
  expect_equal(iss$value, "mystery")
  expect_equal(iss$check, "unknown_vocabulary")
})

test_that("the shipped vocabulary is pinned and usable", {
  v <- lv_vocab()
  expect_true(all(c("archiveType", "paleoData_variableName", "paleoData_units",
                    "paleoData_proxy", "interpretation_variable",
                    "interpretation_seasonality") %in% names(v)))
  expect_false(is.na(lv_vocab_pin()))
  # Sanity against the real tables rather than a fixture.
  expect_equal(vocab_standardize("lake sediment", "archiveType", v)$value, "LakeSediment")
})

# standardizeLipdBatch reaches the values by round-tripping through
# as.lipdTsTibble()/as.lipd(), which pads every column to the dataset's maximum
# interpretation count with empty scope=NA entries. That is the mechanism behind
# the interpretation shells in the database, and it is live: it adds them to
# every file it touches, including year columns.
test_that("standardizing changes values without touching structure", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir())
  src <- withr::local_tempdir(); out <- withr::local_tempdir()
  write_lpd(src, "A.Author.2001", tsids = c("T1", "T2"))
  L <- lipdR::readLipd(fs::path(src, "A.Author.2001.lpd"))
  L$archiveType <- "lake sediment"
  tab <- L$paleoData[[1]]$measurementTable[[1]]
  cn <- names(Filter(function(c) is.list(c) && identical(as.character(c$TSid), "T1"), tab))[1]
  L$paleoData[[1]]$measurementTable[[1]][[cn]]$interpretation <-
    list(list(scope = "climate", variable = "temperature"))
  mid <- withr::local_tempdir()
  lipdR::writeLipd(L, path = mid, removeNamesFromLists = TRUE)

  shape <- function(p) {
    x <- lipdR::readLipd(p)
    n <- 0L; ni <- 0L
    for (pd in x$paleoData) for (tb in pd$measurementTable) {
      for (k in setdiff(names(tb), c("tableName", "filename", "missingValue"))) {
        cl <- tb[[k]]
        if (!is.list(cl) || is.null(cl$variableName)) next
        n <- n + 1L; ni <- ni + length(cl$interpretation)
      }
    }
    c(cols = n, interps = ni)
  }
  before <- shape(fs::path(mid, "A.Author.2001.lpd"))

  r <- lv_ingest_standardize(mid, out, progress = FALSE)
  after <- shape(fs::path(out, "A.Author.2001.lpd"))

  expect_equal(after, before)
  expect_equal(lipdR::readLipd(fs::path(out, "A.Author.2001.lpd"))$archiveType, "LakeSediment")
  expect_true("archiveType" %in% r$changes$field)
  expect_false(is.na(r$pin))
})

test_that("standardize refuses to write in place", {
  expect_error(lv_ingest_standardize("."), "never writes in place")
})
