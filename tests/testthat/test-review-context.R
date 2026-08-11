test_that("examples show non-missing values and how full the column is", {
  # Taking the first few values would show NA, NA, NA for a sparse column and
  # read as empty. Two real columns in the h2k round-2 batch look exactly like
  # that: "d13C code" is 114 populated values out of 580, and they are isotope
  # measurements, not the empty flag column the leading NAs suggested.
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  # Same length as its sibling: a ragged table makes writeLipd empty every
  # column in it, which is its own trap.
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$values <- list(NA, -5.34, -6.62)
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$variableName <- "d13C code"
  fs::file_delete(fs::path(d, "A.Author.2001.lpd"))
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))

  x <- tibble::tibble(field = "paleoData_variableName", value = "d13C code",
                      dataSetName = "A.Author.2001", TSid = "T1")
  ctx <- lv_review_context(x, d)
  ex <- unlist(ctx$examples)
  expect_true(any(grepl("-5.34", ex)))
  expect_false(any(grepl("NA, NA", ex)))
  expect_true(any(grepl("2 of 3 values", ex)))
})

test_that("an empty column says so rather than looking like a lookup failure", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = "T1")
  L <- suppressWarnings(lipdR::readLipd(fs::path(d, "A.Author.2001.lpd")))
  nm <- names(lv_cols_of(L$paleoData[[1]]$measurementTable[[1]]))[1]
  L$paleoData[[1]]$measurementTable[[1]][[nm]]$values <- list(NA, NA, NA)
  fs::file_delete(fs::path(d, "A.Author.2001.lpd"))
  suppressWarnings(lipdR::writeLipd(L, path = d, removeNamesFromLists = TRUE))

  x <- tibble::tibble(field = "paleoData_variableName", value = "temperature",
                      dataSetName = "A.Author.2001", TSid = "T1")
  ex <- unlist(lv_review_context(x, d)$examples)
  expect_true(any(grepl("empty", ex)))
  # Not a fixed total: lipdR does not preserve the length of an all-NA column
  # across a write, so the count is asserted as "none filled", not "none of 3".
  expect_true(any(grepl("0 of [0-9]+ values", ex)))
})

test_that("siblings list the other variables in the table", {
  d <- withr::local_tempdir()
  write_lpd(d, "A.Author.2001", tsids = c("T1", "T2"))
  x <- tibble::tibble(field = "paleoData_variableName", value = "temperature",
                      dataSetName = "A.Author.2001", TSid = "T1")
  sib <- unlist(lv_review_context(x, d)$siblings)
  expect_true(length(sib) >= 1)
  expect_true(all(nzchar(sib)))
})

test_that("no directory means no context rather than an error", {
  # The review must still generate when the staged files are gone.
  x <- tibble::tibble(field = "paleoData_variableName", value = "x",
                      dataSetName = "A", TSid = "T1")
  expect_length(lv_review_context(x, NULL)$key, 0)
  expect_length(lv_review_context(x, "/nonexistent/path")$key, 0)
})

test_that("fields are ordered for working, not alphabetically", {
  # variableName decisions constrain units and proxy, and a decompose on
  # variableName creates the seasonality an interpretation row refers to.
  expect_lt(lv_field_rank("paleoData_variableName"), lv_field_rank("paleoData_units"))
  expect_lt(lv_field_rank("paleoData_units"), lv_field_rank("paleoData_proxy"))
  expect_lt(lv_field_rank("paleoData_proxy"), lv_field_rank("interpretation_variable"))
  # An unknown field sorts last rather than leading silently.
  expect_gt(lv_field_rank("something_new"), lv_field_rank("archiveType"))
})

test_that("decisions carry into a regenerated review", {
  # Regenerating is how the review gains example values and a better ordering,
  # and it must not cost the decisions already made.
  mk <- function(vals, dec = NA_character_) {
    r <- tibble::tibble(field = "paleoData_variableName", value = vals, n = 1L,
                        example = NA_character_,
                        datasets = vector("list", length(vals)),
                        candidates = vector("list", length(vals)),
                        source_pdfs = vector("list", length(vals)),
                        examples = vector("list", length(vals)),
                        siblings = vector("list", length(vals)),
                        past_candidates = rep(list(tibble::tibble()), length(vals)))
    for (nm in LV_REVIEW_PROPOSED) r[[paste0("proposed_", nm)]] <- NA_character_
    for (nm in LV_REVIEW_DECIDED) r[[nm]] <- NA_character_
    r$decision <- dec
    r
  }
  old <- mk(c("a", "b", "gone"), c("synonym", NA, "leave"))
  old$map_to <- c("A", NA, NA)
  new <- mk(c("a", "b", "c"))

  m <- lv_review_carry(old, new)
  expect_equal(m$decision, c("synonym", NA, NA))
  expect_equal(m$map_to[1], "A")
  # A value that vanished from the new batch is reported, not silently lost.
  expect_equal(attr(m, "carried")$lost, "gone")
  expect_equal(attr(m, "carried")$moved, 1L)
})

test_that("carrying never overwrites a decision already made in the target", {
  # The target is the file being worked in now; the source is history.
  mk1 <- function(dec, map) {
    r <- tibble::tibble(field = "paleoData_units", value = "z-scores", n = 1L,
                        example = NA_character_, datasets = list(character()),
                        candidates = list(character()), source_pdfs = list(character()),
                        examples = list(character()), siblings = list(character()),
                        past_candidates = list(tibble::tibble()))
    for (nm in LV_REVIEW_PROPOSED) r[[paste0("proposed_", nm)]] <- NA_character_
    for (nm in LV_REVIEW_DECIDED) r[[nm]] <- NA_character_
    r$decision <- dec; r$map_to <- map; r
  }
  m <- lv_review_carry(mk1("synonym", "old"), mk1("synonym", "kept"))
  expect_equal(m$map_to, "kept")
  expect_equal(attr(m, "carried")$moved, 0L)
})
