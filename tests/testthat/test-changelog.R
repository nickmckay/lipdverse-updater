lp <- function(..., tsids = c("T1", "T2")) {
  cols <- list(tableName = "t", filename = "t.csv", missingValue = "NaN")
  for (i in seq_along(tsids)) {
    cols[[paste0("v", i)]] <- list(variableName = paste0("var", i), TSid = tsids[i],
                                   number = i, units = "permil", values = c(1, 2, 3))
  }
  L <- list(dataSetName = "A.Author.2001", archiveType = "LakeSediment",
            geo = list(siteName = "Somewhere", latitude = 40),
            pub = list(list(doi = "10.1/x")),
            paleoData = list(list(measurementTable = list(cols))))
  mods <- list(...)
  for (nm in names(mods)) L[[nm]] <- mods[[nm]]
  L
}

test_that("an unchanged dataset produces no changes", {
  expect_equal(nrow(lv_changelog_diff(lp(), lp())), 0)
})

test_that("changes, additions and removals are each identified", {
  a <- lp(); b <- lp()
  b$archiveType <- "GlacierIce"                                   # changed
  b$paleoData[[1]]$measurementTable[[1]]$v1$units <- NULL          # removed
  b$paleoData[[1]]$measurementTable[[1]]$v1$proxy <- "d18O"        # added
  d <- lv_changelog_diff(a, b)

  expect_setequal(d$kind, c("changed", "removed", "added"))
  expect_equal(d$from[d$field == "archiveType"], "LakeSediment")
  expect_equal(d$to[d$field == "archiveType"], "GlacierIce")
  expect_true(is.na(d$to[d$field == "units"]))
  expect_true(is.na(d$from[d$field == "proxy"]))
})

test_that("changes are attributed to the timeseries they belong to", {
  a <- lp(); b <- lp()
  b$paleoData[[1]]$measurementTable[[1]]$v2$units <- "degC"
  d <- lv_changelog_diff(a, b)
  expect_equal(d$tsid, "T2")
  expect_equal(d$variableName, "var2")
})

# Keyed by TSid rather than position: a column that moves within a table is the
# same timeseries, and reporting it as a delete plus an add would be wrong and
# would bury the real changes.
test_that("reordering columns is not a change", {
  a <- lp()
  b <- lp()
  t <- b$paleoData[[1]]$measurementTable[[1]]
  b$paleoData[[1]]$measurementTable[[1]] <- t[c("tableName", "filename", "missingValue", "v2", "v1")]
  expect_equal(nrow(lv_changelog_diff(a, b)), 0)
})

# The same comparison the merge uses, so a value written back at a different
# precision does not appear as a change on every run.
test_that("a value differing only in precision is not a change", {
  a <- lp(); b <- lp()
  a$geo$latitude <- 40.12345
  b$geo$latitude <- 40.1234
  expect_equal(nrow(lv_changelog_diff(a, b)), 0)
})

test_that("volatile fields are ignored", {
  a <- lp(); b <- lp()
  b$lipdverseLink <- "http://example.org/v2"
  b$tagMD5 <- "deadbeef"
  expect_equal(nrow(lv_changelog_diff(a, b)), 0)
  # Unless you ask for them.
  expect_gt(nrow(lv_changelog_diff(a, b, ignore = character())), 0)
})

test_that("interpretations are keyed within a scope, not by position", {
  a <- lp(); b <- lp()
  a$paleoData[[1]]$measurementTable[[1]]$v1$interpretation <-
    list(list(scope = "climate", variable = "temperature"),
         list(scope = "isotope", variable = "d18O"))
  b$paleoData[[1]]$measurementTable[[1]]$v1$interpretation <-
    list(list(scope = "climate", variable = "precipitation"),
         list(scope = "isotope", variable = "d18O"))
  d <- lv_changelog_diff(a, b)
  expect_equal(nrow(d), 1)
  expect_equal(d$field, "climateInterpretation1_variable")
  expect_equal(d$category, "Paleo Interpretation metadata")
})

test_that("compilation membership changes are recorded", {
  a <- lp(); b <- lp()
  b$paleoData[[1]]$measurementTable[[1]]$v1$inCompilation <-
    list(list(compilationName = "iso2k", compilationVersion = "1_0_0"))
  d <- lv_changelog_diff(a, b)
  expect_equal(d$kind, "added")
  expect_equal(d$to, "iso2k")
})

test_that("categories match the vocabulary the corpus already uses", {
  a <- lp(); b <- lp()
  b$archiveType <- "GlacierIce"
  b$geo$siteName <- "Elsewhere"
  b$pub[[1]]$doi <- "10.2/y"
  b$paleoData[[1]]$measurementTable[[1]]$v1$units <- "degC"
  d <- lv_changelog_diff(a, b)
  expect_setequal(d$category, c("Base metadata", "Geographic metadata",
                                "Publication metadata", "Paleo Column metadata"))
})

# The two fields createChangelog() never recorded. 56% of datasets belong to two
# or more compilations and they share the fields stored in the file, so without
# these an entry says what changed but not which run did it.
test_that("an entry carries the compilation and run that made the change", {
  a <- lp(); b <- lp(); b$archiveType <- "GlacierIce"
  e <- lv_changelog_entry(lv_changelog_diff(a, b), version = "1.0.4",
                          last_version = "1.0.3", compilation = "iso2k",
                          run_id = "20260804T0100-abc", curator = "nicholas")
  expect_equal(e$compilation, "iso2k")
  expect_equal(e$run_id, "20260804T0100-abc")
  expect_equal(e$version, "1.0.4")
  expect_equal(e$lastVersion, "1.0.3")
  expect_match(e$timestamp, "UTC$")
  expect_equal(names(e$changes), "Base metadata")
  expect_match(e$changes[["Base metadata"]][[1]][[1]], "has been replaced by 'GlacierIce'")
})

test_that("rendering is deterministic for the same change set", {
  a <- lp(); b <- lp()
  b$archiveType <- "GlacierIce"; b$geo$siteName <- "Elsewhere"
  b$paleoData[[1]]$measurementTable[[1]]$v1$units <- "degC"
  d <- lv_changelog_diff(a, b)
  ts <- as.POSIXct("2026-08-04 00:00:00", tz = "UTC")
  e1 <- lv_changelog_entry(d, "1.0.1", timestamp = ts)
  e2 <- lv_changelog_entry(d[sample(nrow(d)), ], "1.0.1", timestamp = ts)
  expect_identical(e1, e2)
})

test_that("appending leaves earlier entries alone", {
  L <- lp()
  L$changelog <- list(list(version = "1.0.0", curator = "someone"))
  L2 <- lv_changelog_append(L, lv_changelog_entry(lv_changelog_diff(lp(), lp()), "1.0.1"))
  expect_length(L2$changelog, 2)
  expect_equal(L2$changelog[[1]]$version, "1.0.0")
  expect_equal(L2$changelog[[2]]$version, "1.0.1")
})

test_that("dataset versions read and tick at the patch level", {
  L <- lp()
  expect_true(is.na(lv_changelog_last_version(L)))
  L$changelog <- list(list(version = "1.0.9"), list(version = "1.0.12"))
  expect_equal(lv_changelog_last_version(L), "1.0.12")
  expect_equal(lv_changelog_next_version("1.0.12"), "1.0.13")
  expect_equal(lv_changelog_next_version(NA_character_), "1.0.0")
})

# Dataset versions are dotted; compilation versions are underscored. They count
# different things and mixing them would be a silent mess.
test_that("dataset and compilation versions are not interchangeable", {
  expect_equal(lv_changelog_next_version("1.0.12"), "1.0.13")
  expect_equal(lv_tick_version("0_4_0", "a", "a")$version, "0_4_1")
})
