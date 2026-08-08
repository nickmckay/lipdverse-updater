# Extracted from test-changelog.R:59

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
a <- lp()
b <- lp()
a$geo$latitude <- 40.12345
b$geo$latitude <- 40.1234
expect_equal(nrow(lv_changelog_diff(a, b)), 0)
