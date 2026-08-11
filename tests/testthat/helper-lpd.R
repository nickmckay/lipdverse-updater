# Build a minimal but structurally real .lpd for tests: a zip whose
# bag/data/metadata.jsonld matches the layout the database actually uses
# (columns in an unnamed list under a `columns` key).

make_column <- function(tsid, variableName = "temperature", col_extra = NULL) {
  c(list(TSid = tsid, variableName = variableName, units = "degC",
         values = list(1, 2, 3)),
    col_extra %||% list())
}

make_metadata <- function(dataSetName, datasetId, tsids,
                          version = "1.0.0", archiveType = "LakeSediment",
                          legacy_columns = FALSE, col_extra = NULL) {
  cols <- lapply(tsids, make_column, col_extra = col_extra)
  tbl <- if (legacy_columns) {
    # Older writers stored each column as a named entry of the table.
    stats::setNames(cols, paste0("var", seq_along(cols)))
  } else {
    list(tableName = "paleo1measurement1", filename = "x.csv", columns = cols)
  }
  list(
    dataSetName = dataSetName,
    datasetId   = datasetId,
    archiveType = archiveType,
    # Without lipdVersion, readLipd() prompts interactively for it, which
    # derails any test that reads a file back.
    lipdVersion = 1.3,
    # validLipd() requires geo, and the writer verifies with it, so fixtures
    # must be genuinely valid LiPD rather than merely parseable.
    geo         = list(latitude = 40.0, longitude = -105.0, elevation = 1500,
                       siteName = paste0("Site ", dataSetName)),
    # Real datasets carry a publication; without one, any test touching pubN_*
    # exercises the "create from nothing" path rather than the normal one.
    pub         = list(list(author = list(list(name = "Author, A.")),
                            title = paste("A study of", dataSetName))),
    changelog   = list(list(version = "1.0.0"), list(version = version)),
    paleoData   = list(list(measurementTable = list(tbl)))
  )
}

write_lpd <- function(dir, dataSetName, datasetId = NULL, tsids = c("T1", "T2"),
                      csv = NULL, csv_quote = FALSE, ...) {
  if (is.null(datasetId)) datasetId <- paste0("ID", dataSetName)
  stage <- file.path(tempfile("lpd"), "bag", "data")
  dir.create(stage, recursive = TRUE)
  meta <- make_metadata(dataSetName, datasetId, tsids, ...)
  jsonlite::write_json(meta, file.path(stage, "metadata.jsonld"), auto_unbox = TRUE, null = "null")
  if (!is.null(csv)) {
    utils::write.csv(csv, file.path(stage, paste0(dataSetName, ".paleo1measurement1.csv")),
                     row.names = FALSE, quote = csv_quote)
  }
  root <- dirname(dirname(stage))
  out <- normalizePath(file.path(dir, paste0(dataSetName, ".lpd")), mustWork = FALSE)
  withr::with_dir(root, utils::zip(out, "bag", flags = "-rq"))
  out
}

make_db <- function(specs) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  for (s in specs) do.call(write_lpd, c(list(dir = d), s))
  d
}

# An ensemble table with its own TSid, for testing the values/values_ensemble
# split. Reusing a measurement table would put one TSid in two tables, which the
# export's key check rejects -- correctly.
make_ensemble_table <- function(tsid = "E1", n = 3) {
  list(tableName = "chron0model0ensemble0", filename = "e.csv",
       columns = list(list(TSid = tsid, variableName = "ageEnsemble",
                           units = "yr BP", values = as.list(seq_len(n)))))
}
