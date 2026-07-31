# Build a minimal but structurally real .lpd for tests: a zip whose
# bag/data/metadata.jsonld matches the layout the database actually uses
# (columns in an unnamed list under a `columns` key).

make_column <- function(tsid, variableName = "temperature") {
  list(TSid = tsid, variableName = variableName, units = "degC",
       values = list(1, 2, 3))
}

make_metadata <- function(dataSetName, datasetId, tsids,
                          version = "1.0.0", archiveType = "LakeSediment",
                          legacy_columns = FALSE) {
  cols <- lapply(tsids, make_column)
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
    # validLipd() requires geo, and the writer verifies with it, so fixtures
    # must be genuinely valid LiPD rather than merely parseable.
    geo         = list(latitude = 40.0, longitude = -105.0, elevation = 1500,
                       siteName = paste0("Site ", dataSetName)),
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
