# A review file for the 109 live conflict-marked values.
#
# Each holds two or more genuinely different texts that the legacy merge forced
# into one field. Choosing between them is curation, not cleanup, so this lists
# them for a person rather than repairing anything.
suppressMessages(devtools::load_all("~/GitHub/lipdverse-updater", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

db <- lv_path("database")
lv <- readRDS("/Users/nicholas/lipdverse-staging/marker-live.rds")
todo <- lv |> filter(!collapsible | leaf == "proxyLumps")
cat("values to review:", nrow(todo), "in", dplyr::n_distinct(todo$file), "files\n")

clean_segments <- function(v) {
  x <- gsub("(((", " ", v, fixed = TRUE); x <- gsub(")))", " ", x, fixed = TRUE)
  p <- strsplit(x, "///", fixed = TRUE)[[1]]
  p <- trimws(gsub("[[:space:]]+", " ", p)); p[nzchar(p)]
}
distinct_texts <- function(seg) {
  out <- character()
  for (p in seg) {
    q <- regmatches(p, gregexpr('"[^"]*"', p))[[1]]
    rest <- trimws(gsub("[[:space:]]+", " ", trimws(gsub('"[^"]*"', " ", p))))
    out <- c(out, gsub('^"|"$', "", q), rest)
  }
  out <- trimws(out); unique(out[nzchar(out) & !tolower(out) %in% c("null", "na")])
}
cols_of <- function(tb) if (!is.null(tb[["columns"]])) tb[["columns"]] else
  tb[!names(tb) %in% c("filename", "tableName", "missingValue")]

# Re-read each file to recover the variants and the column they belong to: the
# path alone does not say which variable's notes these are, and that is the
# context the decision turns on.
rows <- list()
for (f in unique(todo$file)) {
  p <- fs::path(db, f)
  L <- tryCatch(suppressWarnings(lipdR::readLipd(p)), error = function(e) NULL)
  if (is.null(L)) next
  for (blk in c("paleoData", "chronData")) {
    for (pd in L[[blk]]) for (tb in pd$measurementTable) {
      if (!is.list(tb)) next
      for (cl in cols_of(tb)) {
        if (!is.list(cl)) next
        for (k in names(cl)) {
          v <- cl[[k]]
          if (!is.character(v) && !is.numeric(v)) next
          v1 <- as.character(v)[1]
          if (is.na(v1) || !grepl("(((", v1, fixed = TRUE)) next
          tx <- distinct_texts(clean_segments(v1))
          rows[[length(rows) + 1L]] <- tibble::tibble(
            dataSetName = sub("[.]lpd$", "", f),
            TSid = as.character(cl[["TSid"]])[1],
            variableName = as.character(cl[["variableName"]])[1],
            field = k, n_variants = length(tx), chars = nchar(v1),
            variants = paste(sprintf("[%d] %s", seq_along(tx), tx), collapse = "\n"),
            keep = NA_character_, note = NA_character_)
        }
      }
    }
  }
}
r <- bind_rows(rows)
cat("recovered:", nrow(r), "values\n")
out <- path.expand("~/lipdverse-staging/review/conflict-markers-live.csv")
fs::dir_create(fs::path_dir(out))
readr::write_csv(r, out, na = "")
cat("written:", out, "\n\n")
print(as.data.frame(count(r, field, n_variants, sort = TRUE) |> head(8)), right = FALSE)
cat("\ndatasets:", dplyr::n_distinct(r$dataSetName), "\n")
