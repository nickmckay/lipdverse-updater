# Classify every legacy conflict marker in the database.
#
# lipdverseR's merge wrote unresolved conflicts into the value as
# "((( a ))) b /// c" and then patched them out by string substitution. Where it
# missed, the marker stayed, and the next run merged the marked value again and
# added a layer.
#
# The question that decides what can be repaired automatically: once the markers
# are stripped, does the value hold ONE piece of text repeated, or several
# different ones? One is a safe collapse. Several is a real disagreement between
# two curators, and choosing for them is the loss this project exists to prevent.
suppressPackageStartupMessages(library(dplyr))
db <- path.expand("~/Dropbox/lipdverse/database")
files <- unique(readRDS("/Users/nicholas/lipdverse-staging/conflict-markers.rds")$file)
N <- as.integer(Sys.getenv("LV_N", "0")); if (N > 0) files <- head(files, N)
cat("files with markers:", length(files), "\n")

clean_segments <- function(v) {
  x <- gsub("(((", " ", v, fixed = TRUE)
  x <- gsub(")))", " ", x, fixed = TRUE)
  parts <- strsplit(x, "///", fixed = TRUE)[[1]]
  parts <- trimws(gsub("[[:space:]]+", " ", parts))
  parts[nzchar(parts)]
}
distinct_texts <- function(segments) {
  out <- character()
  for (p in segments) {
    q <- regmatches(p, gregexpr('"[^"]*"', p))[[1]]
    rest <- trimws(gsub('"[^"]*"', " ", p))
    rest <- trimws(gsub("[[:space:]]+", " ", rest))
    out <- c(out, gsub('^"|"$', "", q), rest)
  }
  out <- trimws(out)
  out[nzchar(out) & !tolower(out) %in% c("null", "na")]
}

future::plan(future::multisession, workers = min(12L, future::availableCores() - 2L))
one <- function(f) {
  p <- file.path(db, f)
  nms <- tryCatch(unzip(p, list = TRUE)$Name, error = function(e) NULL)
  j <- grep("[.]jsonld$", nms, value = TRUE); if (!length(j)) return(NULL)
  con <- unz(p, j[1])
  m <- tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                                   simplifyVector = FALSE), error = function(e) NULL)
  close(con); if (is.null(m)) return(NULL)

  out <- list()
  walk <- function(x, path) {
    if (!is.list(x)) {
      v <- as.character(unlist(x))
      if (length(v) == 1 && !is.na(v) && grepl("(((", v, fixed = TRUE)) {
        texts <- unique(distinct_texts(clean_segments(v)))
        out[[length(out) + 1L]] <<- tibble::tibble(
          file = f, path = path, chars = nchar(v),
          segments = length(clean_segments(v)),
          distinct = length(texts),
          collapsible = length(texts) <= 1L,
          texts = paste(utils::head(texts, 4), collapse = " || "))
      }
      return(invisible())
    }
    ns <- names(x); if (is.null(ns)) ns <- rep("", length(x))
    for (i in seq_along(x)) {
      k <- if (nzchar(ns[i])) ns[i] else paste0("[", i, "]")
      walk(x[[i]], paste0(path, "$", k))
    }
  }
  walk(m, "")
  if (length(out)) bind_rows(out) else NULL
}
r <- bind_rows(furrr::future_map(files, one, .options = furrr::furrr_options(
  seed = TRUE, globals = c("db", "clean_segments", "distinct_texts"),
  packages = c("jsonlite", "tibble", "dplyr"))))
saveRDS(r, "/Users/nicholas/lipdverse-staging/marker-classification.rds")

cat("marked values:", nrow(r), "in", dplyr::n_distinct(r$file), "files\n\n")
cat("=== can it be collapsed automatically? ===\n")
print(as.data.frame(count(r, collapsible)), right = FALSE)
cat("\n=== by number of distinct texts ===\n")
print(as.data.frame(count(r, distinct, sort = TRUE) |> head(10)), right = FALSE)
cat("\n=== which fields ===\n")
r$leaf <- sub("^.*[$]", "", r$path)
print(as.data.frame(count(r, leaf, sort = TRUE) |> head(12)), right = FALSE)
cat("\n=== the genuine conflicts ===\n")
g <- r |> filter(!collapsible)
cat("count:", nrow(g), "in", dplyr::n_distinct(g$file), "files\n")
print(as.data.frame(g |> transmute(file = substr(file, 1, 34), leaf,
                                   distinct, texts = substr(texts, 1, 70)) |> head(12)), right = FALSE)
