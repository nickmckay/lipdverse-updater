# Daff conflict markers left in the files by the legacy pipeline.
#
# lipdverseR's three-way merge wrote unresolved conflicts into the value itself
# as "((( a ))) b /// c", then patched them out by string substitution. Where
# that missed, the marker stayed -- and the next run merged the marked value
# again, so each run added another layer. PinkPanther.Asmerom.2007 reached
# 99,833 characters: one sentence, 297 times.
suppressPackageStartupMessages(library(dplyr))
db <- path.expand("~/Dropbox/lipdverse/database")
files <- list.files(db, "[.]lpd$")
future::plan(future::multisession, workers = min(12L, future::availableCores() - 2L))

one <- function(f) {
  p <- file.path(db, f)
  nms <- tryCatch(unzip(p, list = TRUE)$Name, error = function(e) NULL)
  j <- grep("[.]jsonld$", nms, value = TRUE); if (!length(j)) return(NULL)
  con <- unz(p, j[1])
  txt <- tryCatch(paste(readLines(con, warn = FALSE), collapse = "\n"), error = function(e) NULL)
  close(con); if (is.null(txt)) return(NULL)
  # Cheap text test first: parsing every file to walk it is far slower, and a
  # marker is a literal string.
  n_open <- lengths(regmatches(txt, gregexpr("(((", txt, fixed = TRUE)))
  n_sep  <- lengths(regmatches(txt, gregexpr(" /// ", txt, fixed = TRUE)))
  if (n_open == 0 && n_sep == 0) return(NULL)
  tibble::tibble(file = f, n_open = n_open, n_sep = n_sep, bytes = nchar(txt))
}
r <- bind_rows(furrr::future_map(files, one, .options = furrr::furrr_options(
  seed = TRUE, globals = c("db"), packages = c("tibble", "dplyr"))))
saveRDS(r, "/Users/nicholas/lipdverse-staging/conflict-markers.rds")

cat("files scanned:", length(files), "\n")
cat("files carrying a conflict marker:", nrow(r), "\n\n")
if (!nrow(r)) quit()
print(as.data.frame(r |> arrange(desc(n_open)) |> head(15)), right = FALSE)
cat("\ntotal '(((' occurrences:", sum(r$n_open), "| ' /// ':", sum(r$n_sep), "\n")
