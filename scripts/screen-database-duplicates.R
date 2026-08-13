# Screen the whole database against itself for duplicate datasets (task #18).
#
# The "28 duplicates" figure this task carries came from ingest screens run
# before lv_duplicate_screen() worked -- a tibble() column named `existing`
# shadowed the argument of the same name, so the hash column held dataset names
# and the join could never match. Every screen came back clean. That figure is
# therefore not a measurement of anything, and this rebuilds it.
#
# Two files are the same record if they carry the same numbers, whatever they
# are called. But only *diagnostic* numbers count, and the first pass showed how
# much of the database is not:
#
#   - chronData is boilerplate. Two chron uncertainty columns are byte-identical
#     across 346 datasets each -- a shared age-model envelope, not data -- and
#     between them they generated 119,370 of the first pass's 61,454 pairs.
#     Flag columns (`rejected`, `thickness`, `deltaR`) are constants repeated
#     across families of files.
#   - a constant column identifies nothing, however long it is.
#   - a column appearing in many datasets is, by that fact alone, not evidence
#     that any two of them are the same record.
#
# Those three filters are applied and reported, not applied silently: the point
# of the screen is a number someone can act on.
suppressMessages(devtools::load_all(quiet = TRUE))
suppressPackageStartupMessages({library(dplyr); library(parallel)})

DIR    <- lv_path("database")
CACHE  <- fs::path_expand("~/lipdverse-staging/value-hashes-detail.rds")
OUT    <- fs::path_expand("~/lipdverse-staging/review/database-duplicates.csv")
CORES  <- max(1L, detectCores() - 2L)
MAX_DS <- 5L   # a hash in more than this many datasets is boilerplate

paths <- fs::dir_ls(DIR, glob = "*.lpd", type = "file")
fp <- lv_scan(DIR)$fingerprint
cat("files:", length(paths), "\n")

if (fs::file_exists(CACHE) && identical(readRDS(CACHE)$fingerprint, fp)) {
  cols <- readRDS(CACHE)$cols
  cat("column hashes from cache\n")
} else {
  cat("hashing on", CORES, "cores\n"); t0 <- Sys.time()
  x <- mclapply(paths, lv_value_hashes_one, detail = TRUE, mc.cores = CORES)
  names(x) <- sub("\\.lpd$", "", fs::path_file(paths))
  # mclapply returns the error condition rather than throwing, so a dead worker
  # would otherwise become a dataset with no columns and drop silently out of
  # the screen.
  bad <- !vapply(x, is.data.frame, logical(1))
  if (any(bad)) { print(names(x)[bad]); stop("hashing failed") }
  cols <- bind_rows(x, .id = "ds")
  fs::dir_create(fs::path_dir(CACHE))
  saveRDS(list(fingerprint = fp, cols = cols), CACHE)
  cat("hashed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
}

cat("\ncolumns hashed:", nrow(cols), "across", n_distinct(cols$ds), "datasets\n")

keep <- cols |> filter(block == "paleoData", n_unique > 1)
cat("dropped, chronData:      ", sum(cols$block != "paleoData"), "\n")
cat("dropped, constant column:", sum(cols$block == "paleoData" & cols$n_unique <= 1), "\n")

spread <- keep |> count(hash, name = "n_ds")
boiler <- spread |> filter(n_ds > MAX_DS)
cat("dropped, in >", MAX_DS, "datasets:", sum(keep$hash %in% boiler$hash),
    "columns /", nrow(boiler), "distinct\n")
if (nrow(boiler)) {
  cat("\n  the boilerplate paleo columns:\n")
  print(as.data.frame(keep |> filter(hash %in% boiler$hash) |>
                        left_join(spread, by = "hash") |>
                        distinct(hash, .keep_all = TRUE) |>
                        arrange(desc(n_ds)) |> select(n_ds, variable, n, n_unique) |>
                        head(12)), right = FALSE)
}
keep <- keep |> filter(!hash %in% boiler$hash)
cat("\ndiagnostic columns:", nrow(keep), "across", n_distinct(keep$ds), "datasets\n")

h <- split(keep$hash, keep$ds)

# Positive control. A screen that reports nothing and a screen that is not
# running are indistinguishable from their output, so make it report something
# known first: datasets fed in as if new must match themselves.
ctl <- names(h)[lengths(h) >= 5][1:2]
c1 <- lv_duplicate_screen(h[ctl], h, NULL)
ok <- nrow(c1) >= 2 && all(ctl %in% c1$new[c1$same_name])
cat("positive control:", if (ok) "PASS" else "FAIL", "\n")
if (!ok) { print(as.data.frame(c1)); stop("the screen is not matching") }

# Self-comparison: drop a dataset matching itself, keep one row per unordered
# pair.
m <- lv_duplicate_screen(h, h, NULL) |> filter(new != existing, new < existing)
cat("\ncandidate pairs:", nrow(m), "\n")
if (!nrow(m)) { cat("no duplicates\n"); quit() }
print(as.data.frame(count(m, disposition)), right = FALSE)

m <- m |> arrange(desc(containment), desc(shared))
fs::dir_create(fs::path_dir(OUT))
readr::write_csv(m |> select(new, existing, shared, n_new, n_existing, containment,
                             disposition, recommendation), OUT)
cat("\nwritten:", OUT, "\n\n")
print(as.data.frame(m |> select(new, existing, shared, n_new, n_existing, containment) |>
                      head(50)), right = FALSE)
