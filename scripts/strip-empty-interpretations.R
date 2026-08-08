#!/usr/bin/env Rscript
# Remove interpretations that assert nothing: no fields at all, or only a scope.
#
# These came from the lipdR collapse bug fixed in 5d11e11. A time series tibble
# is rectangular, so a column with fewer interpretations than the dataset's
# richest column arrived with all-NA cells, and collapsing materialised those as
# interpretations whose only content was the scope taken from the block name.
#
# 199,431 of them across the database on 2026-08-08, against 88,017 real ones.
# Removed in two runs, 20260808T160719-mo11u8 and 20260808T180919-owgrcc.
#
# Walks every table a column can live in: measurement, and each model summary,
# ensemble and distribution table. A first attempt walked only measurementTable
# and left 37,035 behind, which only surfaced because a separate survey counted
# both and the numbers disagreed.
#
#   Rscript scripts/strip-empty-interpretations.R              dry run
#   LV_APPLY=1 Rscript scripts/strip-empty-interpretations.R   promote
#
# Snapshot first. Verifies every changed file before promoting -- TSids, content
# count and validLipd -- and aborts without writing if any fails.

suppressMessages(devtools::load_all("~/GitHub/lipdverse-updater", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))
DRY   <- !identical(Sys.getenv("LV_APPLY"), "1")
LIMIT <- as.integer(Sys.getenv("LV_LIMIT", "0"))

db <- lv_path("database")
stage <- path.expand("~/lipdverse-staging/strip-model-interp")
if (fs::dir_exists(stage)) fs::dir_delete(stage)
fs::dir_create(stage)

scal <- function(v) {
  if (is.null(v) || !length(v)) return(NA_character_)
  v <- unlist(v)[1]
  if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
}
filled <- function(it) {
  if (!is.list(it) || !length(it)) return(character())
  names(it)[vapply(it, function(v) { s <- scal(v); !is.na(s) && nzchar(trimws(s)) }, logical(1))]
}
removable <- function(it) { k <- filled(it); length(k) == 0L || identical(sort(k), "scope") }
cols_of <- function(tb) if (!is.null(tb$columns)) tb$columns else
  tb[!names(tb) %in% c("filename", "tableName", "missingValue", "googWorkSheetKey")]

strip_tab <- function(tb) {
  if (!is.list(tb)) return(list(tb = tb, n = 0L))
  cols <- cols_of(tb); n <- 0L
  for (i in seq_along(cols)) {
    cl <- cols[[i]]
    if (!is.list(cl) || is.null(cl$interpretation)) next
    drop <- vapply(cl$interpretation, removable, logical(1))
    if (!any(drop)) next
    n <- n + sum(drop)
    cl$interpretation <- if (any(!drop)) cl$interpretation[!drop] else NULL
    cols[[i]] <- cl
  }
  if (n && !is.null(tb$columns)) tb$columns <- cols else if (n) for (nm in names(cols)) tb[[nm]] <- cols[[nm]]
  list(tb = tb, n = n)
}

# Everything a table can hang from: the measurement tables, and each model's
# summary and ensemble tables. The first pass walked only measurementTable, which
# left 37,510 removable interpretations sitting in model tables.
walk <- function(L, apply_it) {
  n <- 0L
  for (blk in c("paleoData", "chronData")) {
    for (pi in seq_along(L[[blk]])) {
      for (ti in seq_along(L[[blk]][[pi]]$measurementTable)) {
        r <- strip_tab(L[[blk]][[pi]]$measurementTable[[ti]]); n <- n + r$n
        if (apply_it) L[[blk]][[pi]]$measurementTable[[ti]] <- r$tb
      }
      for (mi in seq_along(L[[blk]][[pi]]$model)) {
        for (kind in c("summaryTable", "ensembleTable", "distributionTable")) {
          for (ti in seq_along(L[[blk]][[pi]]$model[[mi]][[kind]])) {
            r <- strip_tab(L[[blk]][[pi]]$model[[mi]][[kind]][[ti]]); n <- n + r$n
            if (apply_it) L[[blk]][[pi]]$model[[mi]][[kind]][[ti]] <- r$tb
          }
        }
      }
    }
  }
  list(L = L, n = n)
}
count_content <- function(L) {
  n <- 0L
  for (blk in c("paleoData", "chronData")) for (pi in seq_along(L[[blk]])) {
    tabs <- c(L[[blk]][[pi]]$measurementTable,
              unlist(lapply(L[[blk]][[pi]]$model,
                            function(m) c(m$summaryTable, m$ensembleTable, m$distributionTable)),
                     recursive = FALSE))
    for (tb in tabs) { if (!is.list(tb)) next
      for (cl in cols_of(tb)) if (is.list(cl) && !is.null(cl$interpretation))
        n <- n + sum(!vapply(cl$interpretation, removable, logical(1))) }
  }
  n
}

files <- list.files(db, "[.]lpd$")
if (LIMIT > 0) files <- head(files, LIMIT)
cat("scanning", length(files), "files for removable interpretations in any table\n")

future::plan(future::multisession, workers = min(12L, future::availableCores() - 2L))
t0 <- Sys.time()
one <- function(f) {
  L <- tryCatch(suppressWarnings(lipdR::readLipd(fs::path(db, f))), error = function(e) NULL)
  if (is.null(L)) return(list(file = f, n = 0L, before = 0L, after = 0L, ok = FALSE))
  n <- walk(L, FALSE)$n
  if (n == 0L) return(list(file = f, n = 0L, before = 0L, after = 0L, ok = TRUE))
  before <- count_content(L)
  L <- walk(L, TRUE)$L
  after <- count_content(L)
  suppressWarnings(lipdR::writeLipd(L, path = stage, removeNamesFromLists = TRUE))
  list(file = f, n = n, before = before, after = after, ok = TRUE)
}
res <- furrr::future_map(files, one, .options = furrr::furrr_options(seed = TRUE,
  globals = c("db", "stage", "scal", "filled", "removable", "cols_of", "strip_tab", "walk", "count_content")))
removed <- sum(vapply(res, function(z) z$n, integer(1)))
cat("stripped in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("removed:", removed, "| content before:", sum(vapply(res, function(z) z$before, integer(1))),
    "after:", sum(vapply(res, function(z) z$after, integer(1))), "\n")

staged <- list.files(stage, "[.]lpd$")
cat("files changed:", length(staged), "\n")
if (!length(staged)) { cat("nothing to do\n"); quit() }

cat("\n=== verify ===\n")
check <- function(f) {
  A <- suppressWarnings(lipdR::readLipd(fs::path(db, f)))
  B <- suppressWarnings(lipdR::readLipd(fs::path(stage, f)))
  ts <- function(L) sort(unlist(lapply(c("paleoData","chronData"), function(b)
    lapply(L[[b]], function(p) lapply(p$measurementTable, function(t)
      vapply(Filter(function(x) is.list(x) && !is.null(x$TSid), cols_of(t)), function(x) as.character(x$TSid), ""))))))
  why <- c()
  if (!identical(ts(A), ts(B))) why <- c(why, "TSids")
  if (!identical(count_content(A), count_content(B))) why <- c(why, "lost content")
  if (!isTRUE(tryCatch({ suppressWarnings(lipdR::validLipd(B)); TRUE }, error = function(e) FALSE))) why <- c(why, "validLipd")
  if (length(why)) paste(f, "-", paste(why, collapse = ", ")) else NA_character_
}
bad <- unlist(furrr::future_map(staged, check, .options = furrr::furrr_options(seed = TRUE,
  globals = c("db", "stage", "scal", "filled", "removable", "cols_of", "count_content"))))
bad <- bad[!is.na(bad)]
cat("verified:", length(staged) - length(bad), "of", length(staged), "\n")
if (length(bad)) { cat(paste(" ", head(bad, 15)), sep = "\n"); cat("\nNOT promoting.\n"); quit(status = 1) }

if (DRY) { cat("\nDRY RUN. Set LV_APPLY=1 to promote.\n"); quit() }
invisible(lv_promote(stage, db, run_id = lv_run_id(), partial = TRUE, dry_run = FALSE))
