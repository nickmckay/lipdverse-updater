#!/usr/bin/env Rscript
#
# Move compilation-specific metadata out of the shared key namespace and into
# `csm` inside the matching `inCompilation` entry.
#
#   ./scripts/migrate-csm.R --src=~/Dropbox/lipdverse/database --out=/tmp/migrated
#   ./scripts/migrate-csm.R --src=... --out=... --dry-run
#   ./scripts/migrate-csm.R --src=... --out=... --limit=50
#
# Writes to `--out`; the source is never modified. Review the result with
# scripts/shadow-compare.R before swapping anything into place.
#
# Rules, from review/csm-field-names.csv and review/csm-proposal.md:
#
#   single owner  the value moves into that compilation's csm
#   shared        the value is copied into every compilation the dataset
#                 belongs to. Which compilation wrote it is not recoverable
#                 from the file, so copying is lossless and each compilation
#                 can diverge from there.
#   Remove        the key is deleted
#
# Where a target compilation has no membership entry on that column, the key is
# left in place and reported rather than moved: attaching csm to a compilation
# a dataset does not belong to would assert membership that is not there.
#
# On collision (two source keys landing on one compilation+field), values are
# appended with "; ", compilation-private value first, skipping duplicates.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
})
suppressMessages(devtools::load_all(Sys.getenv("LIPDR_PATH", path.expand("~/GitHub/lipdR")), quiet = TRUE))

args    <- commandArgs(trailingOnly = TRUE)
getarg  <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) v[1] else d }
src     <- path.expand(getarg("src", "~/Dropbox/lipdverse/database"))
out     <- path.expand(getarg("out", ""))
limit   <- as.integer(getarg("limit", NA))
dry     <- "--dry-run" %in% args
repo    <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))

if (!nzchar(out) && !dry) stop("--out is required (the source is never modified)")
if (!dir.exists(src)) stop("source not found: ", src)

say <- function(...) cat(sprintf("%s %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

# ---- mapping ---------------------------------------------------------------
map <- read_csv(file.path(repo, "review/csm-field-names.csv"),
                col_types = cols(.default = col_character()), progress = FALSE) |>
  mutate(field = ifelse(is.na(approved_field) | !nzchar(trimws(approved_field)),
                        proposed_field, trimws(approved_field)),
         compilation = trimws(compilation))

removals <- map |> filter(tolower(compilation) == "remove" | tolower(field) == "remove") |> pull(key)
moves    <- map |> filter(!key %in% removals, !is.na(compilation), nzchar(compilation)) |>
  transmute(key, compilation, field,
            shared = !is.na(ownership) & grepl("^shared", ownership))

# Keys are stored on a column without their structural prefix.
bare <- function(k) sub("^(paleoData|chronData|geo|pub[0-9]*|calibration)_", "", k)
moves$bare <- bare(moves$key)
removals_bare <- bare(removals)

say(sprintf("mapping: %d keys move, %d delete (%d shared, %d single-owner)",
            nrow(moves), length(removals), sum(moves$shared), sum(!moves$shared)))

files <- list.files(src, "[.]lpd$", full.names = TRUE)
if (!is.na(limit)) files <- head(files, limit)
say(sprintf("source: %s (%d files)", src, length(files)))
if (dry) { say("DRY RUN -- nothing will be written"); }

if (!dry) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---- helpers ---------------------------------------------------------------
comp_names <- function(col) {
  ic <- col$inCompilation
  if (is.null(ic) || !is.list(ic)) return(character())
  vapply(ic, function(e) {
    n <- if (is.list(e)) unlist(e[["compilationName"]]) else NULL
    if (length(n)) as.character(n)[1] else NA_character_
  }, character(1))
}

set_csm <- function(col, compilation, field, value) {
  nms <- comp_names(col)
  i <- which(nms == compilation)
  if (!length(i)) return(NULL)              # no membership: caller reports it
  i <- i[1]
  cur <- col$inCompilation[[i]]$csm
  if (!is.list(cur)) cur <- list()
  if (!is.null(cur[[field]])) {
    a <- as.character(cur[[field]]); b <- as.character(value)
    if (!identical(a, b) && nzchar(b) && !grepl(b, a, fixed = TRUE)) {
      value <- paste(a, b, sep = "; ")     # append rather than overwrite
    } else {
      value <- a
    }
  }
  cur[[field]] <- value
  col$inCompilation[[i]]$csm <- cur
  col
}

stat <- new.env(parent = emptyenv())
bump <- function(k, n = 1L) stat[[k]] <- (stat[[k]] %||% 0L) + n
`%||%` <- function(a, b) if (is.null(a)) b else a
orphans <- list()

# ---- migrate ---------------------------------------------------------------
t0 <- Sys.time()
for (fi in seq_along(files)) {
  f <- files[fi]
  L <- tryCatch(readLipd(f), error = function(e) NULL)
  if (is.null(L)) { bump("unreadable"); next }
  dsn <- L$dataSetName %||% basename(f)
  changed <- FALSE

  for (pd in seq_along(L$paleoData)) {
    for (tb in seq_along(L$paleoData[[pd]]$measurementTable)) {
      tab <- L$paleoData[[pd]]$measurementTable[[tb]]
      colnames_ <- setdiff(names(tab), c("tableName", "filename", "missingValue"))
      for (cn in colnames_) {
        col <- tab[[cn]]
        if (!is.list(col)) next
        present <- intersect(names(col), unique(c(moves$bare, removals_bare)))
        if (!length(present)) next

        for (b in present) {
          if (b %in% removals_bare) {
            col[[b]] <- NULL; bump("deleted"); changed <- TRUE; next
          }
          rows <- moves[moves$bare == b, ]
          val <- col[[b]]
          targets <- if (any(rows$shared)) comp_names(col) else rows$compilation
          targets <- unique(stats::na.omit(targets))
          placed <- FALSE
          for (tgt in targets) {
            fld <- rows$field[match(TRUE, rows$compilation == tgt)]
            if (is.na(fld)) fld <- rows$field[1]        # shared: same field name everywhere
            res <- set_csm(col, tgt, fld, val)
            if (!is.null(res)) { col <- res; placed <- TRUE; bump("placed") }
          }
          if (placed) {
            col[[b]] <- NULL; changed <- TRUE
          } else {
            bump("orphaned")
            orphans[[length(orphans) + 1L]] <- tibble::tibble(
              dataSetName = dsn, column = cn, key = b,
              wanted = paste(rows$compilation, collapse = ";"),
              has = paste(comp_names(col), collapse = ";"))
          }
        }
        tab[[cn]] <- col
      }
      L$paleoData[[pd]]$measurementTable[[tb]] <- tab
    }
  }

  if (changed) bump("datasets_changed")
  if (!dry) tryCatch(writeLipd(L, path = out, removeNamesFromLists = TRUE),
                     error = function(e) bump("write_failed"))
  if (fi %% 250 == 0) say(sprintf("  %d/%d  (%.0fs)", fi, length(files),
                                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

say(sprintf("done in %.0fs", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
for (k in sort(ls(stat))) say(sprintf("  %-18s %d", k, stat[[k]]))

if (length(orphans)) {
  o <- bind_rows(orphans)
  p <- file.path(repo, "review", paste0("csm-migration-orphans-", basename(src), ".csv"))
  write_csv(o, p, na = "")
  say(sprintf("  orphan report: %s (%d rows, %d distinct keys)",
              p, nrow(o), dplyr::n_distinct(o$key)))
}
