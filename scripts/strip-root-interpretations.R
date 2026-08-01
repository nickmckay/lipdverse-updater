#!/usr/bin/env Rscript
#
# Remove flattened interpretation keys from the dataset root.
#
#   ./scripts/strip-root-interpretations.R --dry-run
#   ./scripts/strip-root-interpretations.R --stage
#   ./scripts/strip-root-interpretations.R --promote --commit
#
# 624 datasets carry keys like `environmentInterpretation1_variable` at the
# dataset root, left by lipdverseR's flattening. Interpretation is a column-level
# concept: a root copy claims to describe every column of the dataset at once,
# which is never right, and these copies shadowed the real per-column values
# badly enough to hide the fact that qc_frame was not reading them at all.
#
# qc_frame now ignores them, so they are inert. This takes them out of the files.
#
# Not all of them. Measured across the database, of 2,292 root keys:
#
#   1,320  hold no value at all
#     899  hold a value that is already on one of the dataset's columns
#      73  hold a value found nowhere on any column
#
# The last group is left in place and reported, the same way the csm migration
# handled its orphans. --force removes them.
#
# There is an obvious-looking way to rescue them that does not work, recorded
# here so it is not tried again. The QC sheets are keyed by TSid, so they look
# like they hold the per-column attribution the flattening lost: all 73 values
# are present in hydroclimate2k's sheet, across all 69 datasets, mapping onto
# 251 columns. But the sheet's values are the same flattening. For every one of
# the 73, *every* TSid in the dataset carries the identical value, because
# lipdverseR built the QC row set by stamping the dataset-level value onto each
# column. Placing them back would write an environment interpretation onto
# uncertainty (19 columns), uncertaintyHigh (18), uncertaintyLow (18),
# correction, mineralogy, sampleID and core -- columns that cannot have one.
# The attribution is circular, and acting on it would make the flattening
# permanent instead of removing it.

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) path.expand(v[1]) else d }
db     <- getarg("src", lv_path("database"))
stage  <- getarg("stage-dir", path.expand("~/lipdverse-staging/root-interp"))
force  <- "--force" %in% args
# An explicit, reviewed table of which column each otherwise-unrepresented value
# belongs to. A table rather than a rule: the ownership was established per
# dataset (the column carrying real interpretations, or a decision where there
# was none), and a heuristic general enough to cover all four would be wrong
# somewhere. With --place every root key becomes removable, because the values
# that had nowhere to go now have somewhere.
place  <- getarg("place", if ("--place" %in% args) "review/root-interpretation-placements.csv" else NULL)

do_stage   <- "--stage" %in% args
do_promote <- "--promote" %in% args

ROOT_RE <- "Interpretation[0-9]+_"

# Every (field, value) carried by any interpretation on any column, ignoring
# scope and index: the root name's scope is not trustworthy (a root
# `environmentInterpretation1_variable` routinely matches an isotope-scope
# column value), so a looser test is the honest one for "already present".
column_values <- function(m) {
  have <- character()
  for (blk in c("paleoData", "chronData")) for (pd in m[[blk]]) {
    tabs <- c(pd$measurementTable,
              unlist(lapply(pd$model, function(md) c(md$summaryTable, md$ensembleTable)),
                     recursive = FALSE))
    for (tb in tabs) {
      if (!is.list(tb)) next
      cols <- if (!is.null(tb$columns)) tb$columns else tb
      for (cl in cols) {
        if (!is.list(cl)) next
        for (it in cl$interpretation) {
          if (!is.list(it)) next
          for (s in names(it)) have <- c(have, paste0(s, "=", as.character(unlist(it[[s]]))[1]))
        }
      }
    }
  }
  have
}

read_meta <- function(p) {
  nm <- tryCatch(utils::unzip(p, list = TRUE)$Name, error = function(e) NULL)
  j <- grep("jsonld$", nm, value = TRUE)
  if (!length(j)) return(NULL)
  con <- unz(p, j[1]); on.exit(close(con), add = TRUE)
  tryCatch(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                              simplifyVector = FALSE), error = function(e) NULL)
}

files <- list.files(db, "[.]lpd$", full.names = TRUE)
cat(sprintf("source: %s (%d files)\nmode  : %s%s\n\n", db, length(files),
            if (do_promote) "promote" else if (do_stage) "stage" else "dry run",
            if (force) "  [--force: removing unmatched values too]" else ""))

plan <- list(); n_ds <- 0L
for (p in files) {
  m <- read_meta(p)
  if (is.null(m)) next
  keys <- grep(ROOT_RE, names(m), value = TRUE)
  if (!length(keys)) next
  have <- column_values(m)
  for (k in keys) {
    v <- m[[k]]
    v <- if (is.null(v) || !length(v)) NA_character_ else as.character(unlist(v))[1]
    empty <- is.na(v) || !nzchar(v)
    fld <- sub(paste0("^.*", ROOT_RE), "", k)
    dup <- !empty && paste0(fld, "=", v) %in% have
    # The root copies are the older, unstandardised wording of what the columns
    # already say: root "sea surface temperature" against a column carrying
    # climate:temperature and isotope:temperature. Comparing the strings exactly
    # would call 67 of these unique when they carry no information the file does
    # not already hold, more precisely, elsewhere. Labelled separately from an
    # exact duplicate so the report stays auditable.
    same_field <- sub("^([^=]*)=.*$", "\\1", have) == fld
    red <- !empty && !dup && any(same_field) &&
      any(vapply(sub("^[^=]*=", "", have[same_field]), function(w) {
        # Equality after dropping the qualifier, not substring matching in
        # either direction: "precipitationisotope" contains "precipitation" but
        # the isotopic composition of precipitation is not its amount.
        a <- sub("^(sea surface|subsurface seawater|lake water) ", "", tolower(v))
        b <- tolower(w)
        !is.na(b) && nzchar(b) && a == b
      }, logical(1)))
    plan[[length(plan) + 1L]] <- data.frame(
      file = basename(p), key = k, value = v,
      disposition = if (empty) "empty" else if (dup) "duplicate"
                    else if (red) "restatement" else "unmatched")
  }
  n_ds <- n_ds + 1L
}
plan <- bind_rows(plan)
plan$remove <- plan$disposition %in% c("empty", "duplicate", "restatement") | force

cells <- NULL
if (!is.null(place)) {
  pl <- read_csv(place, col_types = cols(.default = col_character()), progress = FALSE)
  cells <- tibble::tibble(tsid = pl$TSid, field = pl$key, value = pl$value,
                          present = TRUE, dataset_id = NA_character_)
  covered <- paste0(pl$dataSetName, ".lpd|", pl$key, "|", pl$value)
  hit <- paste0(plan$file, "|", plan$key, "|", plan$value) %in% covered
  plan$remove <- plan$remove | hit
  cat(sprintf("placements: %d value%s onto %d column%s in %d dataset%s\n",
              n_distinct(paste(pl$dataSetName, pl$key)),
              if (n_distinct(paste(pl$dataSetName, pl$key)) == 1) "" else "s",
              nrow(pl), if (nrow(pl) == 1) "" else "s",
              n_distinct(pl$dataSetName), if (n_distinct(pl$dataSetName) == 1) "" else "s"))
}


cat(sprintf("%d root key%s across %d dataset%s\n\n", nrow(plan),
            if (nrow(plan) == 1) "" else "s", n_ds, if (n_ds == 1) "" else "s"))
print(as.data.frame(count(plan, disposition, remove)))
write_csv(plan, "review/root-interpretation-keys.csv", na = "")
cat("\nreport: review/root-interpretation-keys.csv\n")

kept <- plan[!plan$remove, ]
if (nrow(kept)) {
  cat(sprintf("\n%d key%s kept (value found on no column). Pass --force to remove.\n",
              nrow(kept), if (nrow(kept) == 1) "" else "s"))
}

if (!do_stage && !do_promote) quit(save = "no")

# ---- write -----------------------------------------------------------------

todo <- unique(plan$file[plan$remove])
if (do_stage) {
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  for (f in todo) {
    L <- tryCatch(lipdR::readLipd(file.path(db, f)), error = function(e) NULL)
    if (is.null(L)) { cat("unreadable:", f, "\n"); next }
    for (k in plan$key[plan$file == f & plan$remove]) L[[k]] <- NULL
    lipdR::writeLipd(L, path = stage, removeNamesFromLists = TRUE)
  }
  cat(sprintf("\nstaged %d file%s in %s\n", length(list.files(stage, "[.]lpd$")),
              if (length(todo) == 1) "" else "s", stage))

  if (!is.null(cells) && nrow(cells)) {
    # Read from staging, where the root keys are already gone, so the two edits
    # compose into one file instead of the second undoing the first.
    idx <- lv_db_index(lv_scan(db), cache = TRUE)
    staged <- sub("[.]lpd$", "", list.files(stage, "[.]lpd$"))
    idx$datasets$path[match(staged, idx$datasets$fileDataSetName)] <-
      fs::path(stage, paste0(staged, ".lpd"))
    iss <- lv_apply_qc(cells, db, stage, index = idx, progress = FALSE)
    cat(sprintf("placed %d value%s\n", nrow(cells), if (nrow(cells) == 1) "" else "s"))
    if (nrow(iss)) print(as.data.frame(count(tibble::as_tibble(iss), check, severity)))
  }
}

if (do_promote) {
  stopifnot(dir.exists(stage))
  if ("--commit" %in% args) system2("scripts/snapshot-database.sh", stdout = TRUE)
  rec <- lv_promote(stage, db, run_id = "strip-root-interpretations", partial = TRUE,
                    dry_run = !("--commit" %in% args))
  print(rec)
  if (!("--commit" %in% args)) cat("\npass --commit to write.\n")
}
