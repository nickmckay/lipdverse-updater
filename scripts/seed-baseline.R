#!/usr/bin/env Rscript
#
# Establish a compilation's baseline in the QC store.
#
#   ./scripts/seed-baseline.R hydroclimate2k --from=files
#   ./scripts/seed-baseline.R hydroclimate2k --from=files --commit
#
# The three-way merge needs a baseline to tell "the curator edited this" from
# "the file changed". A compilation that has never run under this system has
# none, so every sheet-vs-file difference on a shared field becomes a conflict
# and the run aborts. This writes that baseline once.
#
# ---------------------------------------------------------------------------
# WHICH SIDE TO SEED FROM IS A PER-COMPILATION JUDGEMENT, AND GETTING IT WRONG
# SILENTLY OVERWRITES REAL DATA.
#
# Seeding from a side declares "this side is unchanged since the last run".
# Whatever the *other* side says then reads as an edit and gets applied.
#
#   --from=files   The files are current; the sheet holds curator edits to
#                  apply. Right when a compilation's files have not moved since
#                  its last update but leads are still editing the sheet.
#                  This is hydroclimate2k as of 2026-08-03.
#
#   --from=sheet   The sheet is the stale side and the files have moved on.
#                  Seeding from files here would apply an old sheet over newer
#                  file content. Likely correct for the older compilations,
#                  where files have kept evolving through other compilations'
#                  runs while the sheet sat untouched for years.
#
#   --from=qcts    The state the last legacy run actually resolved to, read from
#                  html/<compilation>/<version>/qcTs.csv. Neither current side is
#                  declared unchanged: the baseline is what lipdverseR last
#                  wrote, so a difference on either side reads as a change made
#                  since, which is what it is. Use --version= to pick the
#                  release; the newest published one is the default.
#
# There is no safe default, so --from is required.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
comp   <- args[!grepl("^--", args)][1]
getarg <- function(f, d = NULL) { v <- sub(paste0("^--", f, "="), "", grep(paste0("^--", f, "="), args, value = TRUE)); if (length(v)) v[1] else d }
from   <- getarg("from")
commit <- "--commit" %in% args
force  <- "--force" %in% args

if (is.na(comp)) stop("usage: seed-baseline.R <compilation> --from=files|sheet [--commit]")
if (!isTRUE(from %in% c("files", "sheet", "qcts"))) {
  stop("--from=files, --from=sheet or --from=qcts is required; see the header for which to pick")
}
version <- getarg("version")

cfg   <- lv_config(comp)
db    <- lv_path("database")
store <- qc_store()
bk    <- sheet_backend_google()
run   <- lv_run_id()

cat(sprintf("compilation : %s\nseed from   : %s\nrun_id      : %s\nmode        : %s\n\n",
            comp, from, run, if (commit) "COMMIT" else "dry run"))

existing <- qc_state_current(store, comp)
if (nrow(existing) && !force) {
  stop(sprintf("%s already has a baseline of %d cells. Seeding again would rewrite history; pass --force if that is really intended.",
               comp, nrow(existing)))
}

idx <- lv_db_index(lv_scan(db), cache = TRUE)
mem <- lv_compilation_datasets(cfg, bk, idx)
ds  <- mem$datasets
ts <- lv_qc_timeseries(idx, datasets = ds)   # paleoData only
cat(sprintf("considered  : %d datasets, %d timeseries\n", length(ds), length(ts)))

frame <- qc_frame(db, datasets = ds, progress = FALSE)
frame <- frame[frame$tsid %in% ts, , drop = FALSE]
frame <- bind_rows(frame, lv_membership_frame(idx, comp, ts))
sheet <- qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)

cells <- switch(from,
  files = frame,
  sheet = sheet,
  qcts  = {
    # The recorded output of a published legacy run, parsed by the same reader
    # the replay harness uses.
    v <- lv_replay_versions(comp)
    if (!nrow(v)) stop("no published version of ", comp, " carries a qcTs.csv")
    pick <- if (is.null(version)) utils::tail(v$version, 1) else version
    row <- v[v$version == pick, , drop = FALSE]
    if (!nrow(row)) {
      stop("version ", pick, " has no qcTs.csv; available: ", paste(v$version, collapse = ", "))
    }
    cat(sprintf("seeding from : %s\n", file.path(row$path[1], "qcTs.csv")))
    lv_replay_cells(file.path(row$path[1], "qcTs.csv"))
  })
cells <- cells[cells$tsid %in% ts, , drop = FALSE]
cat(sprintf("frame       : %d cells\nsheet       : %d cells\nbaseline    : %d cells from the %s\n",
            nrow(frame), nrow(sheet), nrow(cells), from))

# What the first real run will then do, which is the number worth checking
# before committing a baseline.
plan <- qc_merge(cells, sheet, frame)
cat("\nafter seeding, the first run would see:\n")
print(as.data.frame(count(tibble::as_tibble(plan$cells), resolution)))
if (nrow(plan$conflicts)) {
  cat("\nremaining conflicts by field:\n")
  print(as.data.frame(count(tibble::as_tibble(plan$conflicts), field, sort = TRUE)))
}
ch <- plan$cells[plan$cells$resolution == "sheet", , drop = FALSE]
if (nrow(ch)) {
  cat("\ntop fields the sheet would write into the files:\n")
  print(as.data.frame(head(count(tibble::as_tibble(ch), field, sort = TRUE), 12)))
}

if (!commit) {
  cat("\npass --commit to write the baseline.\n")
  quit(save = "no")
}

ev <- qc_diff_to_events(qc_cells_empty(), cells, source = "migration",
                        reason = sprintf("baseline seeded from the %s", from))
qc_store_append(store, comp, ev, run_id = run)
cat(sprintf("\nseeded %d event%s into the store for %s\n", nrow(ev),
            if (nrow(ev) == 1) "" else "s", comp))
