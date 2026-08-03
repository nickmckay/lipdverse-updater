#!/usr/bin/env Rscript
#
# Snapshot every compilation's QC Google Sheet into the git-tracked QC store.
#
# lipdverseR has no versioned QC baseline: `backupQCId` is not actually set for
# any compilation in drakePlan.R, and `lastUpdate.csv` is overwritten in place
# every run. When a merge silently drops curated values there is nothing to
# diff against. This script gives that baseline immediately, and its output is
# the migration input for the QC event store.
#
# Snapshots are written to STABLE paths and committed. Git is the history:
# `git log compilations/hydroclimate2k/QC.csv.gz` shows every change; the files
# are gzipped because the largest QC tabs are tens of megabytes and this runs
# nightly. No timestamped directories needed.
#
#   ./scripts/snapshot-qc-sheets.R
#   ./scripts/snapshot-qc-sheets.R --dry-run
#   ./scripts/snapshot-qc-sheets.R --compilation=hydroclimate2k
#   ./scripts/snapshot-qc-sheets.R --no-commit
#
# Env: LIPDVERSE_QCSTORE   (default ~/GitHub/lipdverse-qcstore)
#      LIPDVERSE_GOOG_EMAIL, LIPDVERSE_GOOG_CACHE

suppressPackageStartupMessages({
  library(googlesheets4)
  library(readr)
})

args        <- commandArgs(trailingOnly = TRUE)
dry_run     <- "--dry-run"  %in% args
no_commit   <- "--no-commit" %in% args
only        <- sub("^--compilation=", "", grep("^--compilation=", args, value = TRUE))

qcstore <- Sys.getenv("LIPDVERSE_QCSTORE", path.expand("~/GitHub/lipdverse-qcstore"))
email   <- Sys.getenv("LIPDVERSE_GOOG_EMAIL", "nick.mckay2@gmail.com")
cache   <- Sys.getenv("LIPDVERSE_GOOG_CACHE", path.expand("~/GitHub/lipdverseR/.secret"))

script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
registry_path <- file.path(script_dir, "..", "inst", "extdata", "compilations.tsv")

ts  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
say <- function(...) cat(sprintf("%s [qcsnap] %s\n", ts, paste0(...)), file = stderr())

# Sheets reads are flaky often enough that lipdverseR wraps every call in a
# retry helper (read_sheet_retry). Same idea, but with backoff.
with_retry <- function(expr, what, tries = 4) {
  for (i in seq_len(tries)) {
    out <- try(force(expr), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    if (i == tries) stop(sprintf("%s failed after %d tries: %s", what, tries, conditionMessage(attr(out, "condition"))))
    Sys.sleep(2^i)
  }
}

# ---- inputs ---------------------------------------------------------------

stopifnot(file.exists(registry_path))
reg <- read_tsv(registry_path, col_types = cols(.default = col_character()), progress = FALSE)
reg <- reg[!is.na(reg$compilation) & nzchar(reg$compilation), ]
if (length(only) > 0) {
  reg <- reg[reg$compilation %in% only, ]
  if (nrow(reg) == 0) stop("no compilation matching: ", paste(only, collapse = ", "))
}
say(sprintf("%d compilation(s) to snapshot", nrow(reg)))

# Shared sheets that are not per-compilation but are equally unversioned.
shared <- data.frame(
  compilation    = c("_shared/convo", "_shared/versions", "_shared/vocab-registry"),
  qc_sheet_id    = c("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w",
                     "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",
                     "16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY"),
  last_update_id = NA_character_,
  age_or_year    = NA_character_,
  stringsAsFactors = FALSE
)

targets <- rbind(as.data.frame(reg[, names(shared)[1:4]]), shared)

if (dry_run) {
  say("DRY RUN -- would snapshot:")
  for (i in seq_len(nrow(targets))) say(sprintf("  %-28s %s", targets$compilation[i], targets$qc_sheet_id[i]))
  say(sprintf("  into %s", qcstore))
  quit(status = 0)
}

gs4_auth(email = email, cache = cache)

dir.create(qcstore, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(file.path(qcstore, ".git"))) {
  say("initialising git repo at ", qcstore)
  system2("git", c("-C", shQuote(qcstore), "init", "-q"))
  writeLines(c("# LiPDverse QC store",
               "",
               "Verbatim snapshots of every QC Google Sheet, written to stable paths.",
               "Git is the history: `git log -p <file>` shows every change to any cell.",
               "",
               "Written by lipdverse-updater/scripts/snapshot-qc-sheets.R.",
               "Do not hand-edit; this directory is overwritten on every run."),
             file.path(qcstore, "README.md"))
  writeLines(c("*.duckdb", "*.parquet.tmp", ".DS_Store"), file.path(qcstore, ".gitignore"))
}

# ---- snapshot -------------------------------------------------------------

failures <- character()
summary  <- list()

for (i in seq_len(nrow(targets))) {
  comp <- targets$compilation[i]
  id   <- targets$qc_sheet_id[i]
  if (is.na(id) || !nzchar(id)) next

  out_dir <- file.path(qcstore, "snapshots", comp)

  res <- try({
    meta <- with_retry(gs4_get(id), sprintf("gs4_get(%s)", comp))
    tabs <- meta$sheets$name

    # Rewrite the directory from scratch so a tab deleted upstream shows up as
    # a deletion in git rather than lingering as a stale file forever.
    if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    rows <- integer(length(tabs))
    for (j in seq_along(tabs)) {
      tab <- tabs[j]
      # Always by NAME, never by index: lipdverseR's `sheet = 1` reads broke
      # whenever tab order shifted. Always as character: type guessing silently
      # dropped values in sparse columns.
      d <- with_retry(
        range_read(id, sheet = tab, col_types = "c", .name_repair = "minimal"),
        sprintf("read %s!%s", comp, tab)
      )
      rows[j] <- nrow(d)
      # Gzipped: HoloceneHydroclimate's QC tab alone is 73 MB uncommpressed and
      # LegacyClimate-LiPD 63 MB, and this runs nightly. Every content change
      # writes a new blob, so plain CSV would push the repo past GitHub's size
      # guidance within a year. These compress about 10:1, and `git log -p` on a
      # 73 MB CSV was never usable anyway -- the history is for recovery, not
      # for reading diffs. readr reads and writes .csv.gz transparently.
      write_csv(d, file.path(out_dir, paste0(gsub("[/\\\\]", "_", tab), ".csv.gz")), na = "")
    }

    jsonline <- sprintf(
      '{\n  "compilation": "%s",\n  "sheet_id": "%s",\n  "snapshot_utc": "%s",\n  "tabs": [%s],\n  "rows": [%s]\n}\n',
      comp, id, ts,
      paste(sprintf('"%s"', tabs), collapse = ", "),
      paste(rows, collapse = ", "))
    writeLines(jsonline, file.path(out_dir, "_meta.json"))

    summary[[comp]] <- sprintf("%s (%d tabs, %d rows)", comp, length(tabs), sum(rows))
    say(sprintf("  %-28s %d tabs, %d rows", comp, length(tabs), sum(rows)))
  }, silent = TRUE)

  if (inherits(res, "try-error")) {
    msg <- conditionMessage(attr(res, "condition"))
    say(sprintf("  %-28s FAILED: %s", comp, msg))
    failures <- c(failures, sprintf("%s: %s", comp, msg))
  }
}

# ---- commit ---------------------------------------------------------------

if (!no_commit) {
  system2("git", c("-C", shQuote(qcstore), "add", "-A"))
  changed <- system2("git", c("-C", shQuote(qcstore), "diff", "--cached", "--quiet"))
  if (changed != 0) {
    stat <- system2("git", c("-C", shQuote(qcstore), "diff", "--cached", "--shortstat"), stdout = TRUE)
    msg  <- sprintf("QC snapshot %s\n\n%s\n\n%s", ts,
                    paste(unlist(summary), collapse = "\n"),
                    if (length(failures)) paste("FAILED:", paste(failures, collapse = "\n")) else "")
    system2("git", c("-C", shQuote(qcstore), "commit", "-q", "-m", shQuote(msg)))
    say("committed: ", trimws(paste(stat, collapse = " ")))
  } else {
    say("no changes since last snapshot")
  }
}

if (length(failures)) {
  say(sprintf("%d compilation(s) failed", length(failures)))
  quit(status = 1)
}
say("done")
