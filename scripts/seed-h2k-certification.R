#!/usr/bin/env Rscript
#
# Execute the reviewed dispositions in review/hydroclimate2k-certification.csv.
#
#   ./scripts/seed-h2k-certification.R            # dry run, writes nothing
#   ./scripts/seed-h2k-certification.R --commit   # seed the store, promote files
#
# A one-off. It seeds hydroclimate2k's certification baseline **from the QC
# sheet rather than from the files**, which is backwards from every other path
# in this package, because here the files are the corrupted side: the csm
# migration copied the shared paleoData_QCCertification into every member
# compilation, so most of what a file says about hydroclimate2k was written by
# someone else. The sheet's 10 distinct strings are hydroclimate2k's own; the
# files carry 33, of which one is shared with the sheet.
#
# Every row's disposition comes from the review file, never from this script.
# A blank `decision` accepts `suggested_action`, which is the convention for
# review files here. Any other decision text aborts rather than being guessed at.
#
# Order matters and matches the runner: membership is not touched, csm is
# written into the entry that already exists, and the legacy flat key is removed
# from columns that have no entry to move it into.

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
commit <- "--commit" %in% args
comp   <- "hydroclimate2k"
FIELD  <- "paleoData_hydroclimate2kCertification"
FLAT   <- "hydroclimate2kCertification"
review <- file.path("review", "hydroclimate2k-certification.csv")
stage  <- path.expand(file.path("~/lipdverse-staging", paste0("seed-cert-", comp)))

if (!file.exists(review)) stop("no review file at ", review,
                               "; run scripts/reconcile-h2k-certification.R first")

r <- readr::read_csv(review, col_types = readr::cols(.default = readr::col_character()),
                     progress = FALSE)
r$action <- ifelse(is.na(r$decision) | !nzchar(trimws(r$decision)),
                   r$suggested_action, trimws(r$decision))

# A decision nobody can execute must stop the run, not be skipped quietly: the
# whole point of the review file is that the judgement in it is acted on.
KNOWN <- c(r$suggested_action, "skip")
bad <- r[!r$action %in% KNOWN, , drop = FALSE]
if (nrow(bad)) {
  print(as.data.frame(count(bad, decision)), right = FALSE)
  stop(nrow(bad), " row(s) carry a decision this script cannot execute. ",
       "Use one of the suggested_action strings, or 'skip'.")
}

seed_rows <- r[grepl("^seed baseline", r$action), , drop = FALSE]
csm_write <- r[grepl("overwrite csm|write the sheet value into csm", r$action), , drop = FALSE]
csm_drop  <- r[grepl("^remove from hydroclimate2k csm", r$action), , drop = FALSE]
flat_drop <- r[grepl("^delete the legacy flat key", r$action), , drop = FALSE]

cat(sprintf("compilation : %s\nmode        : %s\n\n", comp,
            if (commit) "COMMIT" else "dry run"))
cat(sprintf("baseline to seed from the sheet : %d cell%s\n", nrow(seed_rows),
            if (nrow(seed_rows) == 1) "" else "s"))
cat(sprintf("csm values to write             : %d\n", nrow(csm_write)))
cat(sprintf("csm values to remove            : %d\n", nrow(csm_drop)))
cat(sprintf("legacy flat keys to delete      : %d\n", nrow(flat_drop)))

db    <- lv_path("database")
idx   <- lv_db_index(lv_scan(db), cache = TRUE)
store <- qc_store()
run   <- lv_run_id()

# ---- guard: the sheet has not moved since the review -----------------------
#
# The review file records what the sheet said when it was generated. Seeding a
# baseline from a sheet that has since changed would record a value no curator
# ever approved, and the baseline is the thing every future run is measured
# against, so a wrong one is invisible from then on.
cfg <- lv_config(comp)
sheet_now <- qc_sheet_pull(sheet_backend_google(), cfg$qc_sheet_id, cfg$qc_tabs$qc) |>
  filter(field == FIELD, !is.na(value), nzchar(value)) |>
  transmute(tsid, now = trimws(value))
chk <- seed_rows |>
  transmute(tsid, then = trimws(suggested_value)) |>
  left_join(sheet_now, by = "tsid") |>
  filter(is.na(now) | then != now)
if (nrow(chk)) {
  print(as.data.frame(head(chk, 20)), right = FALSE)
  stop(nrow(chk), " certification cell(s) differ between the review file and the sheet. ",
       "Re-run scripts/reconcile-h2k-certification.R --force and review again.")
}

# Not every sheet value is seeded -- 58 of the 142 sit on timeseries the curator
# has excluded, and they are reviewed as such -- so the check above is deliberately
# one-directional. What it cannot see is a cell the sheet has gained since, which
# would be a judgement nobody reviewed.
fresh <- setdiff(sheet_now$tsid, r$tsid)
if (length(fresh)) {
  stop(length(fresh), " certification cell(s) are on the sheet but not in the review file",
       " (e.g. ", paste(utils::head(fresh, 5), collapse = ", "), "). ",
       "Re-run scripts/reconcile-h2k-certification.R --force and review again.")
}
cat(sprintf("sheet check : %d seeded cell%s match the sheet; %d reviewed as not-seeded\n",
            nrow(seed_rows), if (nrow(seed_rows) == 1) "" else "s",
            nrow(sheet_now) - nrow(seed_rows)))

# ---- the file side ---------------------------------------------------------

if (dir.exists(stage)) unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)

cells <- bind_rows(
  csm_write |> transmute(tsid, field = FIELD, value = trimws(suggested_value)),
  # NA removes the key rather than emptying it.
  csm_drop  |> transmute(tsid, field = FIELD, value = NA_character_)) |>
  mutate(present = !is.na(value), dataset_id = NA_character_)

if (nrow(cells)) {
  res <- lv_apply_csm(cells, idx, comp, dir = db, out = stage, progress = FALSE)
  cat(sprintf("csm applied : %d cell%s across %d dataset%s\n", res$n,
              if (res$n == 1) "" else "s", length(res$datasets),
              if (length(res$datasets) == 1) "" else "s"))
  if (nrow(res$issues)) print(as.data.frame(count(tibble::as_tibble(res$issues), check, severity)))
  if (lv_n_issues(res$issues, "error")) stop("csm apply produced errors; not promoting")
}

# The legacy flat key sits on columns with no hydroclimate2k entry to move it
# into -- the curator has marked every one of them inThisCompilation = FALSE --
# so there is no csm target and the key is simply removed. Done here rather than
# through lv_apply_qc(), which only handles fields in the shared namespace.
if (nrow(flat_drop)) {
  ts2ds <- setNames(idx$timeseries$dataSetName, idx$timeseries$TSid)
  paths <- setNames(idx$datasets$path, idx$datasets$fileDataSetName)
  by_ds <- split(flat_drop$tsid, unname(ts2ds[flat_drop$tsid]))
  n_flat <- 0L
  for (dsn in names(by_ds)) {
    src <- if (file.exists(file.path(stage, paste0(dsn, ".lpd"))))
      file.path(stage, paste0(dsn, ".lpd")) else paths[[dsn]]
    L <- lipdR::readLipd(src)
    hit <- by_ds[[dsn]]
    for (pd in seq_along(L$paleoData)) {
      for (tb in seq_along(L$paleoData[[pd]]$measurementTable)) {
        tab <- L$paleoData[[pd]]$measurementTable[[tb]]
        for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
          col <- tab[[cn]]
          if (!is.list(col) || is.null(col$TSid)) next
          if (!as.character(col$TSid)[1] %in% hit) next
          if (is.null(col[[FLAT]])) next
          col[[FLAT]] <- NULL
          n_flat <- n_flat + 1L
          tab[[cn]] <- col
        }
        L$paleoData[[pd]]$measurementTable[[tb]] <- tab
      }
    }
    lipdR::writeLipd(L, path = stage, removeNamesFromLists = TRUE)
  }
  cat(sprintf("flat keys   : %d removed across %d dataset%s\n", n_flat, length(by_ds),
              if (length(by_ds) == 1) "" else "s"))
}

staged <- list.files(stage, "[.]lpd$")
cat(sprintf("staged      : %d file%s\n", length(staged), if (length(staged) == 1) "" else "s"))

# ---- the store side --------------------------------------------------------
#
# The baseline is seeded as events, so it carries the same provenance as any
# other change and can be read back through qc_state_at(). old_present = FALSE:
# the store has never held this field, and saying so is what makes the seed
# distinguishable from a curator edit later.
base <- qc_state_current(store, comp) |> filter(field == FIELD)
if (nrow(base)) stop("the store already holds ", nrow(base), " cell(s) for ", FIELD,
                     "; this seed is a one-off and has already run")

ev <- seed_rows |>
  transmute(run_id = NA_character_, event_seq = NA_integer_, ts = NA_character_,
            compilation = comp, tsid, dataset_id = NA_character_, field = FIELD,
            old_value = NA_character_, old_present = FALSE,
            new_value = trimws(suggested_value), new_present = TRUE,
            source = "sheet", actor = NA_character_,
            reason = "one-off certification baseline seeded from the QC sheet")

if (commit) {
  system2("scripts/snapshot-database.sh", stdout = TRUE)
  if (length(staged)) print(lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = FALSE))
  qc_store_append(store, comp, ev, run_id = run)
  cat(sprintf("store       : appended %d seed event%s (run %s)\n", nrow(ev),
              if (nrow(ev) == 1) "" else "s", run))
} else {
  if (length(staged)) print(lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = TRUE))
  cat(sprintf("store       : would append %d seed event%s\n", nrow(ev),
              if (nrow(ev) == 1) "" else "s"))
  cat(sprintf("staging kept for inspection: %s\n", stage))
}
