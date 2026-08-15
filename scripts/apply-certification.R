#!/usr/bin/env Rscript
#
# Execute the reviewed dispositions in review/<compilation>-certification.csv.
#
#   ./scripts/apply-certification.R iso2k            # dry run, writes nothing
#   ./scripts/apply-certification.R iso2k --commit   # rewrite and promote
#
# Generalised from the hydroclimate2k version. That one also seeded a store
# baseline, because hydroclimate2k had none and its sheet was the only clean
# record. A compilation seeded from its published qcTs already has a baseline,
# so this touches the files only.
#
# Every row's disposition comes from the review file, never from this script. A
# blank `decision` accepts `suggested_action`; anything else must be one of the
# words below, and an unrecognised decision aborts rather than being guessed at.
#
#   keep    leave the file as it is
#   skip    same, said differently
#
# What the actions do, all inside inCompilation[<compilation>].csm:
#
#   strip the appended value   write suggested_value (what the sheet and the
#                              baseline agree on) over the concatenated one
#   write the sheet value      set it where the file has none
#   remove from csm            drop the key
#   delete the legacy flat key drop the bare column key the migration left
#   nothing to do / leave      no write

suppressPackageStartupMessages({library(dplyr)})
suppressMessages(devtools::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
comp   <- args[!grepl("^--", args)][1]
commit <- "--commit" %in% args
if (is.na(comp)) stop("usage: apply-certification.R <compilation> [--commit]")

review <- file.path("review", paste0(comp, "-certification.csv"))
if (!file.exists(review)) stop("no review file at ", review,
                               "; run scripts/reconcile-certification.R ", comp, " first")

r <- readr::read_csv(review, col_types = readr::cols(.default = readr::col_character()),
                     progress = FALSE)
r$action <- ifelse(is.na(r$decision) | !nzchar(trimws(r$decision)),
                   r$suggested_action, trimws(r$decision))
KNOWN <- c(r$suggested_action, "keep", "skip")
bad <- r[!r$action %in% KNOWN, , drop = FALSE]
if (nrow(bad)) {
  print(as.data.frame(count(bad, decision)), right = FALSE)
  stop(nrow(bad), " row(s) carry a decision this script cannot execute.")
}

set   <- r[grepl("^strip the appended|^write the sheet value", r$action), , drop = FALSE]
drop  <- r[grepl("^remove from csm", r$action), , drop = FALSE]
flat  <- r[grepl("^delete the legacy flat key", r$action), , drop = FALSE]
kept  <- r[r$action %in% c("keep", "skip"), , drop = FALSE]

cfg <- lv_config(comp)
db  <- lv_path("database")
idx <- lv_db_index(lv_scan(db), cache = TRUE)
run <- lv_run_id()
stage <- path.expand(file.path("~/lipdverse-staging", paste0("cert-", comp)))

cat(sprintf("compilation : %s\nmode        : %s\n\n", comp, if (commit) "COMMIT" else "dry run"))
cat(sprintf("csm values to set    : %d\ncsm values to remove : %d\nflat keys to delete  : %d\nleft alone by review : %d\n",
            nrow(set), nrow(drop), nrow(flat), nrow(kept)))

# The sheet must still say what the review file recorded. A value corrected on
# the sheet since would otherwise be overwritten by a stale suggestion, and the
# whole point of stripping is that the sheet is the authority.
fields <- lv_csm_fields(comp)
FIELD <- fields$qc_name[fields$csm_field == "QCCertification"]
now <- qc_sheet_pull(sheet_backend_google(), cfg$qc_sheet_id, cfg$qc_tabs$qc) |>
  filter(field == FIELD, !is.na(value), nzchar(value)) |>
  transmute(tsid, now = trimws(value))
chk <- set |> transmute(tsid, then = trimws(suggested_value)) |>
  left_join(now, by = "tsid") |> filter(is.na(now) | then != now)
if (nrow(chk)) {
  print(as.data.frame(utils::head(chk, 10)), right = FALSE)
  stop(nrow(chk), " cell(s) differ between the review file and the sheet; re-run reconcile-certification.R")
}
cat("sheet check : the values this would write still match the sheet\n")

cells <- bind_rows(
  set  |> transmute(tsid, field = FIELD, value = trimws(suggested_value)),
  drop |> transmute(tsid, field = FIELD, value = NA_character_)) |>
  mutate(present = !is.na(value), dataset_id = NA_character_)

if (dir.exists(stage)) unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)

if (nrow(cells)) {
  res <- lv_apply_csm(cells, idx, comp, dir = db, out = stage, progress = FALSE)
  cat(sprintf("csm applied : %d cell%s across %d dataset%s\n", res$n,
              if (res$n == 1) "" else "s", length(res$datasets),
              if (length(res$datasets) == 1) "" else "s"))
  if (nrow(res$issues)) print(as.data.frame(count(tibble::as_tibble(res$issues), check, severity)))
  if (lv_n_issues(res$issues, "error")) stop("csm apply produced errors; not promoting")
}

# The legacy key sits on the column itself rather than in csm, so lv_apply_qc()
# does not reach it: that path handles the shared namespace only.
if (nrow(flat)) {
  FLAT <- sub("^paleoData_", "", FIELD)
  ts2ds <- setNames(idx$timeseries$dataSetName, idx$timeseries$TSid)
  paths <- setNames(idx$datasets$path, idx$datasets$fileDataSetName)
  by_ds <- split(flat$tsid, unname(ts2ds[flat$tsid]))
  n_flat <- 0L
  for (dsn in names(by_ds)) {
    src <- if (file.exists(file.path(stage, paste0(dsn, ".lpd")))) file.path(stage, paste0(dsn, ".lpd"))
           else paths[[dsn]]
    if (is.null(src) || is.na(src)) next
    L <- lipdR::readLipd(src)
    for (pd in seq_along(L$paleoData)) for (tb in seq_along(L$paleoData[[pd]]$measurementTable)) {
      tab <- L$paleoData[[pd]]$measurementTable[[tb]]
      for (cn in setdiff(names(tab), c("tableName", "filename", "missingValue"))) {
        col <- tab[[cn]]
        if (!is.list(col) || is.null(col$TSid)) next
        if (!as.character(col$TSid)[1] %in% by_ds[[dsn]]) next
        if (is.null(col[[FLAT]])) next
        col[[FLAT]] <- NULL; n_flat <- n_flat + 1L; tab[[cn]] <- col
      }
      L$paleoData[[pd]]$measurementTable[[tb]] <- tab
    }
    lipdR::writeLipd(L, path = stage, removeNamesFromLists = TRUE)
  }
  cat(sprintf("flat keys   : %d removed across %d dataset%s\n", n_flat, length(by_ds),
              if (length(by_ds) == 1) "" else "s"))
}

staged <- list.files(stage, "[.]lpd$")
cat(sprintf("staged      : %d file%s\n", length(staged), if (length(staged) == 1) "" else "s"))
if (!length(staged)) { cat("nothing to promote\n"); quit(save = "no") }

if (commit) system2("scripts/snapshot-database.sh", stdout = TRUE)
print(lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = !commit))
if (!commit) cat(sprintf("\nstaging kept for inspection: %s\n", stage))
