# =============================================================================
# LiPDverse update workflow, step by step
# =============================================================================
#
# A template for driving lipdverse-updater from an interactive R session, using
# hydroclimate2k as the example. The scripts in scripts/ do all of this in one
# go; this exists so you can stop between stages and look at what is happening.
#
# NOTHING HERE WRITES ANYTHING until you reach the sections marked COMMIT, and
# those are wrapped in `if (FALSE)` so they cannot run by accident. Read a
# section, run it, look at the object it leaves behind, then move on.
#
# Two pipelines. Run them in this order when both apply:
#
#   1. INGEST  bring newly contributed .lpd files into the database
#   2. UPDATE  merge the QC sheet with the files and write the result back
#
# =============================================================================

library(dplyr)
devtools::load_all("~/GitHub/lipdverse-updater")

comp <- "hydroclimate2k"
cfg  <- lv_config(comp)       # sheet id, tab names, database directory, strictness
db   <- lv_path("database")
store <- qc_store()           # the append-only QC event log, in lipdverse-qcstore
bk   <- sheet_backend_google()
run  <- lv_run_id()           # one id for everything this session writes

cfg


# =============================================================================
# 0. The database index
# =============================================================================
# Reads only the metadata JSON out of each .lpd, so the whole 7,177-file
# database takes about 20 seconds rather than the many minutes a full
# readLipd() would. Cached by file md5, so it is fast on repeat.
#
# Everything downstream needs it, so build it once and pass it around.

idx <- lv_db_index(lv_scan(db), cache = TRUE)
idx

# Identity is checked before anything else. Duplicate TSids, duplicate
# datasetIds and duplicate names are errors, not things to auto-rename.
lv_validate_identity(idx)


# =============================================================================
# 1. INGEST — only if you have new files to bring in
# =============================================================================
# Skip to section 2 if you are just running an update.
#
# Contributed files are not LiPDverse-ready: most have no datasetId, many have
# no TSids, and some carry identifiers copied from whatever file was used as a
# template. Each stage below reports before anything is written.

incoming <- path.expand("~/lipdverse-staging/incoming-hydroclimate2k")

## 1a. Names and required metadata -------------------------------------------
# Errors exclude a file from the batch; warnings and info do not. Severity is
# calibrated against the database: Site.Author.YYYY covers only 82% of it, so a
# non-conforming name warns, but every dataset has an archiveType and
# coordinates, so their absence is an error.

val <- lv_ingest_validate(incoming, idx)
count(val, severity, check, sort = TRUE)
blocked <- unique(val$path[val$severity == "error"])
blocked

## 1b. Identity ---------------------------------------------------------------
# The rule that matters: a TSid already in LiPDverse means one of two opposite
# things. If the file IS that dataset -- matching datasetId, or no datasetId yet
# and a matching dataSetName -- it is an update and the TSid is kept, so the
# timeseries keeps its compilation memberships. Otherwise someone used an
# existing file as a template, and it is re-minted.

scan <- lv_ingest_scan(incoming)
scan <- scan[!scan$file %in% blocked, ]
plan_id <- lv_ingest_identity(scan, idx)
count(plan_id, action, reason, sort = TRUE)

# Which datasets are being treated as updates rather than new records:
unique(plan_id$dataSetName[grepl("^update", plan_id$reason)])

## 1c. Write the resolved identity to staging ---------------------------------
# Never in place. Each staged file is re-read afterwards and checked for its
# columns, its data file, its measured values and validLipd. A file that loses
# anything is deleted from staging and reported rather than promoted.

stage_id <- path.expand("~/lipdverse-staging/wf-identity")
res <- lv_ingest_apply(plan_id, incoming, stage_id, idx)
res$staged |> length()
res$skipped
res$issues |> as.data.frame()

## 1d. Standardize vocabulary -------------------------------------------------
# Six fields, against the pinned tables in inst/extdata/vocab -- never fetched
# from the network, so a run is reproducible. Unmatched values are reported and
# left exactly as they are rather than guessed at.
#
# This walks the LiPD object directly. Do not be tempted by lipdverseR's
# standardizeLipdBatch(): it round-trips through as.lipdTsTibble()/as.lipd(),
# which invents an empty interpretation on every column.

stage_std <- path.expand("~/lipdverse-staging/wf-standardized")
std <- lv_ingest_standardize(stage_id, stage_std)
std$pin                                  # which vocabulary version was used
count(std$changes, field, rule, sort = TRUE)
count(as_tibble(std$issues), field, sort = TRUE)   # unmatched, for a curator

## 1d-bis. Curator decisions on unrecognised vocabulary ----------------------
# std$issues lists values the vocabulary does not recognise. They are left
# exactly as they are rather than guessed at, so somebody has to decide. This is
# where that happens, and the decision is recorded once and never asked again.
#
# The seven alignment Google Sheets are the source of truth for the vocabulary
# (standardTables.RDS is built from them; see the "LiPD-PaST alignment
# directory"). Nothing here writes to them. A decision takes effect locally at
# once, and is also emitted as a patch file in that sheet's own schema, to be
# appended upstream deliberately and in one batch.

# Next to the batch it describes, rather than in the state directory. Every
# stage globs *.lpd explicitly, so a CSV here is inert: promote, scan and the
# value hashes all ignore it, and the review travels with the batch.
# Submissions arrive as a directory per dataset, usually with the paper beside
# the .lpd. Ingest flattens them, losing that link; lv_ingest_sources() recovers
# it so each review row can point at the paper that answers it. Values like HHI
# are author shorthand that no vocabulary will ever contain.
src <- lv_ingest_sources(incoming_download)   # the tree as downloaded, not `incoming`

rev <- fs::path(stage_std, "vocab-review.json")
lv_vocab_review(std$issues, rev, sources = src)

# The review also carries past_candidates: the top PaST thesaurus matches for
# each value, from lv_past(). Those matter for `new_term`, since the alignment
# sheets carry pastName and pastId alongside every term.
lv_past_match("Palmer Hydrological Drought Index", n = 5)

# Review it in the browser rather than the spreadsheet. Values are grouped by
# the decision they would take, so the 87 rows in the h2k batch are 12 groups,
# and the papers open with a click. It writes the decision columns and nothing
# else; applying stays a separate step.
if (FALSE) lv_vocab_review_app(rev)

# Or open `rev` directly and fill in the `decision` column. Four options:
#
# (The file also has a `proposed_*` set of columns, plus confidence and
# rationale. Those are for the Claude-driven path -- `scripts/lv-claude`, which
# runs the same stages and fills in proposals. lv_vocab_apply_review() reads
# only the `decision` side, so proposals sit inert until lv_vocab_accept()
# copies the ones you want across. Working by hand, ignore them.)
#
#   synonym    the value means an existing term. Give map_to. Overlaid onto the
#              vocabulary, so it matches from now on.
#   new_term   a legitimate term the vocabulary lacks. Becomes canonical locally
#              and is proposed upstream.
#   decompose  the value carries two facts at once. `MJJASO precip index` is
#              precipitation AND a seasonality of MJJASO. Give map_to plus
#              also_field and also_value. Deliberately not a synonym: a synonym
#              would rewrite the name and drop the season.
#   leave      correct as it stands, or not decidable yet. Recorded so it is not
#              offered again.
#
# `candidates` ranks the vocabulary by similarity, preferring word-stem and
# containment matches over raw edit distance, which is what surfaces
# `precipitation` for the compound precip-index names.

if (FALSE) {                     # once you have filled it in
  lv_vocab_apply_review(rev)                      # dry run: what would be recorded
  lv_vocab_apply_review(rev, dry_run = FALSE)     # record, and write upstream patches
}

# Then standardize again, with the decisions in force. lv_ingest_standardize()
# reads the overlay by default, so this needs no argument.
if (FALSE) {
  std <- lv_ingest_standardize(stage_id, stage_std)
  count(std$changes, rule)       # `decompose` rows write two fields per value
}

## 1e. Duplicate screen -------------------------------------------------------
# Metadata cannot tell "the same record under a different name" from "a
# different record at the same site" -- pollen datasets all share variable
# names, and one site has many cores. Hashing the non-axis measurement columns
# can. About a minute for the whole database, cached.

dbh <- lv_value_hashes(db, cache = fs::path(lv_path("state"), "cache", "value-hashes.rds"))
inh <- lv_value_hashes(stage_std)
dup <- lv_duplicate_screen(inh, dbh, idx)
dup |> select(new, existing, shared, containment, disposition) |> as.data.frame()
dup$recommendation                       # what to hand back to the group

## 1f. COMMIT the ingest ------------------------------------------------------
if (FALSE) {
  system2("scripts/snapshot-database.sh")          # snapshot first, always
  lv_promote(stage_std, db, run_id = run, partial = TRUE, dry_run = FALSE)

  # Membership is NOT written into the files. New datasets go into
  # datasetsInCompilation as TRUE; their timeseries then appear in the QC tab
  # with inThisCompilation = FALSE and you admit them there.
  tab <- sheet_read(bk, cfg$qc_sheet_id, cfg$qc_tabs$datasets)
  add <- setdiff(sub("\\.lpd$", "", list.files(stage_std, "[.]lpd$")), tab$dsn)
  add
}


# =============================================================================
# 2. UPDATE — merge the QC sheet with the files
# =============================================================================

## 2a. Scope ------------------------------------------------------------------
# Two different sets, and conflating them breaks the mechanism:
#
#   considered  datasets not excluded in datasetsInCompilation. Every timeseries
#               of these appears in the QC tab -- that is how a candidate
#               becomes visible to a lead.
#   members     timeseries actually carrying an inCompilation entry. This is the
#               compilation.

mem <- lv_compilation_datasets(cfg, bk, idx)
ds  <- mem$datasets
ts  <- idx$timeseries$TSid[idx$timeseries$dataSetName %in% ds]
members <- lv_compilation_timeseries(idx, comp)

length(ds); length(ts); length(members)
mem$missing                              # named by the sheet, absent from the database

## 2b. The three inputs to the merge ------------------------------------------
# base  what the store last recorded    (the baseline)
# sheet what the QC sheet says now      (curator edits)
# frame what the files say now
#
# If base is empty the compilation has never run under this system. Seed it
# first with scripts/seed-baseline.R, choosing --from by which side has moved:
# see review/compilation-staleness.csv.

base  <- qc_state_current(store, comp)
sheet <- qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc)
frame <- qc_frame(db, datasets = ds)
frame <- frame[frame$tsid %in% ts, ]

# Membership is not stored as a field, so the file-side view is derived.
frame <- bind_rows(frame, lv_membership_frame(idx, comp, ts))

nrow(base); nrow(sheet); nrow(frame)

# A dataset-level field repeats across every row of its dataset in the sheet, so
# those rows should agree. Where they do not, one of them is wrong: CO07CAFR
# carries two pub1_citation values differing by an en-dash mangled into three
# characters. The merge cannot see this -- only the row that differs from the
# baseline becomes a change, so by the time cells reach lv_apply_qc there is a
# single value and nothing to compare against.
#
# Scoped by the registry's cardinality, not by where the field is stored. Those
# are different questions: minYear sits at the dataset root but varies per
# timeseries, and going by storage location flags 438 fields, nearly all of them
# legitimately varying.
reg <- lv_qc_fields()
ds_level <- reg$qc_name[reg$cardinality %in% "dataset"]
sd <- sheet[sheet$field %in% ds_level & !is.na(sheet$value), ]
sd$dataSetName <- unname(setNames(idx$timeseries$dataSetName, idx$timeseries$TSid)[sd$tsid])
sd |>
  filter(!is.na(dataSetName)) |>
  group_by(dataSetName, field) |>
  summarise(n = n_distinct(value), .groups = "drop") |>
  filter(n > 1)

## 2c. Merge ------------------------------------------------------------------
# Per cell: if only the sheet moved, the curator edited it; if only the files
# moved, the files did; if both moved the same way, converged; if both moved
# differently, ownership decides -- curator takes the sheet, machine takes the
# files, shared is a real conflict and base is retained.
#
# A blank never deletes unless the curator owns the field and may clear it.
# That single rule is what makes the old NA-as-deletion loss impossible.

plan <- qc_merge(base, sheet, frame)
plan                                     # summary
count(as_tibble(plan$changes), field, resolution, sort = TRUE) |> head(15)

# Aborts on an unresolved conflict under cfg$strict, after writing the report.
qc_plan_check(plan, path = file.path(lv_run_dir(run), "conflicts.csv"))

state <- qc_plan_state(plan)             # the resolved state

## 2d. What would be written to the files -------------------------------------
# Only cells the sheet moved. "file" and "converged" mean the file already holds
# the value. inThisCompilation is handled separately, as structure.

write_cells <- plan$cells[plan$cells$resolution == "sheet" &
                          plan$cells$field != "inThisCompilation", ]
nrow(write_cells)
count(as_tibble(write_cells), field, sort = TRUE) |> head(10)

# A curator can type anything into a cell. Quarantine values the files cannot
# hold rather than letting one abort a several-hundred-file promote.
bad <- lv_validate_values(write_cells)
as.data.frame(bad)
write_cells <- anti_join(write_cells, bad[, c("TSid", "field")],
                         by = c("tsid" = "TSid", "field" = "field"))

## 2e. Apply to staging -------------------------------------------------------

stage <- path.expand("~/lipdverse-staging/wf-update")
if (fs::dir_exists(stage)) fs::dir_delete(stage)
fs::dir_create(stage)

# Membership first, from the merged plan so an admission leaves the same events
# as any other curator edit.
mplan <- plan$cells[plan$cells$field == "inThisCompilation" &
                    plan$cells$resolution %in% c("sheet", "converged"), ]
mres <- lv_apply_membership(mplan, idx, comp, dir = db, out = stage)
length(mres$added); length(mres$removed)

# Then the rest. Point the index at staging for datasets membership already
# rewrote, so the two stages compose into one file.
aidx <- idx
if (length(mres$datasets)) {
  hit <- match(mres$datasets, aidx$datasets$fileDataSetName)
  aidx$datasets$path[hit] <- fs::path(stage, paste0(mres$datasets, ".lpd"))
}
iss <- lv_apply_qc(write_cells, db, stage, index = aidx)
as.data.frame(iss)

# Invariant: nothing outside the compilation was touched.
setdiff(sub("\\.lpd$", "", list.files(stage, "[.]lpd$")), ds)

## 2f. Changelog --------------------------------------------------------------
# Per dataset, comparing staged against live. Each entry carries the compilation
# and run_id, which createChangelog() never recorded -- 56% of datasets belong to
# two or more compilations and they share the fields stored in the file, so an
# entry saying only what changed cannot say which run did it.
#
# Writes the entries into the staged files, so promoting carries them along.

for (f in list.files(stage, "[.]lpd$")) {
  live <- fs::path(db, f)
  if (!fs::file_exists(live)) next
  a <- suppressWarnings(lipdR::readLipd(live))
  b <- suppressWarnings(lipdR::readLipd(fs::path(stage, f)))
  d <- lv_changelog_diff(a, b)
  if (!nrow(d)) next
  v <- lv_changelog_next_version(lv_changelog_last_version(b))
  b <- lv_changelog_append(b, lv_changelog_entry(
    d, version = v, last_version = lv_changelog_last_version(b),
    compilation = comp, run_id = run))
  lipdR::writeLipd(b, path = stage, removeNamesFromLists = TRUE)
}

# What one dataset's diff looks like, before it is rendered into an entry:
lv_changelog_diff(
  suppressWarnings(lipdR::readLipd(fs::path(db, list.files(stage, "[.]lpd$")[1]))),
  suppressWarnings(lipdR::readLipd(fs::path(stage, list.files(stage, "[.]lpd$")[1]))))

## 2g. Verify without writing -------------------------------------------------
# Re-reads every staged file, runs validLipd, checks names and sizes. This is
# the gate; a dry run tells you exactly what a commit would do.

rec <- lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = TRUE)
rec

## 2h. Version ----------------------------------------------------------------
# A_B_C: A ticks on publication, B when the dataset set changes (resetting C),
# C when metadata changes. Take the membership this run WOULD produce, from the
# plan -- re-reading the database on a dry run returns what you started with and
# makes every run look like a metadata change.

prev <- lv_version_current(store, comp)
members_now <- union(setdiff(members, mres$removed), mres$added)
ds_now <- unique(idx$timeseries$dataSetName[idx$timeseries$TSid %in% members_now])

vfile <- fs::path(store$path, "version_datasets.csv")
ds_before <- if (!is.null(prev) && fs::file_exists(vfile)) {
  v <- readr::read_csv(vfile, col_types = readr::cols(.default = "c"))
  v$dataset[v$compilation == comp & v$version == prev]
} else ds_now

ver <- lv_tick_version(prev, ds_before, ds_now)
ver

## 2i. COMMIT the update ------------------------------------------------------
# Snapshot, write, record. lv_promote moves the originals to .trash rather than
# deleting, so lv_write_rollback(run_id = run) undoes it.
if (FALSE) {
  system2("scripts/snapshot-database.sh")

  lv_promote(stage, db, run_id = run, partial = TRUE, dry_run = FALSE)

  qc_store_append(store, comp, qc_diff_to_events(base, state, source = "sheet"),
                  run_id = run)

  lv_version_append(store, comp, ver, run_id = run,
                    db_fingerprint = lv_scan(db)$fingerprint)

  # Push the sheet. Scoped to considered datasets so a de-selected dataset drops
  # out of the tab. Uses range_write, which preserves the tab's colour coding --
  # write_sheet() would clear it.
  push_state <- state[state$tsid %in% ts, ]
  qc_sheet_push(push_state, bk, cfg$qc_sheet_id, cfg$qc_tabs$qc,
                mode = "full", dry_run = FALSE)

  # Optional: recolour the header by thematic group.
  qc_sheet_colour_groups(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc, dry_run = FALSE)
}


# =============================================================================
# 3. Check it settled
# =============================================================================
# The invariant worth trusting: a second run against unchanged inputs changes
# nothing. It has caught more real bugs on this project than any diff.

if (FALSE) {
  base2  <- qc_state_current(store, comp)
  frame2 <- qc_frame(db, datasets = ds)
  frame2 <- bind_rows(frame2[frame2$tsid %in% ts, ],
                      lv_membership_frame(idx, comp, ts))
  plan2  <- qc_merge(base2, qc_sheet_pull(bk, cfg$qc_sheet_id, cfg$qc_tabs$qc), frame2)
  plan2$summary$n_changed          # expect 0
}


# =============================================================================
# If something went wrong
# =============================================================================
#   lv_write_rollback(run_id = run)             restore the files this run wrote
#   scripts/restore-from-snapshot.sh            restore the whole database
#   git -C ~/GitHub/lipdverse-qcstore log       the QC event log is versioned
#
# Reports for every run, whether it succeeded or not:
#   lv_run_dir(run)
