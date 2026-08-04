---
name: lipdverse-update
version: 1.0.0
description: |
  Run a LiPDverse database update or ingest with lipdverseUpdater, stopping at
  every gate that needs a human. Proposes vocabulary decisions, duplicate
  dispositions and identity resolutions for review, but never commits a write.
  Use when asked to run, dry-run, or prepare an update for a compilation, or to
  ingest a batch of contributed .lpd files.
allowed-tools:
  - Read
  - Write
  - Edit
  - Bash
  - AskUserQuestion
---

# LiPDverse update runner

You drive `lipdverseUpdater` on Nick's behalf. The pipeline is built so that
every dangerous step is already gated; your job is to run the safe parts, do the
tedious judgement work, and stop cleanly at the gates with something worth
reviewing.

`workflow-example.R` in the repo root is the canonical sequence, kept in step
with `scripts/run-compilation.R`. Read it first. If it disagrees with this file,
it wins, and say so.

## The one rule

**You propose. Nick decides. Nothing you write is self-applying.**

Concretely:

- Write only `proposed_decision`, `proposed_map_to`, `proposed_also_field`,
  `proposed_also_value`, `confidence`, `rationale`. Never `decision`, `map_to`,
  `also_field`, `also_value`.
- `lv_vocab_apply_review()` reads only the `decision` side, so a file full of
  your proposals is inert until Nick runs `lv_vocab_accept()`.
- Never call `lv_vocab_accept()` yourself.
- Never pass `dry_run = FALSE` to anything. Not `lv_promote()`, not
  `lv_vocab_apply_review()`, not `qc_sheet_push()`.
- Never write to a Google Sheet, including the seven vocabulary alignment
  sheets. They are shared and authoritative.
- Never run `scripts/pin-vocabulary.R --commit`.

If a step needs one of those, stop and hand it back with the exact command to
run.

## Before anything

```r
devtools::load_all("~/GitHub/lipdverse-updater")
```

Check `git status` in both `~/GitHub/lipdverse-updater` and
`~/GitHub/lipdverse-qcstore`. Uncommitted changes in the store mean a previous
run was interrupted or is mid-review; report it and ask before proceeding.

Take a database snapshot before any run that could write:
`scripts/snapshot-database.sh`.

## Ingest (stage 1)

Only when there are new contributed files. Follow `workflow-example.R` §1.

Gates, in order:

1. **Validation** (`lv_ingest_validate`). Errors exclude a file. Report the
   count by check, and name the excluded files. Do not repair a file to get it
   past validation unless asked.

2. **Identity** (`lv_ingest_identity`). The rule matters and is easy to get
   backwards: a TSid already in LiPDverse means the file is an *update* if the
   datasetId matches, or if there is no datasetId and the dataSetName matches.
   Otherwise someone used an existing file as a template and the TSid is
   re-minted. Report which datasets are being treated as updates, since that is
   the destructive direction.

3. **Vocabulary** (`lv_ingest_standardize`, then the review file). This is the
   bulk of your work. See below.

4. **Duplicates** (`lv_duplicate_screen`). Report the disposition and the
   recommendation. A match against an existing dataset is not automatically a
   duplicate: it is often a legitimate update. Say which you think it is and
   why.

## Proposing vocabulary decisions

Generate the review file with `lv_vocab_review(std$issues, path)`, then fill in
the proposal columns.

Four decisions:

| `proposed_decision` | when | also needs |
|---|---|---|
| `synonym` | the value means an existing `lipdName` | `proposed_map_to` |
| `new_term` | a real concept the vocabulary lacks | — |
| `decompose` | the value carries two facts at once | `proposed_map_to`, `proposed_also_field`, `proposed_also_value` |
| `leave` | correct as it stands, or undecidable | — |

Rules for proposing:

- **Only propose when confident. Leave the row blank otherwise.** A blank row is
  a useful signal; a wrong high-confidence guess costs more than it saves.
- `proposed_map_to` must already exist as a `lipdName` in that vocabulary, or
  `lv_vocab_apply_review()` will refuse it. Check with
  `lv_vocab()[[field]]$lipdName`. If the right target does not exist, propose
  `new_term` for the target first and explain.
- The `candidates` column is a similarity ranking, not an answer. Verify against
  the vocabulary before proposing.
- **Prefer `decompose` whenever a value carries a season, a depth, a statistic
  or a method alongside the variable.** `MJJASO precip index` is `precipitation`
  plus a seasonality of `MJJASO`. Proposing `synonym` there silently discards
  the season, which is the exact loss this mechanism exists to prevent.
- Set `confidence` to `high` only when the vocabulary itself settles it, e.g.
  proxy `Documents` where `historical` already lists `Documentary` as a synonym.
  Use `medium` for a clear reading that needs a second pair of eyes, and `low`
  for a plausible guess.
- `rationale` is one line, and says what the evidence was, not what the decision
  was. "already a synonym of historical" beats "maps to historical".

Then report: how many rows, how many you proposed, the split by decision type
and confidence, and the ones you deliberately left blank with the reason.

Hand back exactly this:

```r
lv_vocab_accept("<path>", min_confidence = "high")     # dry run, shows what it would take
lv_vocab_accept("<path>", min_confidence = "high", dry_run = FALSE)
lv_vocab_apply_review("<path>")                        # dry run
lv_vocab_apply_review("<path>", dry_run = FALSE)
```

## Update (stage 2)

Follow `workflow-example.R` §2. Run it through to the dry-run verification and
stop.

Things to look at rather than skip past:

- **Sheet-level consistency** (§2b). Dataset-level fields that disagree across
  rows of one dataset. Two rows here is normal; several hundred means the check
  is scoped wrong, so say so rather than reporting them all.
- **Conflicts in the plan.** Never resolve a `shared`-ownership conflict on your
  own. Report the base, sheet and file values and let Nick choose.
- **The changelog** (§2f). It reads and rewrites every staged file, so it is the
  slow stage. Do not skip it: promoting without it loses the attribution.
- **The version** (§2h). B ticks when the dataset set changes, C when only
  metadata changes, A only on publication. If B ticked, say which datasets
  entered or left.

Stop at §2i and report the receipt: files to replace, to add, to delete, and the
version transition. `lv_promote(dry_run = FALSE)` is Nick's to run.

## Reporting

Lead with what needs a decision, then what you did, then what you skipped. Give
counts with denominators. If a stage produced a report file, name its path.

Do not claim a stage passed if you did not run it. Do not describe a dry run as
if it wrote anything.
