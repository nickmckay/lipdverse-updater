# Review artifacts

Working documents generated for human review. Edit them and hand them back;
they are inputs to the field registry, not outputs of it.

## terms-draft.csv

255 terms: every column appearing in any compilation's QC sheet (198), plus
every canonical field name reachable from one (57 more), with a proposed
classification and the evidence behind it.

Regenerate with `../scripts/draft-term-registry.R --out=terms-draft.csv`.
Reads only the local QC store; no network.

### Canonical vs alias

The convo sheet is already the field-name synonym table. 82 of its 231 rows
have `qcSheetName != tsName`, meaning the QC sheet column is a display or short
name for a differently-named canonical field:

```
lat             -> geo_latitude
basis           -> climateInterpretation1_basis
QC Certification-> paleoData_QCCertification
countryOcean    -> geo_gcmdLocation
```

57 of those aliases are live in a QC sheet today. Each gets a `synonym` row
pointing at its canonical term, and the canonical gets its own row.

Usage is attributed to the **canonical**, not the alias: `geo_latitude` never
appears as a QC column, but `lat` appears in every compilation, so
`geo_latitude` is correctly scored as ubiquitous. A synonym row's own usage is
in `alias_n_filled` / `alias_compilations`, and a canonical row lists its
aliases in `aliases`.

### How to review

Fill in **`decision`**. Leave it blank to accept `suggested_category`. The
sort order puts the least certain rows first, so a top-down pass hits the real
decisions early and the long tail is mostly confirmation.

Rows are grouped by **`family`**, which collapses index digits
(`climateInterpretation1_basis`, `climateInterpretation2_basis`, … →
`climateInterpretation*_basis`). Decide a family once; 198 terms are only
~90 families.

### Categories

| category | meaning |
|---|---|
| `standard` | Belongs in the LiPDverse standard. One canonical value per dataset, shared by every compilation containing it. |
| `compilation` | Compilation-specific: either a field only one compilation wants, or a standard field that may legitimately differ between compilations. |
| `synonym` | A variant or alias of another term. Put the canonical term in `canonical_if_synonym`. |
| `control` | An instruction column consumed by the pipeline, not dataset metadata. |
| `unused` | The column exists in a sheet but is never populated anywhere. Candidate for deletion. |
| `key` | Identifier. Never merged. |
| `review` | Evidence is genuinely ambiguous. |

For anything you mark `compilation`, `suggested_scope` is worth a look too:
whether the field should be stored per-compilation in the `.lpd` (namespaced
like the existing `inCompilationBeta<N>_*` blocks) or kept out of the standard
corpus entirely.

### How the proposals were derived

Ordered rules, first match wins. Deliberately conservative — anything not
matching a clear pattern goes to `review` rather than being guessed.

1. `TSid` / `dataSetName` / `datasetId` → `key`
2. `changelogNotes` / `standardizationNotes` / `instructions` → `control`
3. convo maps it to a differently-named canonical field → `synonym`
4. Heuristically normalises to an existing convo term → `synonym`
5. Never populated in any compilation → `unused`
6. Used by ≥50% of compilations, or in convo and used by ≥4 → `standard`
7. Term name contains a compilation's name (`iso2kUI`) → `compilation`
8. Used by ≤2 compilations → `compilation`
9. Otherwise → `review`

Rule 4 (the heuristic, as opposed to rule 3's recorded mapping) only strips
index digits, never the semantic prefix: `climateInterpretation2_*` and
`environmentInterpretation1_*` are different scopes and must not collapse
together. It is also never applied to a canonical term, or `geo_latitude`
would be declared an alias of its own alias `lat`.

### Evidence columns

| column | meaning |
|---|---|
| `in_convo`, `ts_name`, `convo_type`, `same_across_dataset` | From the convo sheet's QC-column → timeseries-field mapping (231 rows). |
| `n_compilations`, `ubiquity`, `compilations` | How many sheets carry the column, and which. |
| `n_filled`, `fill_rate`, `max_distinct` | How much real data it holds. Low `max_distinct` against high `n_filled` suggests a controlled vocabulary. |
| `n_shared_tsids` | TSids for which 2+ compilations both populate this term — the only rows where a conflict is possible. |
| `n_conflict`, `n_conflict_subst`, `conflict_rate` | How often those disagree. `_subst` excludes pure case/punctuation drift (`coral` vs `Coral`). |
| `example_values` | Up to four observed values. |

`n_conflict_subst > 0` on a term proposed as `standard` is the interesting
signal: two compilations are actively disagreeing about a field meant to have
one canonical value. Those are flagged in `note`.

### Caveat

Conflict counts mix genuine disagreement with staleness. 92% of the 6,084
current conflicts involve a compilation that has not run in over three years
(HoloceneAbruptChange last ran 2020-10-08), whose sheet is simply a stale view.
Only 376 conflicts involve compilations that both ran within 14 months, and
only 62 of those are substantive — all CoralHydro2k vs hydroclimate2k, and
almost entirely the short-name/standard-name boundary
(`SrCa_annual` vs `Sr/Ca`, `d18O_sw` vs `d18O`). Use
`../scripts/report-cross-compilation-conflicts.R` for the per-cell detail.

## Scope note: chronData is deferred

LiPDverse updates have only ever touched `paleoData`. Chronology keys therefore
need no merge rule, no ownership classification and no csm handling until that
changes.

This affects two files:

- `key-gap-not-in-sheet.csv` — 93 of its 148 keys are `chronData_*` and can be
  ignored for now, leaving 55 that matter.
- `qc-field-ownership.csv` — unaffected, since it is built from QC sheet columns
  and those are already paleo-only.

chronData will matter again for the canonical export schema, which has to
describe the whole file rather than just the part the pipeline edits.

## key-decisions-addendum.csv

Five rows to paste into the `lipd key standardization` sheet, in its own column
format. They came out of the GBRCD inventory (GBRCD reads its own database
directory and had never been scanned).

- `pub_altDataUrl` → `pub_altDataURL`. The two differ only in case; the
  minority spelling (14 occurrences) folds into the majority (275). Both are
  GBRCD-only — the main database has no `altDataUrl` key at all.
- `hasIGSN`, `pub_doiData`, `pub_altDataURL` and
  `paleoData_uncertaintyAnalyticalUnits` are declared standard rather than
  compilation-specific, so they are self-mapping canonical entries.

Two candidates were dropped because the sheet already covers them, and adding
them would have contradicted existing decisions:

- `paleoData_core` is already recorded as a synonym of `paleoData_coreName`.
- `calibration_targetDataset` is already canonical, with two synonyms
  (`calibration_dataset`, `calibration_target`) mapping to it.

## csm-migration-orphans-*.csv

Cells the csm migration could not place, because the column carries the
metadata but has no `inCompilation` entry for the target compilation. Attaching
csm there would assert a membership the file does not record, so the key is
left where it is and reported.

| source | orphans | note |
|---|---|---|
| `database` | 6,974 | 6,423 on columns with **no compilation membership at all** |
| `GBRCD` | 2,615 | all of them on columns with no membership |

**Resolved during review:** GBRCD's coral records carried CoralHydro2k's
vocabulary (`jcpUsed`, `coralExtensionRate`, `coralExtensionRateNotes`,
`jcpMeasured`, `coralTissueThickness`) without being members of that
compilation. Those five terms are now GBRCD compilation-specific terms as well,
using the same field names, which placed a further 2,966 values and dropped
GBRCD's orphans from 5,581 to 2,615.

Everything still orphaned sits on a column that records **no compilation
membership whatsoever** — 1,707 `temp12kDkIndex`, 1,099
`iso2kPrimaryTimeseries`, 553 `QCCertification` and so on. Those keys stay flat
for now; there is nowhere to put them until membership is established.

## Migration verification

Source against migrated, over a 300-dataset sample:

- **`differs`: 0.** No value was changed by the migration.
- Moved keys balance exactly — 463 `paleoData.SISALEntityID` out, 463
  `inCompilation[1].csm.SISALEntityID` in.
- The remaining diff is entirely lipdR round-trip behaviour, not migration:
  the `inCompilationBeta` → `inCompilation` rename (intended, applied on read
  since 0.7.0), and ensemble tables' `number` field expanding into an indexed
  list. A plain `readLipd`/`writeLipd` with no migration logic reproduces the
  latter exactly, on 12 of the 300 sampled datasets.

## test-compilation.csv

The TSid list for a standing test compilation: a health check meant to be run
regularly, chosen for coverage rather than size.

**161 datasets, 2,824 TSids, 56 MB.** Every paleo column of each chosen dataset
is included — a partial dataset would make the membership tab and the files
disagree in a way no real compilation does.

Selection is deterministic (fixed seed) so the list is stable across
regenerations, and each row records why its dataset was picked. Regenerate with
`../scripts/select-test-compilation.R`.

Quotas are taken per condition rather than sampled at random, so a rare
condition cannot be missed:

| condition | datasets |
|---|---|
| all 17 archive types | 161 |
| interpretations | 161 |
| carries compilation-specific metadata | 160 |
| calibration metadata | 123 |
| chron data | 100 |
| in 2–3 compilations | 63 |
| in exactly 1 | 62 |
| ensemble tables | 24 |
| cross-compilation conflicts | 23 |
| in 4+ compilations | 21 |
| in no compilation at all | 15 |
| ≤3 columns | 28 |
| ≥100 columns | 8 |
| non-alphanumeric names | 7 |
| the reported incident records | 2 (`LS12THAY`, `LS14FEZA`) |

Three detectors were wrong on the first pass and are worth knowing about:

- `LS12THAY` and `LS14FEZA` are **record prefixes, not TSids** — the actual
  identifiers are `LS12THAY01E`, `LS12THAY01B` and so on. Matching exactly found
  nothing.
- Ensembles are invisible in the key inventory, because an ensemble column looks
  like any other column. They are detected by scanning the jsonld of the 900
  largest files for `ensembleTable`.
- No dataset has a single column; the minimum is 2 (a value and its time axis),
  so a "single column" quota could never match.

## promote-dryrun-failures.csv

`lv_promote(dry_run = TRUE)` over all 7,177 migrated files: **7,163 pass, 14
fail**, so the promotion is blocked until these are resolved.

All 14 fail the same way, and **the migration is not the cause**. Each is valid
in the source database and invalid after, but a plain `readLipd()` →
`writeLipd()` with no migration logic reproduces it exactly.

**Mechanism.** An all-`NaN` numeric column loses its type on a lipdR round trip:

```
source, after readLipd :  depth  class=numeric  n=1422  NaN,NaN,NaN
after readLipd/writeLipd: depth  class=logical  n=1422  NA,NA,NA
```

`writeLipd()` emits `NaN` to the CSV; on re-read an all-`NaN` column is typed
logical, and `validLipd()` then rejects the file with "depth values are not
numeric". Every one of the 14 has at least one all-`NaN` numeric column —
`SP05CRBR` has 18 of them.

This is the reverse of the bug fixed in lipdR 0.7.0, where string and logical
columns were written as `NaN`. The values are not lost (they were already
`NaN`), but the column type is, and that is enough to make the file invalid.

Three ways forward, none of them chosen yet:

1. Fix `lipdR` so an all-`NaN` numeric column keeps its type.
2. Have the migration restore the type after writing.
3. Treat an all-`NaN` column as meaningless and drop it — a data question, not a
   code one.

Worth noting the 300-dataset shadow diff did not catch this: it reported
`differs: 0` because the *values* really are unchanged. Only re-reading every
file and validating it surfaced the type loss.

## Promote dry run, second pass

After fixing the lipdR all-`NaN` type loss and re-running the migration:

| source | files | result |
|---|---|---|
| `database` | 7,177 | **all pass** — promotion gate satisfied |
| `GBRCD` | 208 | **all fail**, pre-existing |

**GBRCD's entire database is already invalid.** All 208 source files fail
`validLipd()` with `pub1: author field should be a list` — the author is stored
as a flat string:

```
"Alibert, C., Kinsley, L., Fallon, S.J., McCulloch, M.T., Berkelmans, R. & McAllister, F."
```

rather than the structured `author.name` form the rest of the corpus uses. This
is the same flat-versus-structured author difference seen in the CoralHydro2k
fork, and it predates any of this work — the migration neither caused nor
worsened it.

**Resolved.** `scripts/fix-gbrcd-authors.R` wraps the flat string as
`list(list(name = <string>))`, matching lipdverseR's `fixPubAuthorList()`. All
208 files now pass.

Names are deliberately **not** split into separate entries. A sample of 106
datasets from the main database found **99% of structured author lists carry
exactly one entry**, so a single entry holding the whole string is the corpus
convention rather than a compromise. Splitting would also mean parsing
"Surname, I." against the commas separating authors — the kind of guess that
corrupts names.

Verified by shadow diff that nothing but the author field changed: 289 rows out,
289 in, `pub[N].author` → `pub[N].author[1].name`, and **zero non-author
differences**.

### A version-mismatch trap worth remembering

The first re-run still reported the same 14 database failures, which looked like
the lipdR fix had not worked. It had. The migration script loads lipdR from
source via `devtools::load_all()`, but `lv_promote()` verifies through
`lipdR::readLipd()`, which resolves to the **installed** package. So files were
being written by the fixed lipdR and validated by the unfixed one.

Installing lipdR resolved it. Anything comparing written output against a
validator must be sure both are the same build.

## Promotion — committed 2026-07-31

Both databases now carry the csm structure.

| | files | run id |
|---|---|---|
| `~/Dropbox/lipdverse/database` | 7,177 replaced | `csm-promote-database` |
| `~/Dropbox/lipdverse/GBRCD` | 208 replaced | `csm-promote-gbrcd` |

Zero additions, zero deletions. Every prior copy is in `.trash/<run_id>/`
(1.8 GB and 8.7 MB), and fresh snapshots were taken immediately before.

Post-promotion checks against the live databases:

- **every live file re-verified: 7,177 of 7,177 and 208 of 208 valid**
- 7,177 datasets and 210,363 timeseries, identity validation clean
- fingerprint `bbdd8b14a45e` (was `0fa6e08dfd24`)
- a 250-dataset sample found 155 carrying csm across 1,751 values

Note that `lv_promote()` **moves** staged files into place rather than copying,
so the staging directory is empty afterwards and cannot be promoted twice or
compared against. Verify the live directory instead, as above. The undo path is
`.trash`, not the staging.

### Undoing it

```r
lv_write_rollback(dir = "~/Dropbox/lipdverse/database", run_id = "csm-promote-database")
lv_write_rollback(dir = "~/Dropbox/lipdverse/GBRCD",    run_id = "csm-promote-gbrcd")
```

That restores from `.trash`. Failing that, `scripts/restore-from-snapshot.sh`
restores from the snapshot taken beforehand. Do not run `lv_gc()` on either
directory until the promotion is settled — it prunes the trash.

## A paleo-ensemble corruption in lipdR, found after promotion

Building the test compilation surfaced a second lipdR round-trip bug, more
serious than the `NaN` typing one.

**A paleo ensemble table doubles in width on a single read/write cycle.**

```
SOURCE      GIK17961_2.Wang.2002.paleo1model1ensemble1.csv   127 x 1000
ROUND-TRIP  GIK17961_2.Wang.2002.paleo1model1ensemble1.csv   127 x 2000
TWICE       GIK17961_2.Wang.2002.paleo1model1ensemble1.csv   127 x 2000
```

Chron ensembles are unaffected. The cause is visible in the parsed structure:

```
PALEO ensembleTable -- 2 columns
    depth        number=1,2,3...  values: matrix 127x1000
    temperature  number=1,2,3...  values: matrix 127x1000     <- both claim the same columns

CHRON ensembleTable -- 2 columns
    depth         number=1        values: numeric 218
    ageEnsemble   number=2,3,4... values: matrix 218x1000     <- disjoint, correct
```

Both paleo columns are read as the full 1000-wide matrix and both are written
back, so 1000 becomes 2000. It doubles once and then stabilises.

### Impact on the live database

The csm promotion read and wrote all 7,177 files, so this could have applied
broadly. Checking every paleo-ensemble dataset against the pre-promotion
snapshot: **217 have a paleo ensemble, and exactly 1 was affected** —
`130_806B.Berger.2006.lpd`, 1000 → 2000 members.

That file was restored from `.trash/csm-promote-database` (verified byte-identical
to the pre-promotion snapshot) and the corrupted copy moved to
`.quarantine/csm-promote-database/`. The restored file verifies clean and has
its original 1000-member ensemble. It consequently lacks the csm migration, and
should be re-migrated once the ensemble bug is fixed.

Why only one of 217: most paleo ensembles have the well-formed shape, with one
scalar column and one matrix. Only tables where two columns both parse as the
full matrix double.

### Resolved

The lipdR ensemble fix is verified across the corpus: **all 217 paleo-ensemble
datasets round-trip with zero width change**.

`130_806B.Berger.2006.lpd` was re-migrated with the fixed lipdR and promoted
(run `fix-130_806B`). It now has its correct 1000-member ensemble, uses the
modern `inCompilation` key rather than the legacy `inCompilationBeta`, and
verifies clean. It turned out to carry no csm-eligible keys at all, so the
original migration had been a no-op for it and the trash restore lost nothing.
The quarantine directory has been removed.

Both lipdR fixes are pushed to `origin/dev`.

### lv_promote gained a partial mode

Promoting that single file exposed a design gap the guard caught: `lv_promote`
compared whole directories, so staging one file against a 7,177-file database
read as 7,176 deletions and was refused.

That matters well beyond the one file — the pipeline's normal case is updating
only the datasets that changed. `partial = TRUE` considers no deletions at all,
so a run can add and replace without the files it did not touch looking like
removals. Rollback works the same way.
