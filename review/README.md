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
