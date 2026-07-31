# Compilation-specific metadata (`csm`) in LiPD

Proposal for storing per-compilation metadata inside LiPD files, and for how
`extractTs()` / `collapseTs()` in `lipdR` should handle it.

Status: agreed in outline; two open items at the end.

## Why

Some metadata is a judgement made *by a compilation about a dataset*, not a
property of the dataset itself. Today those live as flat keys in the same
namespace as everything else, which produces two failures.

**Shared keys get overwritten.** `paleoData_QCCertification` is written by 15
compilations and `paleoData_QCnotes` by 16, into one field. Whichever
compilation runs last wins, silently. Measured across the database,
`QCCertification` appears in 4,355 datasets with only 54% of them in its most
common compilation — it is not owned by anyone.

**Compilations invent private keys to escape it.** `paleoData_iso2kCertification`
and `paleoData_hydroclimate2kCertification` already exist: two compilations
solved the problem by hand. This generalises that.

## Storage

Compilation-specific metadata nests inside the existing `inCompilation` entry
that already records membership, under a `csm` key:

```json
"inCompilation": [
  {
    "compilationName": "iso2k",
    "compilationVersion": ["1_0_0", "1_0_1"],
    "csm": {
      "UI": "...",
      "certification": "B",
      "primaryTimeseries": true
    }
  }
]
```

Consequences of nesting rather than prefixing flat keys:

- Two compilations can both have `certification` with no collision.
- Metadata travels with membership: removing a dataset from a compilation
  removes its metadata, rather than leaving it orphaned.
- Each compilation's QC sheet naturally sees only its own fields.

`csm` values are **not versioned**. One current value per field; history lives
in the QC store and the changelog. `compilationVersion` stays a vector for
membership, but `csm` is not indexed by it.

### Everything is column-level

`inCompilation` lives on columns, and *all* csm is stored there, including
metadata that is logically dataset-scoped (site IDs, regions). Those values are
replicated across every column of the dataset.

Source keys lose their structural prefix when they move into `csm`:

| today | becomes |
|---|---|
| `geo_pages2kRegion` | `Pages2kTemperature` → `csm.pages2kRegion` |
| `paleoData_iso2kUI` | `iso2k` → `csm.iso2kUI` |
| `chronData_SISALEntityID` | `SISAL-LiPD` → `csm.SISALEntityID` |
| `LegacyClimateDatasetId` | `LegacyClimate-LiPD` → `csm.LegacyClimateDatasetId` |

Verified: no two source keys collapse onto the same `(compilation, field)` pair
after prefix stripping.

The cost of replication is that a dataset-scoped value is stored once per
column, so the copies can drift. `collapseTs()` should check that all columns
agree for a given `(compilation, field)` and report a conflict rather than
silently taking one — see open items.

## Flat form

```
<compilationKey>_csm_<field>
```

e.g. `iso2k_csm_certification`, `hydroclimate2k_csm_QCCertification`.

### Keyed by compilation name, not array index

`inCompilation1_csm_*` is not usable: the array index is positional and varies
between datasets. Measured over 1,138 datasets:

| compilation | indices it occupies |
|---|---|
| hydroclimate2k | 1, 2, 3, 4, 5 |
| Temp24k | 1, 2, 3, 4, 5 |
| LakeStatus21k | 2, 3, 4, 5 (never 1) |
| SISAL-LiPD | 1, 3, 4 |

11 of 19 compilations appear at more than one index. Index-based keys would
scatter one logical field across five columns of the QC sheet and the tibble.

### Compilation key sanitisation

`gsub("[-_]", "", compilationName)`.

Both characters must go. Of the 19 compilation names present in the files, four
contain a hyphen and one — `DAMP21k_Lakes`, 308 occurrences — contains an
underscore, which would otherwise produce a three-underscore key.

```
SISAL-LiPD         -> SISALLiPD
LegacyClimate-LiPD -> LegacyClimateLiPD
NAm21k-noPollen    -> NAm21knoPollen
OxfordLSDB-LiPD    -> OxfordLSDBLiPD
DAMP21k_Lakes      -> DAMP21kLakes
```

Verified: no collisions after sanitisation, nothing left non-alphanumeric, and
no clash with any existing key prefix.

### The two-underscore invariant

`<comp>_csm_<field>` is the only flat key shape with two underscores. Of 509
keys observed across the database, 87 have none and 421 have exactly one. The
single exception, `pub__ArrayType_`, is already marked `deleteMe`. So
`grepl("^[^_]+_csm_", key)` is a sound test, and no compilation name contains
an underscore once sanitised.

## `extractTs()`

**Always extract every compilation's csm.** No filtering by default.

This is the important safety property. If `extractTs()` returned only one
compilation's metadata, `collapseTs()` on that timeseries would write back a
partial view and delete every other compilation's csm. That is the failure mode
that destroyed 24 `paleoData_createdBy` values in the current pipeline, except
here the partial view would be the *intended* usage. Filtering and renaming can
be applied afterwards, on a timeseries that is never collapsed.

**Implement as a post-pass, not inside the flattener.** The flattener in
`timeseries_extract.R:438-480` derives names structurally, from nesting depth
and position. Building `iso2k_csm_certification` requires reading a sibling
value (`compilationName`) to construct the key, which it cannot do.

`splitInterpretationByScope()` is the precedent: value-dependent renaming done
as a pass over the already-flat timeseries, with `combineInterpretationByScope()`
as its inverse. csm should follow the same shape:

```r
extractTs(D)                     # flattens inCompilation1_csm_certification
  -> nameCsmByCompilation(ts)    # rewrites to iso2k_csm_certification
collapseTs(ts)
  -> unnameCsmByCompilation(ts)  # back to positional, then collapse as today
```

## `collapseTs()`

Two requirements.

**Merge, never replace.** Write csm back by matching `compilationName`, leaving
entries for compilations absent from the timeseries untouched. Combined with
always-extract-everything this makes partial loss structurally impossible rather
than merely unlikely.

**Scrap `compSpecificMetadata`.** `timeseries_collapse.R:247,265,288` contains a
half-finished earlier attempt using a sibling `compSpecificMetadata` block keyed
positionally, with no extract-side counterpart. It is superseded by this and
should be removed rather than left as a third mechanism.

## Migration

`review/compilation-specific-keys.csv` — 40 keys, 31 column / 5 root / 3 geo /
1 table.

| ownership | keys | handling |
|---|---|---|
| single owner | 23 | move to that compilation's `csm` |
| shared across compilations | 14 | namespace per writing compilation |
| owner unresolved | 3 | needs review |

The 14 shared keys are the point of the exercise: `QCCertification` (4,355
datasets), `QCnotes` (3,613), `QCRemainingIssues` (1,178), `iso2kUI` (1,747) and
others are written by many compilations into one field. They have no owner to
migrate to; each writing compilation gets its own namespaced copy, and the
existing single value has to be attributed — which requires knowing which
compilation wrote it. That attribution is not recoverable from the files alone
and will need the QC sheets.

Three keys name something that is not a compilation in the files:
`geo_paleoDIVERSiteId` (paleoDIVER, 190 datasets) and
`paleoData_useInNAm2k` / `paleoData_useInNam2kHydro` (NAm2k — ambiguous between
`NAm21k`, `NAm21k-noPollen` and `Nam2kDendro`).

Three compilation names appear in files but not in `drakePlan.R`:
`OxfordLSDB-LiPD`, `wNAm`, `DAMP21k_Lakes`. Historical, but their membership
records persist and would need csm handling.

## Open items

1. **Field-name redundancy.** Once the compilation owns the namespace, names
   like `iso2k_csm_iso2kUI`, `pages2k_csm_pages2kID` and
   `LegacyClimate_csm_LegacyClimateDatasetId` repeat it. Shortening to
   `iso2k_csm_UI`, `Pages2kTemperature_csm_ID`,
   `LegacyClimateLiPD_csm_datasetId` is cheap now and awkward later. Cosmetic,
   but it fixes 40 names permanently.

2. **Replication consistency.** For dataset-scoped values replicated across
   columns, should `collapseTs()` error on disagreement between columns, warn
   and take the most common, or take the first? Erroring is safest but will fire
   on any file where a previous tool wrote them unevenly.

3. **Attribution of the 14 shared keys.** Determining which compilation wrote
   the existing `QCCertification` value needs the QC sheets, and where two
   compilations both have a value for the same TSid, a decision per case.
