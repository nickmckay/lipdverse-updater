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

A dataset-scoped value is therefore stored once per column and the copies can
drift. **That is accepted.** All csm is treated as column-scoped metadata even
where it logically is not, so `collapseTs()` writes each column's value back
without comparing across columns, and no consistency check is required. This is
what keeps the mechanism single-shaped: everything lives in `inCompilation` on a
column, with no second dataset-level path.

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
migrate to, and which compilation wrote the current value is not recoverable
from the files.

**Resolution: copy the existing value into every compilation the dataset
belongs to.** Each compilation then owns its copy and can change it
independently. This is lossless — no compilation ends up with less than it has
today — and it converges on the correct state as compilations are next updated,
without requiring an attribution that cannot be reconstructed.

Three keys name something that is not a compilation in the files:
`geo_paleoDIVERSiteId` (paleoDIVER, 190 datasets) and
`paleoData_useInNAm2k` / `paleoData_useInNam2kHydro` (NAm2k — ambiguous between
`NAm21k`, `NAm21k-noPollen` and `Nam2kDendro`).

Three compilation names appear in files but not in `drakePlan.R`:
`OxfordLSDB-LiPD`, `wNAm`, `DAMP21k_Lakes`. Historical, but their membership
records persist and would need csm handling.

## Field names

Once the compilation owns the namespace, repeating it in the field is
redundant: `iso2k_csm_iso2kUI` should be `iso2k_csm_UI`.

`review/csm-field-names.csv` proposes a name for all 40 keys. Compilation-name
tokens are stripped, as is a leading `QC`, and the first letter is lower-cased
unless it begins an acronym so `UI` and `ID` survive. Anything that shortened to
fewer than three characters or lost more than 60% of its length is left
unchanged and flagged `REVIEW`: 21 clean, 9 already unchanged, 10 needing a
decision.

Fill in `approved_field` to override; blank accepts the proposal.

Six proposals merge several source keys into one field. These are the ones
worth checking, because they are deliberate consolidations rather than renames:

| field | source keys |
|---|---|
| `certification` | `paleoData_QCCertification` (4,355 datasets), `paleoData_iso2kCertification` (491), `paleoData_hydroclimate2kCertification` (700) |
| `entityID` | `paleoData_SISALEntityID`, `chronData_SISALEntityID` (column and table) |
| `iso2kUI` | `paleoData_iso2kUI`, `chronData_iso2kUI` |
| `datasetId` | `LegacyClimateDatasetId`, `paleoDIVERDatasetId` |
| `siteId` | `paleoDIVERSiteId`, `geo_paleoDIVERSiteId` |
| `useInHydro` | `paleoData_useInNAm2kHydro` (28), `paleoData_useInNam2kHydro` (1) |

`certification` is the design working as intended: iso2k and hydroclimate2k each
invented a private key to escape the shared `QCCertification`, and under `csm`
all three become `certification` inside their own namespace.

The last row is probably a typo rather than two fields — `useInNam2kHydro`
occurs in a single dataset against 28 for `useInNAm2kHydro`.

## Collisions from the approved names

Several approved names deliberately consolidate source keys. Whether that
actually collides depends on the **compilation namespace**, not the field name:
`iso2kCertification` and `hydroclimate2kCertification` both become
`QCCertification`, but they land in `iso2k_csm_` and `hydroclimate2k_csm_`
respectively, so they never meet.

A real collision needs two source keys writing the **same `(compilation,
field)` on the same column**. Measured across the database, every such case is
a compilation's private key meeting the shared key being copied into that same
compilation:

| private key | shared key | target namespace | columns |
|---|---|---|---|
| `hydroclimate2kCertification` | `QCCertification` | `hydroclimate2k_csm_QCCertification` | ~844 |
| `iso2kCertification` | `QCCertification` | `iso2k_csm_QCCertification` | ~729 |
| `meetsHoloceneHydroclimateCriteria` | `QCCertification` | `HoloceneHydroclimate_csm_QCCertification` | ~274 |
| `iso2kHackathonNotes` | `QCnotes` | `iso2k_csm_QCnotes` | ~187 |

These arise precisely because iso2k, hydroclimate2k and HoloceneHydroclimate
each invented a private key to escape the shared field. Migration reunites them.

**Rule: append rather than overwrite.** When two source keys resolve to the
same `(compilation, field)` on a column:

1. If the values are identical, keep one.
2. If one is empty, keep the other.
3. Otherwise concatenate, **compilation-private value first**, separated by
   `"; "`. The private value is the more specific judgement, so it reads first;
   nothing is lost either way.

No other collisions occur. `useIn` consolidates `iso2kPrimaryTimeseries`,
`useInOnset` and `useInNAm2kHydro`, but no column carries more than one of them.

## Open items

1. **Two pairs differ only by letter case, producing near-duplicate fields.**

   - `chronData_SISALEntityID` maps to `SISALEntityID` at `column` scope but
     `entityID` at `table` scope, while `paleoData_SISALEntityID` maps to
     `entityID`. That yields both `SISALLiPD_csm_SISALEntityID` and
     `SISALLiPD_csm_entityID`; presumably all three should be `entityID`.
   - `paleoData_pages2kID` and `paleoData_pages2kId` map to `pages2kID` and
     `pages2kId`, giving `Pages2kTemperature_csm_pages2kID` alongside
     `Pages2kTemperature_csm_pages2kId`. These look like one field with a
     capitalisation typo in the source data (781 versus 81 occurrences).

2. **Three keys have no resolvable owner**, and are now marked `Remove`:
   `geo_paleoDIVERSiteId`, `paleoData_useInNAm2k`, `paleoData_useInNam2kHydro`,
   along with `paleoDIVERDatasetId` and `paleoDIVERSiteId`.

3. **`paleoData_ocean2kID` and `paleoData_useInNAm2kHydro` have no compilation
   assigned** — neither `Remove` nor an owner.
