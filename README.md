# lipdverse-updater

A rebuilt database-update pipeline for the [LiPDverse](https://lipdverse.org):
~7,200 LiPD files across 20 partially-overlapping compilations.

Replaces the update half of [`lipdverseR`](https://github.com/nickmckay/lipdverseR).
Website generation is out of scope here and gets its own repo, consuming the
Parquet/DuckDB export this produces.

Status: **Stages 0 and 1 complete.** The backup layer is live and the read-only core
of the `lipdverseUpdater` package is built and tested. Nothing writes to the database
yet.

## Stage 1 — the read-only core

```r
devtools::load_all(".")

cfg <- lv_config("hydroclimate2k")   # typed config; 20 compilations validate
s   <- lv_scan(cfg$lipd_dir)         # content-hash scan + fingerprint
idx <- lv_db_index(s)                # identity index, metadata only
lv_validate_identity(idx)            # the gate
```

| file | what it provides |
|---|---|
| `R/paths.R` | every path and secret from the environment, nothing hardcoded |
| `R/config.R` | `lv_config()` layering defaults, registry, per-compilation YAML, overrides |
| `R/issues.R` | `lv_issues` accumulator; `error` severity aborts *after* the report is written |
| `R/scan.R` | byte-hash scan, `touch`-stable fingerprint, scan diffing |
| `R/identity.R` | `lv_db_index()` — reads only the JSON member of each `.lpd` |
| `R/validate_identity.R` | duplicate and missing-identifier gate; never auto-renames |
| `R/lock.R` | atomic locks released by `withr::defer` on any exit path |
| `R/cache.R` | content-addressed stage cache with atomic writes |
| `R/logging.R` | run ids, per-run artifact directories, logging |

Run the tests with
`Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", package="lipdverseUpdater", load_package="none")'`
(115 tests).

### Identity of the current database

`lv_validate_identity()` over all 7,177 datasets and 210,363 timeseries in
`database/` reports **zero issues**: no duplicate TSids, datasetIds or dataSetNames,
no missing identifiers, no filename/metadata mismatches, no unreadable files. Indexing
takes ~35s.

The suite includes negative controls that inject each fault class, because without
them a clean result is indistinguishable from a validator that never fires.

That means lipdverseR's `-dup` TSid renaming is currently defending against a condition
that does not exist in the files.

## Shadow-run harness

The acceptance gate for cutting a compilation over: run both pipelines against separate
copies of the database and diff the results field by field.

```sh
./scripts/shadow-compare.R <old_dir> <new_dir> --out=diff.csv
```

```r
shadow_normalize(dir)          # flatten a directory to canonical long form
shadow_diff(old, new)          # differs / only_old / only_new
shadow_report(diff, path)      # summary + full CSV
shadow_compare(old, new, out)  # all three
shadow_snapshot(src, dest)     # clonefile copy for an isolated run
shadow_run_legacy(...)         # lipdverseR in a separate callr session
```

`shadow_normalize()` emits one row per leaf value, keyed by `datasetId`, `TSid` and
`field_path`. Three properties make the diff trustworthy:

- **Columns are keyed by TSid, not position**, so a writer emitting columns in a
  different order produces no diff.
- **Numeric text is canonicalized** (`1.10` and `1.1` agree).
- **Data CSVs are hashed after parsing, not as bytes.** lipdverseR quotes every CSV
  field and the new writer does not; hashing raw bytes reported all 179 CoralHydro2k
  datasets as having changed data when the measurements were identical. There is a
  regression test for this.

Volatile fields are dropped via `inst/extdata/shadow_ignore.csv` (changelog timestamps,
`lipdverseLink`, `createdBy`, derived pointers). Pass `ignore = NULL` to keep them.

`shadow_run_legacy()` runs lipdverseR in a separate `callr` session, because it calls
`setwd()`, writes to the global environment with `<<-`, and caches credentials relative
to its working directory. It refuses to run against the live database unless
`allow_live = TRUE`, and it **writes to Google Sheets** as part of a normal legacy run.
It is not covered by the test suite — that needs a live Google session and hours of
runtime.

### Worked example: the CoralHydro2k fork

`review/shadow-coralhydro2k.csv` is the harness run over the 179 datasets that exist in
both `CoralHydro2k/` and `database/`: 5,319 differences, all explained.

| category | n | what it is |
|---|---|---|
| `only_new` | 4,118 | fields the `database/` copy gained: `longName` (608), `medianRes12k` (608), `interpretation[1..6].scope` (304 each) |
| `only_old` | 544 | fork-only: CoralHydro2k compilation version `1_0_1` (252), flat `pub[1].author` (179) |
| `differs` | 657 | `units` `AD`→`yr AD` (304), `archiveType` `coral`→`Coral` (179), `variableName` `SrCa`→`Sr/Ca` (171) |

Two structural changes show up clearly: publication authors moved from a flat string
(`"Abram, N. J., Gagan, M. K., …"`) to a structured `pub[1].author.name`, and
interpretations gained an explicit `scope`. **Zero data differences** — the measurements
are identical in both copies; only metadata diverged.

### QC sheets versus files

TSids present in a QC sheet but in no `.lpd`, per compilation: GBRCD 1,199 of 1,199
(it reads its own database — see below), HoloceneHydroclimate 36, HoloceneAbruptChange
31, `test` 23, everything else 3 or fewer.

## Stage 0 — backup and baseline

Two nightly jobs, deliberately independent of the rest of the rewrite. They address
the two ways the current system can lose data irrecoverably.

| script | what it protects against |
|---|---|
| `scripts/snapshot-database.sh` | `lipdverseR` unlinks `.lpd` files *before* rewriting them (`nightlyUpdateDrake.R:2057-2058`). A failure between the two destroys the loaded subset of the database. |
| `scripts/snapshot-qc-sheets.R` | There is no versioned QC baseline. `backupQCId` is not set for **any** compilation in `drakePlan.R`, and `lastUpdate.csv` is overwritten in place each run. |

```sh
./scripts/install-cron.sh              # install both as launchd agents
./scripts/install-cron.sh --status
```

launchd rather than cron: it runs missed jobs on wake, so an asleep laptop does not
silently skip a night of backups.

### Database snapshots

```sh
./scripts/snapshot-database.sh                       # take one
./scripts/restore-from-snapshot.sh --list
./scripts/restore-from-snapshot.sh latest Foo.Bar.2016.lpd
./scripts/restore-from-snapshot.sh latest --all --dry-run
./scripts/test-snapshot.sh                           # 17 hermetic tests
```

On APFS the copy is a `clonefile` (`cp -c`): copy-on-write, so a full snapshot of
7,177 files takes ~4 seconds and almost no disk until the originals change. Unlike
hardlinks, clones are unaffected by in-place modification of the source. A full run
including hashing is ~70 seconds.

Each snapshot holds `db/` (the clone), `manifest.tsv` (`md5 / bytes / path`),
`meta.json`, and `changes.tsv` diffing it against the previous snapshot.

The fingerprint is the md5 of the sorted per-file md5s, so **`touch` does not change
it**. `lipdverseR`'s `directoryMD5()` zips the directory and hashes the zip; zip embeds
mtimes, so a touched-but-unchanged file looks like a modification.

Restores never delete: an existing live file is moved to `.pre-restore/<timestamp>/`
before being replaced, and the default is a dry run.

Snapshots live outside Dropbox (`~/lipdverse-snapshots`) so Dropbox does not rehydrate
each clone into a real file and erase the space savings.

### QC sheet snapshots

```sh
./scripts/snapshot-qc-sheets.R
./scripts/snapshot-qc-sheets.R --compilation=hydroclimate2k
./scripts/snapshot-qc-sheets.R --dry-run
```

Dumps every tab of all 20 compilation QC sheets, plus the shared convo, versioning, and
vocabulary-registry sheets, into `$LIPDVERSE_QCSTORE` (default
`~/GitHub/lipdverse-qcstore`) and commits.

Written to **stable paths** — git is the history. `git log -p
snapshots/hydroclimate2k/QC.csv` shows every change to any cell, and `git blame`
attributes it. That is the versioned baseline the current system lacks.

Tabs are addressed **by name**, never index (`sheet = 1` reads break when tab order
shifts), and every column is read as character (type guessing silently drops values in
sparse columns whose first entry appears past row 1000).

A full run is ~9.5 minutes, almost entirely Sheets API latency.

### Cross-compilation conflict report

```sh
./scripts/report-cross-compilation-conflicts.R --out=conflicts.csv
```

Compilations are overlapping views of one file collection, not partitions: **56% of
datasets (3,974 of 7,121) belong to two or more**, one to seven. Fields such as
`archiveType`, `variableName`, `units`, and `geo_*` live in the `.lpd` file, so they are
shared by every compilation containing that dataset.

When two compilations that share a database directory disagree about one, the
compilation that runs last silently wins — no conflict raised, no changelog entry. As
of 2026-07-30 there are 6,084 such disagreements across 4,486 TSids.

**Read that number with two corrections.**

*Staleness.* 5,589 of the 6,084 (92%) involve a compilation that has not run in over
three years — HoloceneAbruptChange last ran 2020-10-08. Those sheets are simply stale
views of a database that moved on, and a correct three-way merge converges them on the
next run rather than fighting.

*Separate directories.* Not every compilation reads from `database/`. Three point
elsewhere (see below), so they cannot overwrite each other's files at all. Once
CoralHydro2k is excluded, **zero** conflicts remain between compilations that both
share `database/` and both ran within 14 months.

Reads only the local snapshots; no network. It does **not** currently account for
per-compilation database directories, so treat cross-directory pairs as forks rather
than conflicts.

## Per-compilation database directories

`lipd_dir` is per-compilation, not global. **GBRCD is the only genuinely separate
database** (`~/Dropbox/lipdverse/GBRCD`, 208 files): only 4 of its filenames also
appear in `database/`, it is identity-clean, and 100% of its 1,199 QC TSids match its
own files.

`test` uses `~/Dropbox/lipdverse/testDatabase` (15 files) and is not a real compilation.

### CoralHydro2k: integrated, but `drakePlan.R` still points at the old directory

CoralHydro2k **has** been integrated into `database/` — all 179 of its datasets carry
`inCompilationBeta: CoralHydro2k` there. The registry here points it at `database`
accordingly.

`~/Dropbox/lipdverse/CoralHydro2k/` still exists and still holds all 179 datasets, and
`lipdverseR`'s `drakePlan.R` still names it as that compilation's `lipdDir`. So the last
CoralHydro2k run (2025-06-30) wrote to the stale fork rather than to the integrated
copies, and the two have diverged in both directions:

| | fork | `database/` |
|---|---|---|
| dataset version | 1.0.3 | 1.0.5 / 1.0.6 |
| CoralHydro2k compilation version | **1.0.1** | 1.0.0 |
| `archiveType` | `coral` | `Coral` |
| `variableName` | `SrCa`, `SrCaUncertainty` | `Sr/Ca`, `uncertainty` |

The `database/` copies got vocabulary standardization and other compilations' updates;
the fork got a CoralHydro2k run that the integrated copies never received. All 179 files
differ; none is byte-identical.

Two consequences:

1. Running CoralHydro2k under `lipdverseR` today writes to the fork again. Its `lipdDir`
   needs updating there, or the run needs to happen under the new system.
2. Whatever the 2025-06-30 run produced (compilation version 1.0.1) exists only in the
   fork. Its QC sheet is already compatible with `database/` — all 304 of its QC TSids
   resolve there — so re-pointing and re-running should reconcile it, but the fork
   should not be deleted until that is confirmed.

## Configuration

All paths come from the environment; nothing is hardcoded.

| variable | default |
|---|---|
| `LIPDVERSE_DATABASE` | `~/Dropbox/lipdverse/database` |
| `LIPDVERSE_SNAPSHOTS` | `~/lipdverse-snapshots` |
| `LIPDVERSE_QCSTORE` | `~/GitHub/lipdverse-qcstore` |
| `LIPDVERSE_SNAPSHOT_RETAIN_DAYS` | `30` |
| `LIPDVERSE_SNAPSHOT_MIN_KEEP` | `7` |
| `LIPDVERSE_GOOG_EMAIL` | `nick.mckay2@gmail.com` |
| `LIPDVERSE_GOOG_CACHE` | `~/GitHub/lipdverseR/.secret` |

`inst/extdata/compilations.tsv` is the compilation registry (name, QC sheet id,
last-update sheet id, age/year), extracted from `lipdverseR/drakePlan.R`.

## Two registry anomalies worth review

Both extracted from `drakePlan.R`, neither addressed here:

- **`NAm21k`** has `qcId == lastUpdateId`. The three-way merge's parent and the live
  sheet are then the same document, so no curator edit can ever register as a change
  against the baseline.
- **`Temp24k`** uses `17XaSH1MNCtBI6ftEnTOHgy9C6mtXvYpR7JCNUNT-vI8`, which is also the
  new-project QC template id used by `createNewProject()`.
