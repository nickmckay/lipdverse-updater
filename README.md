# lipdverse-updater

A rebuilt database-update pipeline for the [LiPDverse](https://lipdverse.org):
~7,200 LiPD files across 20 partially-overlapping compilations.

Replaces the update half of [`lipdverseR`](https://github.com/nickmckay/lipdverseR).
Website generation is out of scope here and gets its own repo, consuming the
Parquet/DuckDB export this produces.

Status: **Stage 0 complete.** The backup layer is live; the R package is not built yet.

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

17 compilations read and write `~/Dropbox/lipdverse/database`. Three do not:

| compilation | directory | files |
|---|---|---|
| `CoralHydro2k` | `~/Dropbox/lipdverse/CoralHydro2k` | 179 |
| `GBRCD` | `~/Dropbox/lipdverse/GBRCD` | 208 |
| `test` | `~/Dropbox/lipdverse/testDatabase` | 15 |

`lipd_dir` is therefore per-compilation, not global.

GBRCD is genuinely separate: only 4 of its 208 filenames also appear in `database/`,
and it is identity-clean with 100% of its 1,199 QC TSids matching its own files.

**CoralHydro2k is a fork, and this one needs a decision.** All 179 of its datasets
exist in *both* directories with the same `datasetId` and the same 608 TSids, but no
file is byte-identical:

- the `CoralHydro2k/` copy is frozen at dataset version `1.0.3`; the `database/` copy
  has advanced to `1.0.5`/`1.0.6`
- all 179 disagree on `archiveType`
- 171 of 608 TSids disagree on `variableName` (`SrCa` vs `Sr/Ca`,
  `SrCaUncertainty` vs `uncertainty`) — the `database/` copy has had vocabulary
  standardization applied and the fork has not

Neither side ever overwrites the other, so this is not a last-writer-wins problem. It
is two published copies of the same `datasetId` with different content, and consumers
get different answers depending on which compilation they download from.

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
