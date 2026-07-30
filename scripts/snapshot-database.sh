#!/bin/bash
#
# Snapshot the LiPDverse database with a per-file md5 manifest.
#
# This is the undo button for lipdverseR's non-transactional final write
# (nightlyUpdateDrake.R:2057-2058 unlinks files, then writes them back; a
# failure between the two destroys the loaded subset of the database).
#
# On APFS the copy is a clonefile (`cp -c`): copy-on-write, so a snapshot costs
# almost no time and almost no space until the originals change. Unlike
# hardlinks, clones are unaffected by in-place modification of the source.
#
#   ./snapshot-database.sh              # take a snapshot
#   ./snapshot-database.sh --dry-run    # report what would happen
#
# Env: LIPDVERSE_DATABASE, LIPDVERSE_SNAPSHOTS,
#      LIPDVERSE_SNAPSHOT_RETAIN_DAYS, LIPDVERSE_SNAPSHOT_MIN_KEEP

LV_TAG=snapshot
. "$(dirname "${BASH_SOURCE[0]}")/lib/common.sh"

DRY_RUN=0
[ "${1:-}" = "--dry-run" ] && DRY_RUN=1

SRC="$LIPDVERSE_DATABASE"
[ -d "$SRC" ] || die "database not found: $SRC (set LIPDVERSE_DATABASE)"

n_src=$(find "$SRC" -maxdepth 1 -name '*.lpd' -type f | wc -l | tr -d ' ')
[ "$n_src" -gt 0 ] || die "no .lpd files in $SRC -- refusing to snapshot an empty database"

prev=$(latest_snapshot)
if [ -n "$prev" ] && [ -f "$LIPDVERSE_SNAPSHOTS/$prev/manifest.tsv" ]; then
  n_prev=$(wc -l < "$LIPDVERSE_SNAPSHOTS/$prev/manifest.tsv" | tr -d ' ')
  # A large drop means the live database may be mid-corruption. Snapshot it
  # anyway (the snapshot is never the thing that loses data) but shout.
  if [ "$n_prev" -gt 0 ] && [ "$n_src" -lt $(( n_prev * 8 / 10 )) ]; then
    warn "file count dropped from $n_prev to $n_src (>20%). Snapshotting anyway."
    warn "If this was not intentional, restore from $LIPDVERSE_SNAPSHOTS/$prev"
  fi
fi

# Snapshot names are second-resolution so they stay sortable and fixed-width.
# Two runs in the same second would collide, so wait out the second rather than
# failing or inventing a suffix that breaks the naming pattern.
stamp=$(date -u +%Y%m%dT%H%M%SZ)
for _ in 1 2 3 4 5; do
  [ -e "$LIPDVERSE_SNAPSHOTS/$stamp" ] || break
  sleep 1
  stamp=$(date -u +%Y%m%dT%H%M%SZ)
done
dest="$LIPDVERSE_SNAPSHOTS/$stamp"

if [ "$DRY_RUN" = 1 ]; then
  log "DRY RUN"
  log "  source:   $SRC ($n_src files)"
  log "  would be: $dest"
  log "  previous: ${prev:-<none>}"
  exit 0
fi

mkdir -p "$LIPDVERSE_SNAPSHOTS"
[ -e "$dest" ] && die "snapshot already exists: $dest"

# Build into a .partial directory so an interrupted run never leaves a
# snapshot that looks complete. Only the final rename publishes it.
staging="$dest.partial"
rm -rf "$staging"
mkdir -p "$staging/db"

log "cloning $n_src files from $SRC"
if ! cp -Rc "$SRC"/*.lpd "$staging/db/" 2>/dev/null; then
  warn "clonefile unavailable (non-APFS?), falling back to a full copy"
  cp -R "$SRC"/*.lpd "$staging/db/" || { rm -rf "$staging"; die "copy failed"; }
fi

n_copied=$(find "$staging/db" -name '*.lpd' -type f | wc -l | tr -d ' ')
[ "$n_copied" = "$n_src" ] || { rm -rf "$staging"; die "copied $n_copied of $n_src files"; }

log "hashing $n_copied files with $LV_JOBS jobs"
manifest_of "$staging/db" > "$staging/manifest.tsv"

n_manifest=$(wc -l < "$staging/manifest.tsv" | tr -d ' ')
[ "$n_manifest" = "$n_src" ] || { rm -rf "$staging"; die "manifest has $n_manifest rows, expected $n_src"; }

fp=$(fingerprint_of "$staging/manifest.tsv")
bytes=$(awk -F'\t' '{s+=$2} END{print s+0}' "$staging/manifest.tsv")

cat > "$staging/meta.json" <<JSON
{
  "timestamp": "$stamp",
  "source": "$SRC",
  "host": "$(hostname -s)",
  "n_files": $n_manifest,
  "total_bytes": $bytes,
  "fingerprint": "$fp",
  "previous": "${prev:-}"
}
JSON

# Diff against the previous snapshot so the operator sees what moved.
if [ -n "$prev" ] && [ -f "$LIPDVERSE_SNAPSHOTS/$prev/manifest.tsv" ]; then
  join -t"$(printf '\t')" -j 1 -a 1 -a 2 -o 0,1.2,2.2 -e '' \
    <(cut -f3,1 "$LIPDVERSE_SNAPSHOTS/$prev/manifest.tsv" | awk -F'\t' '{print $2"\t"$1}' | LC_ALL=C sort -t"$(printf '\t')" -k1,1) \
    <(cut -f3,1 "$staging/manifest.tsv"                   | awk -F'\t' '{print $2"\t"$1}' | LC_ALL=C sort -t"$(printf '\t')" -k1,1) \
    2>/dev/null \
  | awk -F'\t' 'BEGIN{OFS="\t"; print "status","path","old_md5","new_md5"}
      $2=="" && $3!="" {print "added",   $1, "",  $3; next}
      $2!="" && $3=="" {print "removed", $1, $2,  ""; next}
      $2!=$3           {print "changed", $1, $2,  $3}' \
  > "$staging/changes.tsv"

  n_add=$(awk -F'\t' 'NR>1 && $1=="added"'   "$staging/changes.tsv" | wc -l | tr -d ' ')
  n_rm=$( awk -F'\t' 'NR>1 && $1=="removed"' "$staging/changes.tsv" | wc -l | tr -d ' ')
  n_ch=$( awk -F'\t' 'NR>1 && $1=="changed"' "$staging/changes.tsv" | wc -l | tr -d ' ')
  log "vs $prev: +$n_add  -$n_rm  ~$n_ch"
  [ "$n_rm" -gt 0 ] && warn "$n_rm file(s) removed since $prev -- see $dest/changes.tsv"
else
  log "no previous snapshot; this is the baseline"
fi

mv "$staging" "$dest"
log "snapshot complete: $dest ($n_manifest files, fingerprint ${fp:0:12})"

# Prune, but never below MIN_KEEP regardless of age.
all=$(list_snapshots)
n_all=$(printf '%s\n' "$all" | grep -c . || true)
if [ "$n_all" -gt "$LIPDVERSE_SNAPSHOT_MIN_KEEP" ]; then
  cutoff=$(date -u -v-"${LIPDVERSE_SNAPSHOT_RETAIN_DAYS}"d +%Y%m%dT%H%M%SZ 2>/dev/null \
        || date -u -d "-${LIPDVERSE_SNAPSHOT_RETAIN_DAYS} days" +%Y%m%dT%H%M%SZ)
  n_removable=$(( n_all - LIPDVERSE_SNAPSHOT_MIN_KEEP ))
  for s in $all; do
    [ "$n_removable" -le 0 ] && break
    if [ "$s" \< "$cutoff" ]; then
      log "pruning $s (older than $LIPDVERSE_SNAPSHOT_RETAIN_DAYS days)"
      rm -rf "${LIPDVERSE_SNAPSHOTS:?}/$s"
      n_removable=$(( n_removable - 1 ))
    fi
  done
fi

# Leave the partial dirs of any crashed earlier run visible but harmless.
find "$LIPDVERSE_SNAPSHOTS" -maxdepth 1 -name '*.partial' -mtime +1 -exec rm -rf {} + 2>/dev/null || true
