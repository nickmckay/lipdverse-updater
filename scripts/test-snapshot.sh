#!/bin/bash
#
# Hermetic tests for snapshot-database.sh / restore-from-snapshot.sh.
# Runs entirely in a temp directory; never touches the real database.
#
#   ./scripts/test-snapshot.sh

set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()   { printf '  ok   %s\n' "$1"; pass=$(( pass + 1 )); }
no()   { printf '  FAIL %s\n' "$1"; fail=$(( fail + 1 )); }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else no "$1 (expected '$3', got '$2')"; fi; }

export LIPDVERSE_DATABASE="$TMP/db"
export LIPDVERSE_SNAPSHOTS="$TMP/snaps"
mkdir -p "$LIPDVERSE_DATABASE"

mklpd() { printf '%s' "$2" > "$LIPDVERSE_DATABASE/$1"; }
snap()  { "$here/snapshot-database.sh" >/dev/null 2>&1; }
newest(){ ls -1 "$LIPDVERSE_SNAPSHOTS" | grep -E '^[0-9]{8}T[0-9]{6}Z$' | sort | tail -1; }
meta()  { sed -n "s/.*\"$2\": *\"\{0,1\}\([^\",]*\)\"\{0,1\}.*/\1/p" "$LIPDVERSE_SNAPSHOTS/$1/meta.json"; }
fp()    { meta "$1" fingerprint; }

echo "== empty database is refused =="
"$here/snapshot-database.sh" >/dev/null 2>&1
check "exits non-zero on empty db" "$?" "1"
check "no snapshot created" "$(ls -1 "$LIPDVERSE_SNAPSHOTS" 2>/dev/null | wc -l | tr -d ' ')" "0"

echo "== baseline snapshot =="
mklpd a.lpd "alpha"; mklpd b.lpd "bravo"; mklpd c.lpd "charlie"
snap; s1=$(newest)
check "3 files recorded" "$(meta "$s1" n_files)" "3"
check "manifest has 3 rows" "$(wc -l < "$LIPDVERSE_SNAPSHOTS/$s1/manifest.tsv" | tr -d ' ')" "3"
check "no changes.tsv on baseline" "$([ -f "$LIPDVERSE_SNAPSHOTS/$s1/changes.tsv" ] && echo yes || echo no)" "no"

echo "== fingerprint ignores mtime (lipdverseR directoryMD5 defect) =="
sleep 1; touch "$LIPDVERSE_DATABASE"/*.lpd
snap; s2=$(newest)
check "fingerprint unchanged after touch" "$(fp "$s2")" "$(fp "$s1")"
check "changes.tsv is header-only" "$(( $(wc -l < "$LIPDVERSE_SNAPSHOTS/$s2/changes.tsv") - 1 ))" "0"

echo "== add / remove / change are all detected =="
printf '%s' "alpha-edited" > "$LIPDVERSE_DATABASE/a.lpd"
rm "$LIPDVERSE_DATABASE/b.lpd"
mklpd d.lpd "delta"
snap; s3=$(newest)
ch="$LIPDVERSE_SNAPSHOTS/$s3/changes.tsv"
check "1 changed" "$(awk -F'\t' 'NR>1 && $1=="changed"' "$ch" | wc -l | tr -d ' ')" "1"
check "1 removed" "$(awk -F'\t' 'NR>1 && $1=="removed"' "$ch" | wc -l | tr -d ' ')" "1"
check "1 added"   "$(awk -F'\t' 'NR>1 && $1=="added"'   "$ch" | wc -l | tr -d ' ')" "1"
check "fingerprint moved" "$([ "$(fp "$s3")" != "$(fp "$s2")" ] && echo yes || echo no)" "yes"

echo "== restore is byte-identical and never destroys =="
want=$(/sbin/md5 -q "$LIPDVERSE_SNAPSHOTS/$s3/db/a.lpd" 2>/dev/null || md5sum "$LIPDVERSE_SNAPSHOTS/$s3/db/a.lpd" | cut -d' ' -f1)
printf '%s' "CORRUPT" > "$LIPDVERSE_DATABASE/a.lpd"
rm "$LIPDVERSE_DATABASE/d.lpd"

"$here/restore-from-snapshot.sh" latest --all --dry-run >/dev/null 2>&1
check "dry run leaves corruption in place" "$(cat "$LIPDVERSE_DATABASE/a.lpd")" "CORRUPT"
check "dry run does not recreate deleted file" "$([ -f "$LIPDVERSE_DATABASE/d.lpd" ] && echo yes || echo no)" "no"

"$here/restore-from-snapshot.sh" latest --all >/dev/null 2>&1
got=$(/sbin/md5 -q "$LIPDVERSE_DATABASE/a.lpd" 2>/dev/null || md5sum "$LIPDVERSE_DATABASE/a.lpd" | cut -d' ' -f1)
check "corrupted file restored byte-identical" "$got" "$want"
check "deleted file restored" "$([ -f "$LIPDVERSE_DATABASE/d.lpd" ] && echo yes || echo no)" "yes"
check "overwritten copy preserved under .pre-restore" \
  "$(find "$LIPDVERSE_DATABASE/.pre-restore" -name a.lpd -exec cat {} \; 2>/dev/null)" "CORRUPT"

echo "== retention keeps at least MIN_KEEP =="
export LIPDVERSE_SNAPSHOT_RETAIN_DAYS=0 LIPDVERSE_SNAPSHOT_MIN_KEEP=2
snap
n=$(ls -1 "$LIPDVERSE_SNAPSHOTS" | grep -cE '^[0-9]{8}T[0-9]{6}Z$')
check "min_keep floor respected" "$([ "$n" -ge 2 ] && echo yes || echo no)" "yes"

echo
printf '%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
