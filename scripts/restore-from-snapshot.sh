#!/bin/bash
#
# Restore .lpd files from a snapshot taken by snapshot-database.sh.
#
#   ./restore-from-snapshot.sh --list
#   ./restore-from-snapshot.sh 20260730T120000Z --dry-run
#   ./restore-from-snapshot.sh 20260730T120000Z Foo.Bar.2016.lpd Baz.Qux.2019.lpd
#   ./restore-from-snapshot.sh latest --all --dry-run
#
# Default is --dry-run OFF only when files are named explicitly or --all is
# given; a bare snapshot argument reports and does nothing. Restoring never
# deletes: existing live files are moved aside into a .pre-restore directory.

LV_TAG=restore
. "$(dirname "${BASH_SOURCE[0]}")/lib/common.sh"

usage() { sed -n '2,/^$/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2; exit "${1:-1}"; }

[ $# -ge 1 ] || usage

if [ "$1" = "--list" ]; then
  printf '%-18s %8s %10s  %s\n' SNAPSHOT FILES SIZE FINGERPRINT
  for s in $(list_snapshots); do
    m="$LIPDVERSE_SNAPSHOTS/$s/meta.json"
    [ -f "$m" ] || continue
    n=$(sed -n 's/.*"n_files": *\([0-9]*\).*/\1/p' "$m")
    b=$(sed -n 's/.*"total_bytes": *\([0-9]*\).*/\1/p' "$m")
    f=$(sed -n 's/.*"fingerprint": *"\([^"]*\)".*/\1/p' "$m")
    printf '%-18s %8s %9sM  %s\n' "$s" "$n" "$(( b / 1048576 ))" "${f:0:12}"
  done
  exit 0
fi

snap="$1"; shift
[ "$snap" = "latest" ] && snap=$(latest_snapshot)
[ -n "$snap" ] || die "no snapshots found in $LIPDVERSE_SNAPSHOTS"

src="$LIPDVERSE_SNAPSHOTS/$snap/db"
[ -d "$src" ] || die "snapshot not found: $LIPDVERSE_SNAPSHOTS/$snap"

DRY_RUN=1
ALL=0
files=()
for a in "$@"; do
  case "$a" in
    --dry-run) DRY_RUN=1 ;;
    --all)     ALL=1; DRY_RUN=0 ;;
    --force)   DRY_RUN=0 ;;
    -*)        usage ;;
    *)         files+=("$a"); DRY_RUN=0 ;;
  esac
done
# An explicit --dry-run anywhere wins.
for a in "$@"; do [ "$a" = "--dry-run" ] && DRY_RUN=1; done

if [ "$ALL" = 1 ]; then
  while IFS= read -r f; do files+=("$f"); done < <(cut -f3 "$LIPDVERSE_SNAPSHOTS/$snap/manifest.tsv")
fi

if [ ${#files[@]} -eq 0 ]; then
  log "snapshot $snap contains $(wc -l < "$LIPDVERSE_SNAPSHOTS/$snap/manifest.tsv" | tr -d ' ') files"
  log "name files to restore, or pass --all"
  exit 0
fi

dest="$LIPDVERSE_DATABASE"
[ -d "$dest" ] || die "live database not found: $dest"

aside="$dest/.pre-restore/$(date -u +%Y%m%dT%H%M%SZ)"
n_same=0 n_diff=0 n_new=0

for f in "${files[@]}"; do
  s="$src/$f"
  [ -f "$s" ] || { warn "not in snapshot, skipping: $f"; continue; }
  d="$dest/$f"
  if [ -f "$d" ]; then
    if [ "$(_md5 "$s")" = "$(_md5 "$d")" ]; then
      n_same=$(( n_same + 1 )); continue
    fi
    n_diff=$(( n_diff + 1 ))
    action="overwrite (live differs)"
  else
    n_new=$(( n_new + 1 ))
    action="restore (missing from live)"
  fi

  if [ "$DRY_RUN" = 1 ]; then
    printf '  %-28s %s\n' "$action" "$f"
  else
    if [ -f "$d" ]; then
      mkdir -p "$aside"
      mv "$d" "$aside/$f"
    fi
    cp -c "$s" "$d" 2>/dev/null || cp "$s" "$d"
  fi
done

if [ "$DRY_RUN" = 1 ]; then
  log "DRY RUN: $n_new would be restored, $n_diff overwritten, $n_same already identical"
  log "re-run with --force (or name files without --dry-run) to apply"
else
  log "restored $n_new, overwrote $n_diff, skipped $n_same identical"
  [ -d "$aside" ] && log "previous live copies moved to $aside"
fi
