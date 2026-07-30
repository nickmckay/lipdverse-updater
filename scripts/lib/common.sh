# Shared helpers for the lipdverse-updater Stage 0 scripts.
# Sourced, not executed.

set -euo pipefail

: "${LIPDVERSE_DATABASE:=$HOME/Dropbox/lipdverse/database}"
# Snapshots deliberately live OUTSIDE Dropbox: they must not be synced, and
# clonefile savings vanish if Dropbox rehydrates each clone as a real file.
: "${LIPDVERSE_SNAPSHOTS:=$HOME/lipdverse-snapshots}"
: "${LIPDVERSE_QCSTORE:=$HOME/GitHub/lipdverse-qcstore}"
: "${LIPDVERSE_SNAPSHOT_RETAIN_DAYS:=30}"
: "${LIPDVERSE_SNAPSHOT_MIN_KEEP:=7}"

log()  { printf '%s [%s] %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "${LV_TAG:-lv}" "$*" >&2; }
die()  { log "ERROR: $*"; exit 1; }
warn() { log "WARN: $*"; }

# md5sum is GNU-only; macOS ships `md5 -q`. Normalize.
if command -v md5sum >/dev/null 2>&1; then
  _md5() { md5sum "$1" | cut -d' ' -f1; }
else
  _md5() { /sbin/md5 -q "$1"; }
fi
export -f _md5 2>/dev/null || true

# Number of parallel hashing jobs.
: "${LV_JOBS:=$( (sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4) )}"

# Emit "<md5>\t<bytes>\t<relpath>" for every *.lpd under $1, sorted by relpath.
# Sorting is byte-wise (LC_ALL=C) so manifests are comparable across machines.
manifest_of() {
  local root="$1"
  ( cd "$root" && find . -name '*.lpd' -type f -print0 \
      | LC_ALL=C sort -z \
      | xargs -0 -P "$LV_JOBS" -I{} sh -c '
          f="$1"
          if command -v md5sum >/dev/null 2>&1; then h=$(md5sum "$f" | cut -d" " -f1); else h=$(/sbin/md5 -q "$f"); fi
          s=$(wc -c < "$f" | tr -d " ")
          printf "%s\t%s\t%s\n" "$h" "$s" "${f#./}"
        ' _ {} \
      | LC_ALL=C sort -t"$(printf '\t')" -k3,3 )
}

# Fingerprint of a manifest = md5 of its "<md5>  <relpath>" projection.
# Deliberately excludes size and mtime, so a touched-but-unchanged file does
# not change the fingerprint (the defect in lipdverseR's zip-based directoryMD5).
fingerprint_of() {
  cut -f1,3 "$1" | _md5_stdin
}
_md5_stdin() {
  if command -v md5sum >/dev/null 2>&1; then md5sum | cut -d' ' -f1; else /sbin/md5 -q; fi
}

# All snapshot directory names, oldest first. Empty (status 0) if there are none:
# callers assign this in command substitutions under `set -e`, so it must not
# inherit grep's "no match" exit status.
list_snapshots() {
  # Returns 0 with no output when the snapshot root does not exist yet. Both
  # the missing directory and grep's "no match" would otherwise abort the
  # caller under `set -e` / `pipefail`.
  [ -d "$LIPDVERSE_SNAPSHOTS" ] || return 0
  find "$LIPDVERSE_SNAPSHOTS" -maxdepth 1 -type d -name '????????T??????Z' -exec basename {} \; \
    | LC_ALL=C sort
}

# Most recent snapshot directory name, or empty.
latest_snapshot() {
  list_snapshots | tail -1
}
