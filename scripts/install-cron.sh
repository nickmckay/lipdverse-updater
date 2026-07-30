#!/bin/bash
#
# Install (or remove) the nightly Stage 0 backup jobs.
#
# Uses launchd rather than cron: on macOS, launchd runs missed jobs when the
# machine wakes, whereas a cron entry scheduled for 02:00 simply never fires if
# the laptop was asleep. These are backups; silently not running is the one
# failure mode that matters.
#
#   ./scripts/install-cron.sh            # install and load
#   ./scripts/install-cron.sh --status   # show state and recent log tail
#   ./scripts/install-cron.sh --uninstall

set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(dirname "$here")"

AGENTS="$HOME/Library/LaunchAgents"
LOGDIR="$HOME/Library/Logs/lipdverse-updater"
DB_LABEL=org.lipdverse.updater.snapshot-database
QC_LABEL=org.lipdverse.updater.snapshot-qc-sheets

case "${1:-install}" in
  --status)
    for l in "$DB_LABEL" "$QC_LABEL"; do
      printf '%s: ' "$l"
      launchctl list | grep -q "$l" && echo "loaded" || echo "NOT LOADED"
    done
    echo
    for f in "$LOGDIR"/*.log; do
      [ -f "$f" ] || continue
      echo "--- $(basename "$f") (last 5) ---"
      tail -5 "$f"
    done
    exit 0
    ;;
  --uninstall)
    for l in "$DB_LABEL" "$QC_LABEL"; do
      launchctl unload "$AGENTS/$l.plist" 2>/dev/null || true
      rm -f "$AGENTS/$l.plist"
      echo "removed $l"
    done
    exit 0
    ;;
  install) ;;
  *) echo "usage: $0 [install|--status|--uninstall]" >&2; exit 1 ;;
esac

mkdir -p "$AGENTS" "$LOGDIR"

# $1 label  $2 program  $3 hour  $4 minute
write_plist() {
  cat > "$AGENTS/$1.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>$1</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/bash</string>
    <string>-lc</string>
    <string>$2</string>
  </array>
  <key>StartCalendarInterval</key>
  <dict>
    <key>Hour</key><integer>$3</integer>
    <key>Minute</key><integer>$4</integer>
  </dict>
  <key>StandardOutPath</key><string>$LOGDIR/$1.log</string>
  <key>StandardErrorPath</key><string>$LOGDIR/$1.log</string>
  <key>RunAtLoad</key><false/>
</dict>
</plist>
PLIST
  launchctl unload "$AGENTS/$1.plist" 2>/dev/null || true
  launchctl load "$AGENTS/$1.plist"
  echo "installed $1 (daily at $3:$(printf '%02d' "$4"))"
}

# Database snapshot first; QC sheets 30 min later so a slow Sheets run cannot
# delay the cheap, most-critical job.
write_plist "$DB_LABEL" "$repo/scripts/snapshot-database.sh"   2 0
write_plist "$QC_LABEL" "$repo/scripts/snapshot-qc-sheets.R"   2 30

echo
echo "logs: $LOGDIR"
echo "check with: $0 --status"
