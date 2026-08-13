#!/usr/bin/env bash
# Checks that need the real corpus, so they run here rather than in CI.
#
#   ./scripts/nightly-checks.sh
#
# Exits non-zero if anything drifted. Intended for cron alongside the snapshot
# jobs; see scripts/install-cron.sh.
set -uo pipefail
cd "$(dirname "$0")/.." || exit 2
fail=0

echo "== unit suite =="
Rscript -e 'devtools::test(stop_on_failure = TRUE)' || fail=1

echo "== registry regenerable from its review files =="
Rscript scripts/check-registry-regenerable.R || fail=1

echo "== replay profile over the recorded version history =="
Rscript scripts/check-replay-profile.R || fail=1

exit $fail
