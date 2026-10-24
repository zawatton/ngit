#!/bin/sh
# Run under the workspace heavy-job gate.  Every mode is independently bounded.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
case ${1:-} in
  policy|cleanup) exec timeout 90 python3 "$root/test/transport-memory-tests.py" "$1" ;;
  measurement) exec timeout 360 python3 "$root/test/transport-memory-measurement.py" ;;
  e2e)
    : "${NGIT_REAL_REPO:?set NGIT_REAL_REPO to the real source repository}"
    export NGIT_MEMORY_BUDGET_MIB=${NGIT_MEMORY_BUDGET_MIB:-8192}
    wrapper="$root/target/memory-lane/git-wrapper"
    mkdir -p "$wrapper"
    export NGIT_MEMORY_REAL_GIT=$(command -v git)
    export NGIT_MEMORY_FSCK_LOG="$root/target/memory-lane/e2e-fsck.log"
    : >"$NGIT_MEMORY_FSCK_LOG"
    cp "$root/test/transport-memory-git.py" "$wrapper/git"
    chmod +x "$wrapper/git"
    export PATH="$wrapper:$PATH"
    echo "transport-memory real e2e: aggregate budget ${NGIT_MEMORY_BUDGET_MIB} MiB; ngit cumulative limit 480s"
    started=$(date +%s)
    timeout 900 sh "$root/test/real-write.sh" "$NGIT_REAL_REPO" transport 480
    [ "$(wc -l <"$NGIT_MEMORY_FSCK_LOG")" -ge 2 ]
    echo "transport-memory e2e wall: $(($(date +%s) - started))s including full fsck; internal harness limit 900s"
    ;;
  *) echo "usage: $0 policy|cleanup|measurement|e2e" >&2; exit 2 ;;
esac
