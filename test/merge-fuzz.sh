#!/bin/sh
# Differential tests of the reusable merge API, in one standalone process.
# Usage: sh test/merge-fuzz.sh [N]; NGIT_FUZZ_SEED0 selects the random seed.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
exec python3 "$here/merge-fuzz.py" "${1:-100}"
