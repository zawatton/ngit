#!/bin/sh
# Differential file-rename merges; each partition is a ledger criterion.
set -eu
ulimit -v 8388608
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
sh "$root/tools/build-elc.sh"
exec python3 "$root/test/merge-rename-check.py" "$@"
