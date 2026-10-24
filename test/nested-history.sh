#!/bin/sh
# Build this checkout and run the exact nested history differential checks.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
sh "$root/tools/build-elc.sh"
exec python3 "$root/test/nested-history.py" "$@"
