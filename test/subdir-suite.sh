#!/bin/sh
# Compare subdirectory commands with the installed Git oracle.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
sh "$root/tools/build-elc.sh"
exec python3 "$root/test/subdir-suite.py" "$@"
