#!/bin/sh
set -eu
ulimit -v 8388608
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
sh "$root/tools/build-elc.sh"
exec python3 "$root/test/ux-commit.py" "$@"
