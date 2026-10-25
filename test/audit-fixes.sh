#!/bin/sh
set -eu
ulimit -v 8388608
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
sh "$here/tools/build-elc.sh"
exec python3 "$here/test/audit-fixes.py" "$@"
