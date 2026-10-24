#!/bin/sh
set -eu
ulimit -v 8388608
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
exec python3 "$root/test/credential-helpers.py" "$@"
