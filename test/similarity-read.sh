#!/bin/sh
set -eu
ulimit -v 8388608
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
exec python3 "$here/test/similarity-read.py" "$@"
