#!/bin/sh
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
ulimit -v 8388608
exec python3 "$here/test/config-includes.py" "${1:-read}"
