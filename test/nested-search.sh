#!/bin/sh
set -eu
ulimit -v 8388608
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
common=$(git -C "$root" rev-parse --path-format=absolute --git-common-dir)
notes=${NOTES_ROOT:-$(CDPATH= cd -- "$common/../../.." && pwd)}
export NGIT_NELISP_ROOT=${NGIT_NELISP_ROOT:-$notes/dev/nelisp}
export NGIT_NELISP=${NGIT_NELISP:-$(command -v nelisp)}
exec "$notes/bin/heavy-run.sh" sh -c 'sh "$1/tools/build-elc.sh" && exec python3 "$1/test/nested-search.py" "$2"' sh "$root" "${1:-e2e}"
