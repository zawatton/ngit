#!/bin/sh
# Build this worktree and compare literal nested ls-tree paths with Git.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
sh "$root/tools/build-elc.sh"
exec python3 "$root/test/nested-tree.py" "$@"
