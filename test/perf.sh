#!/bin/sh
# Time one ngit command in REPO and fail when it exceeds LIMIT seconds.
# With a trailing --compare, its stdout must also equal git's.
# Usage: test/perf.sh REPO LIMIT ARGS... [--compare]
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
repo=$1 limit=$2
shift 2
compare=
for a in "$@"; do [ "$a" = --compare ] && compare=1; done
args=
for a in "$@"; do [ "$a" = --compare ] || args="$args $a"; done
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
start=$(date +%s.%N)
# shellcheck disable=SC2086
"$root/bin/ngit" -C "$repo" $args >"$out/ngit" || { echo "ngit failed"; exit 1; }
end=$(date +%s.%N)
elapsed=$(echo "$end - $start" | bc)
echo "ngit$args: ${elapsed}s (limit ${limit}s)"
if [ -n "$compare" ]; then
  # shellcheck disable=SC2086
  git -C "$repo" $args >"$out/git"
  cmp -s "$out/git" "$out/ngit" || { echo "output differs from git"; exit 1; }
fi
[ "$(echo "$elapsed <= $limit" | bc)" = 1 ]
