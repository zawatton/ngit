#!/bin/sh
# Read-only log and branch commands on the real repository REPO, against git.
#   walk   - full-history log --oneline, and log --format=fuller -n 300
#   diff   - log -n 50 --stat, log --oneline -- FILE and -- DIR
#   graph  - log --graph --oneline -n 500
#   branch - branch -a -vv and branch --merged
# Every ngit run's stdout and exit status must equal git's, and ngit's runs
# must take at most LIMIT seconds in total.
# Usage: test/real-log.sh REPO MODE LIMIT
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
repo=$1 mode=$2 limit=$3
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
export GIT_OPTIONAL_LOCKS=0
spent=0 fail=0 n=0
run() { # run ARGS...: compare one command, add ngit's time to $spent
  n=$((n + 1))
  git -C "$repo" "$@" >"$out/git.$n" 2>/dev/null; gs=$?
  s=$(date +%s.%N)
  "$ngit" -C "$repo" "$@" >"$out/ngit.$n" 2>"$out/err"; ns=$?
  e=$(date +%s.%N)
  spent=$(echo "$spent + $e - $s" | bc)
  echo "ngit $*: $(echo "$e - $s" | bc)s"
  if [ $gs -ne $ns ] || ! cmp -s "$out/git.$n" "$out/ngit.$n"; then
    echo "MISMATCH: $* [git=$gs ngit=$ns]"
    diff "$out/git.$n" "$out/ngit.$n" | head -6; head -3 "$out/err"; fail=1
  fi
}
# Pin the history so commits made while the test runs do not matter.
head=$(git -C "$repo" rev-parse HEAD) || exit 2
case $mode in
  walk)
    run log --oneline "$head"
    run log --format=fuller -n 300 "$head" ;;
  diff)
    file=$(git -C "$repo" log --format= --name-only -n 200 "$head" | sort | uniq -c | sort -rn | awk 'NR==1 {print $2}')
    dir=$(dirname "$file")
    run log -n 50 --stat "$head"
    run log --oneline "$head" -- "$file"
    run log --oneline "$head" -- "$dir" ;;
  graph)
    run log --graph --oneline -n 500 "$head" ;;
  branch)
    run branch -a -vv
    run branch --merged "$head" ;;
  *) echo "unknown mode $mode"; exit 2 ;;
esac
echo "ngit total: ${spent}s (limit ${limit}s)"
[ $fail = 0 ] && [ "$(echo "$spent <= $limit" | bc)" = 1 ]
