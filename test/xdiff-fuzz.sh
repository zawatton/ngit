#!/bin/sh
# Differential fuzz test: compare ngit-xdiff-unified against
# `git diff --no-index` on N deterministic pseudo-random file pairs.
#
# Usage: sh test/xdiff-fuzz.sh [N]
# Environment: NGIT_NELISP (default: nelisp), NGIT_FUZZ_SEED0 (default: 1).
# Exit status 0 iff every case matches.

n=${1:-100}
seed0=${NGIT_FUZZ_SEED0:-1}
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
nelisp=${NGIT_NELISP:-nelisp}

tmp=$(mktemp -d) || exit 1
trap 'rm -rf "$tmp"' EXIT INT TERM

# Pin the oracle to git's defaults regardless of the user's configuration.
GIT_CONFIG_NOSYSTEM=1
GIT_CONFIG_GLOBAL=/dev/null
export GIT_CONFIG_NOSYSTEM GIT_CONFIG_GLOBAL

gen() {
    # gen SEED DIR -> writes DIR/a DIR/b, prints the context length.
    awk -v seed="$1" -v dir="$2" '
    function rnd(k) { return int(rand() * k) }
    function longline(   s, m) {
        s = "long_identifier_" rnd(3) "_"
        m = 70 + rnd(60)
        while (length(s) < m) s = s "abcdefghij"
        if (rand() < 0.3) s = s "   "
        return s
    }
    function mkline(   t, ind, k) {
        t = rnd(100)
        if (t < 12) return ""
        if (t < 15) return (rnd(2) ? "   " : "\t")
        if (t < 18) return longline()
        ind = ""
        k = rnd(5)
        if (rnd(4) == 0) ind = substr("\t\t\t", 1, rnd(3) + 1)
        else ind = substr("                ", 1, k * (rnd(2) ? 4 : 2))
        if (t < 30) return ind "}"
        if (t < 38) return "func_" rnd(alpha) "() {"
        if (t < 42) return "$var" rnd(alpha) " = " rnd(4)
        if (t < 46) return "_priv" rnd(alpha) ":"
        if (t < 52) return "# comment " rnd(alpha)
        if (t < 56) return "Label" rnd(alpha) "  "
        if (t < 60) return "x" rnd(alpha) "\r"
        return ind "stmt" rnd(alpha) ";"
    }
    function block(arr, cnt,   i, k, kind) {
        # Fill arr[1..cnt] with lines; sometimes structured functions.
        i = 0
        while (i < cnt) {
            kind = rnd(4)
            if (kind == 0 && i + 5 <= cnt) {
                arr[++i] = "func_" rnd(alpha) "() {"
                arr[++i] = "    stmt" rnd(alpha) ";"
                arr[++i] = "    stmt" rnd(alpha) ";"
                arr[++i] = "}"
                arr[++i] = ""
            } else {
                arr[++i] = mkline()
            }
        }
    }
    BEGIN {
        srand(seed)
        alpha = 2 + rnd(12)
        mode = rnd(20)
        na = (rnd(4) == 0) ? rnd(400) : rnd(60)
        if (mode == 0) na = 0
        block(A, na)
        nb = 0
        if (mode == 1) {
            nb = 0
        } else if (mode == 2) {
            for (i = 1; i <= na; i++) B[++nb] = A[i]
        } else {
            ops = 1 + rnd(8)
            for (i = 1; i <= na; i++) T[i] = A[i]
            nt = na
            for (o = 0; o < ops; o++) {
                op = rnd(5)
                p = (nt > 0) ? 1 + rnd(nt) : 1
                len = 1 + rnd(rnd(3) == 0 ? 30 : 5)
                nn = 0
                delete U
                if (op == 0) {            # delete block
                    for (i = 1; i <= nt; i++) if (i < p || i >= p + len) U[++nn] = T[i]
                } else if (op == 1) {     # insert block
                    delete X; block(X, len)
                    for (i = 1; i < p && i <= nt; i++) U[++nn] = T[i]
                    for (j = 1; j <= len; j++) U[++nn] = X[j]
                    for (; i <= nt; i++) U[++nn] = T[i]
                } else if (op == 2) {     # replace block
                    delete X; block(X, 1 + rnd(len + 1))
                    for (i = 1; i < p && i <= nt; i++) U[++nn] = T[i]
                    for (j = 1; j in X; j++) U[++nn] = X[j]
                    for (i = p + len; i <= nt; i++) U[++nn] = T[i]
                } else if (op == 3) {     # move block to the end/front
                    delete X; nx = 0
                    for (i = p; i < p + len && i <= nt; i++) X[++nx] = T[i]
                    if (rnd(2)) for (j = 1; j <= nx; j++) U[++nn] = X[j]
                    for (i = 1; i <= nt; i++) if (i < p || i >= p + len) U[++nn] = T[i]
                    if (!(1 in U) || nn < nt) for (j = 1; j <= nx && nn < nt; j++) U[++nn] = X[j]
                } else {                  # duplicate a block in place
                    for (i = 1; i <= nt; i++) {
                        U[++nn] = T[i]
                        if (i == p) for (j = p; j < p + len && j <= nt; j++) U[++nn] = T[j]
                    }
                }
                delete T
                for (i = 1; i <= nn; i++) T[i] = U[i]
                nt = nn
            }
            for (i = 1; i <= nt; i++) B[++nb] = T[i]
        }
        ea = (na > 0 && rnd(5) == 0) ? "" : "\n"
        eb = (nb > 0 && rnd(5) == 0) ? "" : "\n"
        if (rnd(10) == 0) eb = ea
        fa = dir "/a"; fb = dir "/b"
        printf "" > fa; printf "" > fb
        for (i = 1; i <= na; i++) printf "%s%s", A[i], (i < na ? "\n" : ea) > fa
        for (i = 1; i <= nb; i++) printf "%s%s", B[i], (i < nb ? "\n" : eb) > fb
        close(fa); close(fb)
        c = rnd(10)
        print (c == 0 ? 0 : c == 1 ? 1 : c == 2 ? 5 : 3)
    }'
}

list="$tmp/list"
: > "$list"
i=1
while [ "$i" -le "$n" ]; do
    seed=$((seed0 + i * 7919))
    d="$tmp/c$i"
    mkdir "$d"
    ctx=$(gen "$seed" "$d")
    git diff --no-index -U"$ctx" "$d/a" "$d/b" | sed -n '/^@@/,$p' > "$d/expect"
    echo "$d/a $d/b $ctx" >> "$list"
    echo "$seed" > "$d/seed"
    i=$((i + 1))
done

start=$(date +%s)
"$nelisp" --load "$root/test/xdiff-driver.el" -- --list "$list" > "$tmp/out"
rc=$?
end=$(date +%s)
if [ "$rc" -ne 0 ]; then
    echo "driver exited with status $rc"
    tail -n 3 "$tmp/out"
fi

# Split the driver output into one file per case.
awk -v tmp="$tmp" '
    $0 == "@@@NGIT-XDIFF-CASE@@@" { if (f) close(f); k++; f = tmp "/c" k "/actual"; printf "" > f; next }
    { print > f }
' "$tmp/out"

fail=0
i=1
while [ "$i" -le "$n" ]; do
    d="$tmp/c$i"
    [ -f "$d/actual" ] || : > "$d/actual"
    if ! cmp -s "$d/expect" "$d/actual"; then
        fail=$((fail + 1))
        echo "MISMATCH case $i seed $(cat "$d/seed")"
        diff "$d/expect" "$d/actual" | head -n 12
    fi
    i=$((i + 1))
done

echo "xdiff-fuzz: $((n - fail))/$n cases identical (driver $((end - start))s)"
[ "$fail" -eq 0 ] && [ "$rc" -eq 0 ]
