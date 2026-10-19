#!/bin/sh
# Compare hunk headers containing valid, invalid and byte-truncated UTF-8.
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
export LC_ALL=C GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_OPTIONAL_LOCKS=0
git init -q "$out/repo"
git -C "$out/repo" config user.name Test
git -C "$out/repo" config user.email test@example.com
git -C "$out/repo" config core.autocrlf false

# The first line stays outside the hunk, so it supplies the function name.
python3 - "$out/repo" <<'PY'
import pathlib
import sys

repo = pathlib.Path(sys.argv[1])
cases = {
    "ascii": b"function ascii",
    "valid": b"function \xc2\x80\xdf\xbf\xe0\xa0\x80\xed\x9f\xbf"
             b"\xef\xbf\xbd\xf0\x90\x80\x80\xf4\x8f\xbf\xbf suffix",
    "split-3-one": b"f" * 79 + b"\xe3\x81\x82 suffix",
    "split-3-two": b"f" * 78 + b"\xe3\x81\x82 suffix",
    "split-2": b"f" * 79 + b"\xc2\xa2 suffix",
    "split-4": b"f" * 77 + b"\xf0\x90\x80\x80 suffix",
    "invalid-ff": b"function before\xffafter",
    "continuation-lead": b"function before\x80after",
    "bad-continuation": b"function before\xe3x\x82after",
    "overlong-2": b"function before\xc0\x80after",
    "overlong-3": b"function before\xe0\x9f\xbfafter",
    "overlong-4": b"function before\xf0\x8f\xbf\xbfafter",
    "surrogate": b"function before\xed\xa0\x80after",
    "noncharacter-fffe": b"function before\xef\xbf\xbeafter",
    "noncharacter-ffff": b"function before\xef\xbf\xbfafter",
    "above-unicode": b"function before\xf4\x90\x80\x80after",
    "lead-f5": b"function before\xf5\x80\x80\x80after",
    "lead-f8": b"function before\xf8\x80\x80\x80after",
    "valid-before-invalid": b"function \xe3\x81\x82 before\xffafter",
}
for name, func in cases.items():
    (repo / name).write_bytes(func + b"\n" + b" context\n" * 5 + b" old\n")
PY
git -C "$out/repo" add .
git -C "$out/repo" commit -qm base
python3 - "$out/repo" <<'PY'
import pathlib
import sys

for path in pathlib.Path(sys.argv[1]).iterdir():
    if path.is_file():
        path.write_bytes(path.read_bytes().replace(b" old\n", b" new\n"))
PY

git -C "$out/repo" diff --no-ext-diff --no-color >"$out/git"
"$root/bin/ngit" -C "$out/repo" diff >"$out/ngit"
expected=$(find "$out/repo" -maxdepth 1 -type f | wc -l)
checked=$(grep -c '^@@ .* @@ ' "$out/git" || true)
fail=0
if [ "$checked" -ne "$expected" ] || [ "$checked" -eq 0 ]; then
  echo "Missing function headers: expected=$expected actual=$checked"
  fail=1
fi
if ! cmp "$out/git" "$out/ngit"; then
  echo "MISMATCH: hunk UTF-8 diff"
  fail=1
fi
echo "hunk-utf8: checked=$checked fail=$fail"
exit "$fail"
