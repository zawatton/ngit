#!/bin/sh
# Byte-compile lisp/*.el with the host Emacs into lisp/*.elc, then mark the
# set fresh with lisp/.elc-stamp.  ngit-main.el uses the byte code only
# while the stamp is newer than every source file; without Emacs, ngit
# keeps running from source.  Any changed source rebuilds every module,
# since a macro change in one file affects the byte code of others.
here=$(cd "$(dirname "$0")/.." && pwd)
lisp=$here/lisp
emacs=${EMACS:-emacs}
stamp=$lisp/.elc-stamp

fresh() {
  [ -f "$stamp" ] && [ -z "$(find "$lisp" -maxdepth 1 -name '*.el' -newer "$stamp")" ]
}

fresh && exit 0
command -v "$emacs" >/dev/null 2>&1 || {
  echo "build-elc: $emacs not found; ngit runs from source" >&2
  exit 0
}
# Concurrent test runs share the tree; one builds while the others wait.
lock=$lisp/.elc-lock
until mkdir "$lock" 2>/dev/null; do sleep 1; done
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"; rmdir "$lock"' EXIT
fresh && exit 0

# The stamp takes the time the build started, so an edit made during the
# build still counts as newer.
touch "$tmp/stamp"
# Sources that must stay source: the entry point and profiler are loaded
# by path, ngit-native wraps nl-ffi definitions at load time, and the
# kernel source is read as text for native compilation.
set --
for f in "$lisp"/*.el; do
  case ${f##*/} in
    ngit-main.el|ngit-profile.el|ngit-native.el|ngit-transport-kernel-source.el) ;;
    *) set -- "$@" "$f" ;;
  esac
done
if ! "$emacs" --batch -Q -L "$lisp" -l "$here/tools/compile.el" "$tmp" "$@" \
     >"$tmp/log" 2>&1; then
  grep -E 'Error|FAILED' "$tmp/log" >&2
  echo "build-elc: compilation failed; ngit runs from source" >&2
  exit 1
fi
mv "$tmp"/*.elc "$lisp"/
mv "$tmp/stamp" "$stamp"
