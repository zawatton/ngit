#!/bin/sh
# Wine acceptance entry; all binaries and prefixes are explicitly configurable.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifacts=${NGIT_WINDOWS_TEST_ROOT:-"$root/target/windows-native-20261028"}
exec python3 "$root/test/windows-native.py" \
  --runtime "${NGIT_WINDOWS_TEST_RUNTIME:-$artifacts/runtime/nelisp.exe}" \
  --nelisp-root "${NGIT_WINDOWS_TEST_NELISP_ROOT:-$root/../nelisp}" \
  --dll-dir "${NGIT_WINDOWS_TEST_DLL_DIR:-$artifacts/mingit/ucrt64/bin}" \
  --git "${NGIT_WINDOWS_TEST_GIT:-$artifacts/mingit/cmd/git.exe}" \
  --python "${NGIT_WINDOWS_TEST_PYTHON:-$artifacts/python/python.exe}" \
  --wine "${NGIT_WINDOWS_TEST_WINE:-/usr/lib/wine/wine64}" \
  --wine-prefix "${NGIT_WINDOWS_TEST_PREFIX:-$artifacts/wineprefix}" \
  --artifacts "$artifacts/acceptance-${1:-all}" --case "${1:-all}"
