#!/bin/sh
# Generic recorder installed under a hook name by hooks-suite/real-hooks.
name=${0##*/}
phase=${1:-}
prefix=$(cat "$(dirname "$0")/.ngit-hook-prefix" 2>/dev/null || printf local)
log=${NGIT_HOOK_LOG:?NGIT_HOOK_LOG is required by the hook comparison suite}
{
  printf '%s:%s %s\n' "$prefix" "$0" "$*"
  printf 'cwd %s\n' "$(pwd -P)"
  for argument in "$@"; do printf 'arg: <%s>\n' "$argument"; done
  for key in GIT_DIR GIT_WORK_TREE GIT_PREFIX GIT_INDEX_FILE GIT_EDITOR \
             GIT_REFLOG_ACTION GIT_PUSH_OPTION_COUNT GIT_PUSH_OPTION_0 \
             GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL GIT_AUTHOR_DATE \
             GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL GIT_COMMITTER_DATE; do
    eval "present=\${$key+x}; value=\${$key-}"
    [ -z "$present" ] || printf 'env %s=%s\n' "$key" "$value"
  done
  printf 'stdin-begin\n'
  sed 's/^/stdin:/'
  printf 'stdin-end\n'
  if [ "$name" = reference-transaction ] && [ "$phase" = aborted ] &&
     [ -n "${NGIT_HOOK_RECORD_LOCKS:-}" ]; then
    printf 'locks-begin\n'
    (cd "$(git rev-parse --absolute-git-dir)" && find . -name '*.lock' | LC_ALL=C sort)
    printf 'locks-end\n'
  fi
} >>"$log"
printf 'hook stdout %s:%s\n' "$prefix" "$name"
printf 'hook stderr %s:%s\n' "$prefix" "$name" >&2
if [ "$name" = reference-transaction ] && [ "$phase" = prepared ] &&
   [ -n "${NGIT_HOOK_FAIL_PREPARED_N:-}" ]; then
  counter="$log.prepared"
  count=$(cat "$counter" 2>/dev/null || printf 0)
  count=$((count + 1))
  printf '%s\n' "$count" >"$counter"
  [ "$count" -ne "$NGIT_HOOK_FAIL_PREPARED_N" ] || exit 1
fi
if [ "${NGIT_HOOK_EDIT:-}" = "$name" ] && [ -n "${1:-}" ] && [ -f "$1" ]; then
  printf '\nhook edited by %s\n' "$name" >>"$1"
fi
if [ "${NGIT_HOOK_STAGE:-}" = "$name" ]; then
  printf 'hook staged by %s\n' "$name" >>"${NGIT_HOOK_STAGE_FILE:?}"
  git add -- "$NGIT_HOOK_STAGE_FILE" || exit 1
fi
case ${NGIT_HOOK_FAIL:-} in
  "$name"|"$name:$phase") exit "${NGIT_HOOK_FAIL_STATUS:-1}" ;;
esac
exit 0
