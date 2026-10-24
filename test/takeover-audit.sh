#!/bin/sh
# Focused regressions for checked refs, SSH exits and receive-pack reports.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
ngit=${NGIT_AUDIT_TOOL:-$root/bin/ngit}
mode=${1:?audit mode required}
fx=${NGIT_FIXTURE_DIR:-$root/target/audit-fixtures}/takeover-$mode
rm -rf "$fx"
mkdir -p "$fx"
export LC_ALL=C GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_NAME=Audit GIT_AUTHOR_EMAIL=audit@example.com
export GIT_COMMITTER_NAME=Audit GIT_COMMITTER_EMAIL=audit@example.com
export GIT_AUTHOR_DATE='1700600000 +0000' GIT_COMMITTER_DATE='1700600000 +0000'
git init -q --template= -b main "$fx/seed"
printf 'one\n' >"$fx/seed/file"
git -C "$fx/seed" add file
git -C "$fx/seed" commit -qm one
old=$(git -C "$fx/seed" rev-parse HEAD)
printf 'two\n' >>"$fx/seed/file"
git -C "$fx/seed" commit -qam two
new=$(git -C "$fx/seed" rev-parse HEAD)
export NGIT_AUDIT_OLD=$old NGIT_AUDIT_NEW=$new
sh "$root/tools/build-elc.sh"
fail=0
reject() {
  label=$1; shift
  rc=0
  timeout 60 "$@" >"$fx/$label.out" 2>"$fx/$label.err" || rc=$?
  printf '%s exit=%s\n' "$label" "$rc"
  if [ "$rc" -eq 0 ] || [ "$rc" -eq 124 ]; then fail=1; fi
}
accept() {
  label=$1; shift
  rc=0
  timeout 60 "$@" >"$fx/$label.out" 2>"$fx/$label.err" || rc=$?
  printf '%s exit=%s\n' "$label" "$rc"
  [ "$rc" -eq 0 ] || fail=1
}
pair() { cp -a "$fx/seed" "$fx/git"; cp -a "$fx/seed" "$fx/ngit"; }
case $mode in
stale-head)
  pair
  for tool in git ngit; do
    d=$fx/$tool
    printf 'three\n' >>"$d/file"
    git -C "$d" add file
    mkdir -p "$d/.git/hooks"
    printf '#!/bin/sh\ngit update-ref HEAD HEAD~1\n' >"$d/.git/hooks/pre-commit"
    chmod +x "$d/.git/hooks/pre-commit"
    cmd=git; [ "$tool" = git ] || cmd=$ngit
    reject "$tool" "$cmd" -C "$d" commit -m three
    [ "$(git -C "$d" rev-parse HEAD)" = "$old" ] || fail=1
    [ ! -e "$d/.git/refs/heads/main.lock" ] || fail=1
    [ ! -e "$d/.git/HEAD.lock" ] || fail=1
  done
  for tool in git ngit; do
    d=$fx/$tool-unborn
    cp -a "$fx/seed" "$d"
    git -C "$d" symbolic-ref HEAD refs/heads/unborn
    printf 'initial change\n' >>"$d/file"
    git -C "$d" add file
    mkdir -p "$d/.git/hooks"
    printf '#!/bin/sh\ngit update-ref HEAD refs/heads/main\n' >"$d/.git/hooks/pre-commit"
    chmod +x "$d/.git/hooks/pre-commit"
    cmd=git; [ "$tool" = git ] || cmd=$ngit
    reject "$tool-unborn" "$cmd" -C "$d" commit -m initial
    [ "$(git -C "$d" rev-parse HEAD)" = "$new" ] || fail=1
    [ ! -e "$d/.git/refs/heads/unborn.lock" ] || fail=1
    [ ! -e "$d/.git/HEAD.lock" ] || fail=1
  done
  ;;
stale-ref)
  pair
  for packed in loose packed; do
    if [ "$packed" = packed ]; then
      git -C "$fx/git" pack-refs --all
      git -C "$fx/ngit" pack-refs --all
    fi
    reject "git-$packed" git -C "$fx/git" update-ref refs/heads/main "$old" "$old"
    NGIT_AUDIT_REPO=$fx/ngit; export NGIT_AUDIT_REPO
    reject "ngit-$packed" "${NGIT_NELISP:-nelisp}" --load "$root/test/takeover-ref.el"
    grep -q 'cannot lock ref' "$fx/ngit-$packed.out" || fail=1
    NGIT_AUDIT_CASE=noop; export NGIT_AUDIT_CASE
    reject "git-$packed-noop" git -C "$fx/git" update-ref refs/heads/main "$new" "$old"
    reject "ngit-$packed-noop" "${NGIT_NELISP:-nelisp}" --load "$root/test/takeover-ref.el"
    grep -q 'cannot lock ref' "$fx/ngit-$packed-noop.out" || fail=1
    NGIT_AUDIT_CASE=controls; export NGIT_AUDIT_CASE
    accept "ngit-$packed-controls" "${NGIT_NELISP:-nelisp}" --load "$root/test/takeover-ref.el"
    git -C "$fx/ngit" update-ref -d refs/heads/audit
    unset NGIT_AUDIT_CASE
    [ "$(git -C "$fx/ngit" rev-parse HEAD)" = "$new" ] || fail=1
    [ ! -e "$fx/ngit/.git/refs/heads/main.lock" ] || fail=1
    [ "$packed" != packed ] || [ ! -e "$fx/ngit/.git/refs/heads/main" ] || fail=1
  done
  ;;
ssh-exit|ssh-write-exit)
  cat >"$fx/ssh" <<'SH'
#!/bin/sh
case $1 in -G) exit 1;; esac
for command do :; done
eval "$command"
exit 42
SH
  chmod +x "$fx/ssh"
  export GIT_SSH_COMMAND=$fx/ssh
  git clone -q --bare "$fx/seed" "$fx/server.git"
  url=audit-host:$fx/server.git
  if [ "$mode" = ssh-exit ]; then
    # Git accepts a complete advertisement even when ssh later fails.
    accept git git ls-remote "$url"
    accept ngit "$ngit" ls-remote "$url"
    cmp -s "$fx/git.out" "$fx/ngit.out" || fail=1
  else
    pair
    for tool in git ngit; do
      cmd=git; [ "$tool" = git ] || cmd=$ngit
      reject "$tool-clone" "$cmd" clone -q "$url" "$fx/$tool-clone"
      accept "$tool-fetch-current" "$cmd" -C "$fx/$tool" fetch "$url" main
      git clone -q --bare "$fx/seed" "$fx/$tool-old.git"
      git --git-dir="$fx/$tool-old.git" update-ref refs/heads/main "$old"
      git --git-dir="$fx/$tool-old.git" reflog expire --expire=all --all
      git --git-dir="$fx/$tool-old.git" prune --expire=now
      reject "$tool-fetch-new" "$cmd" -C "$fx/$tool-old.git" fetch "$url" main
      reject "$tool-push" "$cmd" -C "$fx/$tool" push "$url" HEAD:refs/heads/new-$tool
      # A failed SSH exit does not undo an update already accepted remotely.
      [ "$(git --git-dir="$fx/server.git" rev-parse "refs/heads/new-$tool")" = "$new" ] || fail=1
    done
  fi
  ;;
push-missing-report|push-invalid-report)
  pair
  git clone -q --bare "$fx/seed" "$fx/server.git"
  git --git-dir="$fx/server.git" update-ref refs/heads/main "$old"
  cat >"$fx/ssh" <<'SH'
#!/bin/sh
case $1 in -G) exit 1;; esac
exec python3 "$NGIT_AUDIT_RECEIVER" "$NGIT_AUDIT_SERVER" "$NGIT_AUDIT_REPORT"
SH
  chmod +x "$fx/ssh"
  export GIT_SSH_COMMAND=$fx/ssh NGIT_AUDIT_RECEIVER=$root/test/takeover-receiver.py
  export NGIT_AUDIT_SERVER=$fx/server.git
  reports=missing
  [ "$mode" = push-missing-report ] || reports='partial duplicate unknown malformed duplicate-complete unknown-complete'
  for report in $reports; do
    export NGIT_AUDIT_REPORT=$report
    for tool in git ngit; do
      d=$fx/$tool
      git -C "$d" config remote.origin.url "audit-host:$fx/server.git"
      git -C "$d" config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
      git -C "$d" update-ref refs/remotes/origin/main "$old"
      git -C "$d" update-ref refs/remotes/origin/topic "$old"
      cmd=git; [ "$tool" = git ] || cmd=$ngit
      case $report in
        *-complete) accept "$tool-$report" "$cmd" -C "$d" push origin HEAD:main HEAD:topic ;;
        *) reject "$tool-$report" "$cmd" -C "$d" push origin HEAD:main HEAD:topic ;;
      esac
      if [ "$report" = unknown-complete ]; then
        grep -q 'remote reported status on unknown ref' "$fx/$tool-$report.err" || fail=1
      fi
      # An invalid report must never be used to publish tracking updates.
      if [ "$tool" = ngit ]; then
        expected=$old
        case $report in *-complete) expected=$new ;; esac
        [ "$(git -C "$d" rev-parse refs/remotes/origin/main)" = "$expected" ] || fail=1
        [ "$(git -C "$d" rev-parse refs/remotes/origin/topic)" = "$expected" ] || fail=1
      fi
    done
  done
  ;;
*) echo "unknown mode: $mode" >&2; exit 2;;
esac
[ "$fail" -eq 0 ]
