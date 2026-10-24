#!/bin/sh
# Differential lease/atomic acceptance. Every mode must execute real pushes.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
mode=${1:?explicit|tracking|atomic|e2e required}
case $mode in explicit|tracking|atomic|e2e) ;; *) exit 2;; esac
ngit=${NGIT_PUSH_TOOL:-$root/bin/ngit}
fx=${NGIT_FIXTURE_DIR:-$root/target/push-lane/fixtures}/push-safety-$mode
out=${NGIT_PUSH_OUTPUT:-$root/target/push-lane}/$mode
mkdir -p "$out"
rm -rf "$fx"; mkdir -p "$fx"
export LC_ALL=C TZ=UTC GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_NAME=Safety GIT_AUTHOR_EMAIL=safety@example.com
export GIT_COMMITTER_NAME=Safety GIT_COMMITTER_EMAIL=safety@example.com
export GIT_AUTHOR_DATE='1700600000 +0000' GIT_COMMITTER_DATE='1700600000 +0000'
export GIT_TERMINAL_PROMPT=0
unset GIT_SSH_COMMAND GIT_SSH GIT_SSH_VARIANT || :
sh "$root/tools/build-elc.sh"
git init -q --template= -b main "$fx/seed"
printf 'one\n' >"$fx/seed/f"
git -C "$fx/seed" add f; git -C "$fx/seed" commit -qm one
a=$(git -C "$fx/seed" rev-parse HEAD)
printf 'two\n' >>"$fx/seed/f"; git -C "$fx/seed" commit -qam two
b=$(git -C "$fx/seed" rev-parse HEAD)
printf 'three\n' >>"$fx/seed/f"; git -C "$fx/seed" commit -qam three
c=$(git -C "$fx/seed" rev-parse HEAD)
git -C "$fx/seed" checkout -q --detach "$a"
printf 'other\n' >"$fx/seed/g"; git -C "$fx/seed" add g; git -C "$fx/seed" commit -qm other
d=$(git -C "$fx/seed" rev-parse HEAD)
git -C "$fx/seed" update-ref refs/heads/main "$d"
git -C "$fx/seed" update-ref refs/heads/topic "$b"
git -C "$fx/seed" symbolic-ref HEAD refs/heads/main
cat >"$fx/ssh" <<'EOF'
#!/bin/sh
while [ "$#" -gt 0 ]; do
  case $1 in -G) exit 0;; -o|-p|-i) shift 2;; -*) shift;; *) shift; break;; esac
done
exec sh -c "$*"
EOF
chmod +x "$fx/ssh"
export GIT_SSH=$fx/ssh GIT_SSH_VARIANT=ssh
count=0 fail=0
normalize() { sed "s|$fx/git-|$fx/tool-|g; s|$fx/ngit-|$fx/tool-|g; s|$fx|@FX@|g" "$1" >"$2"; }
same() {
  if ! cmp -s "$2" "$3"; then
    echo "FAIL $1"; diff -u "$2" "$3" | head -35 || :; fail=1
  fi
}
state() {
  git -C "$1" for-each-ref --format='%(refname) %(objectname)' >"$2.raw"
  gd=$(git -C "$1" rev-parse --absolute-git-dir)
  if [ -d "$gd/logs" ]; then
    (cd "$gd"; find logs -type f | sort | while IFS= read -r f; do echo "$f"; cat "$f"; done) >>"$2.raw"
  fi
  if [ -f "$gd/push-hook-input" ]; then
    echo '== pre-push input' >>"$2.raw"; cat "$gd/push-hook-input" >>"$2.raw"
  fi
  normalize "$2.raw" "$2"
  git -C "$1" fsck --full --strict --no-progress >"$2.fsck" 2>&1 || { cat "$2.fsck"; fail=1; }
}
prepare() {
  label=$1 scenario=$2 transport=$3
  for tool in git ngit; do
    repo=$fx/$tool-$label; remote=$repo.git
    cp -a "$fx/seed" "$repo"
    git clone -q --bare --template= "$fx/seed" "$remote"
    git --git-dir="$remote" config core.logAllRefUpdates true
    git --git-dir="$remote" config receive.denyDeleteCurrent ignore
    git --git-dir="$remote" update-ref refs/heads/main "$b"
    git --git-dir="$remote" update-ref refs/heads/topic "$a"
    url=$remote
    case $transport in file) url=file://$remote;; ssh) url=safety:$remote;; esac
    git -C "$repo" config remote.peer.url "$url"
    git -C "$repo" config remote.peer.fetch '+refs/heads/*:refs/remotes/review/*'
    git -C "$repo" update-ref refs/remotes/review/main "$b"
    git -C "$repo" update-ref refs/remotes/review/topic "$a"
    case $scenario in
      missing) git -C "$repo" update-ref -d refs/remotes/review/main;;
      stale) git -C "$repo" update-ref refs/remotes/review/main "$a";;
      uptodate) git -C "$repo" update-ref refs/heads/main "$b";;
      ff) git -C "$repo" update-ref refs/heads/main "$c";;
      mapping) git -C "$repo" config --unset remote.peer.fetch;;
      exact) git -C "$repo" config remote.peer.fetch '+refs/heads/main:refs/remotes/review/main';;
      unsupported) git --git-dir="$remote" config receive.advertiseAtomic false;;
      hook)
        mkdir -p "$remote/hooks"
        printf '#!/bin/sh\n[ "$1" != refs/heads/topic ]\n' >"$remote/hooks/update"
        chmod +x "$remote/hooks/update";;
      hook-input)
        mkdir -p "$repo/.git/hooks"
        printf '#!/bin/sh\ncat >"$(git rev-parse --git-dir)/push-hook-input"\n' >"$repo/.git/hooks/pre-push"
        chmod +x "$repo/.git/hooks/pre-push";;
      race)
        mkdir -p "$repo/.git/hooks"
        printf '#!/bin/sh\ngit --git-dir="%s" update-ref refs/heads/main %s %s\n' "$remote" "$c" "$b" >"$repo/.git/hooks/pre-push"
        chmod +x "$repo/.git/hooks/pre-push";;
    esac
  done
}
check() {
  label=$1 scenario=$2 transport=$3 expected=$4; shift 4
  case $label in ${NGIT_PUSH_CASE:-*}) ;; *) return;; esac
  prepare "$label" "$scenario" "$transport"
  for tool in git ngit; do
    cmd=git; [ "$tool" = git ] || cmd=$ngit
    repo=$fx/$tool-$label
    rc=0; timeout 60 "$cmd" -C "$repo" push "$@" >"$out/$tool-$label.out.raw" 2>"$out/$tool-$label.err.raw" || rc=$?
    echo "$rc" >"$out/$tool-$label.rc"
    [ "$rc" = "$expected" ] || { echo "FAIL $label $tool exit $rc expected $expected"; fail=1; }
    normalize "$out/$tool-$label.out.raw" "$out/$tool-$label.out"
    normalize "$out/$tool-$label.err.raw" "$out/$tool-$label.err"
    state "$repo" "$out/$tool-$label.local"
    state "$repo.git" "$out/$tool-$label.remote"
    # Explicit all-or-none assertion, independent of pairwise state equality.
    case $label in atomic-local*|atomic-hook*|mixed-reject*)
      [ "$(git --git-dir="$repo.git" rev-parse refs/heads/main)" = "$b" ] || fail=1
      [ "$(git --git-dir="$repo.git" rev-parse refs/heads/topic)" = "$a" ] || fail=1;;
      atomic-race*|mixed-race*)
      [ "$(git --git-dir="$repo.git" rev-parse refs/heads/main)" = "$c" ] || fail=1
      [ "$(git --git-dir="$repo.git" rev-parse refs/heads/topic)" = "$a" ] || fail=1;;
    esac
  done
  for part in out err rc local remote; do same "$label $part" "$out/git-$label.$part" "$out/ngit-$label.$part"; done
  count=$((count + 1))
  echo "CHECK $label"
}
case $mode in
explicit)
  check explicit-ok normal local 0 --force-with-lease=refs/heads/main:"$b" peer main
  check explicit-short normal local 0 --force-with-lease=main:"$b" peer main
  check explicit-stale normal local 1 --force-with-lease=main:"$a" peer main
  check explicit-symbol normal local 0 --force-with-lease=main:refs/remotes/review/main peer main
  check empty-create normal local 0 --force-with-lease=refs/heads/new: peer main:new
  check empty-existing normal local 1 --force-with-lease=main: peer main
  check lease-delete normal local 0 --force-with-lease=main:"$b" peer :main
  check lease-delete-stale normal local 1 --force-with-lease=main:"$a" peer :main
  check lease-force normal local 0 --force-with-lease=main:"$a" --force peer main
  check force-lease normal local 0 --force --force-with-lease=main:"$a" peer main
  check lease-plus normal local 0 --force-with-lease=main:"$a" peer +main
  check lease-cancel normal local 1 --force-with-lease=main:"$b" --no-force-with-lease peer main
  check lease-repeat normal local 0 --force-with-lease=main:"$b" --force-with-lease=main:"$a" peer main
  check lease-uptodate uptodate local 0 --force-with-lease=main:"$a" peer main
  check hook-stale hook-input local 1 --force-with-lease=main:"$a" peer main topic
  check quiet-stale normal local 1 --quiet --force-with-lease=main:"$a" peer main
  ;;
tracking)
  check tracking-all normal local 0 --force-with-lease peer main topic
  check tracking-named normal local 0 --force-with-lease=main peer main
  check tracking-exact exact local 0 --force-with-lease peer main
  check tracking-missing missing local 1 --force-with-lease peer main
  check tracking-unmapped mapping local 1 --force-with-lease peer main
  check tracking-stale stale local 1 --force-with-lease peer main
  check tracking-partial normal local 1 --force-with-lease=topic peer main topic
  check tracking-cancel normal local 1 --force-with-lease --no-force-with-lease peer main
  check tracking-new missing local 0 --force-with-lease peer main:new
  check tracking-specific normal local 1 --force-with-lease --force-with-lease=main:"$a" peer main topic
  ;;
atomic)
  check atomic-ok normal local 0 --atomic --force peer main topic
  check atomic-local normal local 1 --atomic peer main topic
  check atomic-stale normal local 1 --atomic --force-with-lease=main:"$a" peer main topic
  check atomic-hook hook local 1 --atomic --force peer main topic
  check atomic-unavailable unsupported local 128 --atomic --force peer main topic
  check atomic-cancel unsupported local 0 --atomic --no-atomic --force peer main topic
  check atomic-race race local 1 --atomic --force-with-lease peer main topic
  ;;
e2e)
  for transport in file ssh; do
    check mixed-ok-$transport normal "$transport" 0 --atomic --force-with-lease peer main topic
    check mixed-reject-$transport normal "$transport" 1 --atomic --force-with-lease=main:"$a" peer main topic
    check mixed-race-$transport race "$transport" 1 --atomic --force-with-lease peer main topic
  done
  ;;
esac
[ "$count" -gt 0 ] || exit 2
printf 'push-safety %s: %s cases, failures=%s\n' "$mode" "$count" "$fail"
exit "$fail"
