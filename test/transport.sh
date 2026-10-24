#!/bin/sh
# Differential transport tests. Usage: test/transport.sh clone|fetch|push|remote|ssh
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
# Byte code makes ngit several times faster; without Emacs, sources run.
sh "$root/tools/build-elc.sh" || :
suite=${1:-}
case $suite in clone|fetch|push|remote|ssh) ;; *) echo "usage: $0 clone|fetch|push|remote|ssh"; exit 2;; esac
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}/transport-$suite
rm -rf "$fx" && mkdir -p "$fx"
out=$(mktemp -d "${TMPDIR:-/tmp}/ngit-transport.XXXXXXXX")
trap 'rm -rf "$out" "$fx"' EXIT HUP INT TERM
export LC_ALL=C TZ=UTC GIT_CONFIG_NOSYSTEM=1 GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME='Test Author' GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME='Test Author' GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE='1700600000 +0000' GIT_COMMITTER_DATE='1700600000 +0000'
empty="$fx/empty-template"; mkdir "$empty"; export GIT_TEMPLATE_DIR=$empty

die() { echo "$*"; exit 2; }
commit() { d=$1 m=$2 f=$3; printf '%s\n' "$m" >>"$d/$f"; git -C "$d" add "$f" && git -C "$d" commit -qm "$m"; }
seed() {
  git init -q --template= -b main "$fx/seed" || die 'seed init failed'
  commit "$fx/seed" one tracked.txt
  git -C "$fx/seed" branch topic
  commit "$fx/seed" two tracked.txt
  git -C "$fx/seed" tag v1
  git -C "$fx/seed" tag -a annotated -m 'annotated transport tag'
  git clone -q --template= --bare "$fx/seed" "$fx/base.git" || die 'bare seed failed'
}
seed
fail=0

normalize() { sed "s|$fx/git-|$fx/tool-|g; s|$fx/ngit-|$fx/tool-|g; s|$fx|@FX@|g" "$1" >"$2"; }
state() { # state PATH OUTPUT
  d=$1 o=$2
  if [ -d "$d/.git" ]; then gd="$d/.git"; wt=$d; else gd=$d; wt=; fi
  {
    echo '== HEAD'; cat "$gd/HEAD" 2>/dev/null || :
    echo '== config'; git --git-dir="$gd" config --local --list --show-origin 2>/dev/null | sed 's/^file:[^[:space:]]*/file:@CONFIG@/' | LC_ALL=C sort
    echo '== refs'; git --git-dir="$gd" for-each-ref --format='%(refname) %(objectname) %(symref)' | LC_ALL=C sort
    echo '== reflogs'; (cd "$gd" && find logs -type f 2>/dev/null | LC_ALL=C sort | while IFS= read -r f; do echo "-- $f"; cat "$f"; done)
    echo '== FETCH_HEAD'; cat "$gd/FETCH_HEAD" 2>/dev/null || :
    echo '== reachable'; git --git-dir="$gd" rev-list --objects --all 2>/dev/null | LC_ALL=C sort
    echo '== index'; [ -n "$wt" ] && git -C "$wt" ls-files --stage || :
    echo '== worktree'; [ -n "$wt" ] && (cd "$wt" && find . -path ./.git -prune -o -type f -print | LC_ALL=C sort | while read -r f; do printf '%s ' "$f"; git hash-object "$f"; done) || :
    echo '== fsck'; git --git-dir="$gd" fsck --no-progress --connectivity-only 2>&1
  } >"$o.raw"
  normalize "$o.raw" "$o"
}
same() { cmp -s "$2" "$3" || { echo "MISMATCH: $1"; diff -u "$2" "$3" | head -40; fail=1; }; }
run() { # run TOOL LABEL CWD COMMAND...
  tool=$1 label=$2 cwd=$3; shift 3
  if [ "$tool" = git ]; then cmd=git; else cmd=$ngit; fi
  (cd "$cwd" && timeout 90 "$cmd" "$@") >"$out/$tool.$label.out.raw" 2>"$out/$tool.$label.err.raw"; st=$?
  normalize "$out/$tool.$label.out.raw" "$out/$tool.$label.out"
  normalize "$out/$tool.$label.err.raw" "$out/$tool.$label.err"
  echo "$st" >"$out/$tool.$label.status"
}
compare_run() { label=$1 cwdg=$2 cwdn=$3; shift 3; run git "$label" "$cwdg" "$@"; run ngit "$label" "$cwdn" "$@"; same "$label stdout" "$out/git.$label.out" "$out/ngit.$label.out" && same "$label stderr" "$out/git.$label.err" "$out/ngit.$label.err" && same "$label exit" "$out/git.$label.status" "$out/ngit.$label.status"; }
copy_pair() { name=$1; rm -rf "$fx/git-$name" "$fx/ngit-$name"; cp -a "$fx/$name" "$fx/git-$name"; cp -a "$fx/$name" "$fx/ngit-$name"; }
compare_state() { label=$1 g=$2 n=$3; state "$g" "$out/git.$label.state"; state "$n" "$out/ngit.$label.state"; same "$label state" "$out/git.$label.state" "$out/ngit.$label.state"; }
expect_status() { [ "$(cat "$out/git.$1.status")" = "$2" ] && [ "$(cat "$out/ngit.$1.status")" = "$2" ] || { echo "MISMATCH: $1 expected exit $2"; fail=1; }; }

clone_case() { label=$1; shift; rm -rf "$fx/git-$label" "$fx/ngit-$label"; run git "$label" "$fx" clone --template= "$@" "$fx/git-$label"; run ngit "$label" "$fx" clone --template= "$@" "$fx/ngit-$label"; same "$label stdout" "$out/git.$label.out" "$out/ngit.$label.out"; same "$label stderr" "$out/git.$label.err" "$out/ngit.$label.err"; same "$label exit" "$out/git.$label.status" "$out/ngit.$label.status"; compare_state "$label" "$fx/git-$label" "$fx/ngit-$label"; }

case $suite in
clone)
  timeout 90 nelisp --load "$root/test/transport-delta.el" >"$out/delta.out" 2>"$out/delta.err" || die 'native delta boundary tests failed'
  # Force the native delta/cache and fork paths on a small ordinary pack.
  git -C "$fx/seed" pack-objects --all --stdout --delta-base-offset >"$fx/received.pack" || die 'pack fixture failed'
  git index-pack -o "$fx/reference.idx" "$fx/received.pack" >/dev/null || die 'reference index failed'
  NGIT_TRANSPORT_WORKERS=4 NGIT_TRANSPORT_NATIVE=1 NGIT_TRANSPORT_PACK="$fx/received.pack" NGIT_TRANSPORT_IDX="$fx/received.idx" timeout 90 nelisp --load "$root/test/transport-index.el" >"$out/index.out" 2>"$out/index.err" || die 'native received pack index failed'
  same 'native received pack index' "$fx/reference.idx" "$fx/received.idx"
  for kind in ofs ref branch; do
    case $kind in
      ofs) python3 "$root/test/make-transport-pack.py" "$fx/$kind.pack" ;;
      branch) python3 "$root/test/make-transport-pack.py" "$fx/$kind.pack" branch ;;
      ref) python3 "$root/test/make-pack-fixture.py" "$fx/$kind.pack" ;;
    esac
    git index-pack -o "$fx/$kind.reference.idx" "$fx/$kind.pack" >/dev/null || die 'delta reference index failed'
    NGIT_TRANSPORT_WORKERS=4 NGIT_TRANSPORT_NATIVE=1 NGIT_TRANSPORT_CACHE_COUNT=1 NGIT_TRANSPORT_IO_LIMIT=17 NGIT_TRANSPORT_PACK="$fx/$kind.pack" NGIT_TRANSPORT_IDX="$fx/$kind.received.idx" timeout 90 nelisp --load "$root/test/transport-index.el" >"$out/$kind.index.out" 2>"$out/$kind.index.err" || die 'delta received pack index failed'
    same "$kind delta index" "$fx/$kind.reference.idx" "$fx/$kind.received.idx"
  done
  # Init, bare init and reinitialization are part of the clone/layout contract.
  for tool in git ngit; do cmd=git; [ "$tool" = ngit ] && cmd=$ngit; "$cmd" init --template= -b trunk "$fx/$tool-init" >"$out/$tool.init.out" 2>"$out/$tool.init.err"; echo $? >"$out/$tool.init.status"; "$cmd" -C "$fx/$tool-init" init --template= >>"$out/$tool.init.out" 2>>"$out/$tool.init.err"; echo $? >>"$out/$tool.init.status"; "$cmd" init --template= --bare -b baremain "$fx/$tool-bare" >"$out/$tool.bare.out" 2>"$out/$tool.bare.err"; echo $? >"$out/$tool.bare.status"; done
  for tool in git ngit; do for part in init.out init.err bare.out bare.err; do normalize "$out/$tool.$part" "$out/$tool.$part.norm"; mv "$out/$tool.$part.norm" "$out/$tool.$part"; done; done
  same 'init output' "$out/git.init.out" "$out/ngit.init.out"; same 'init stderr' "$out/git.init.err" "$out/ngit.init.err"; same 'init status' "$out/git.init.status" "$out/ngit.init.status"; compare_state init "$fx/git-init" "$fx/ngit-init"
  same 'bare output' "$out/git.bare.out" "$out/ngit.bare.out"; same 'bare stderr' "$out/git.bare.err" "$out/ngit.bare.err"; same 'bare status' "$out/git.bare.status" "$out/ngit.bare.status"; compare_state bare "$fx/git-bare" "$fx/ngit-bare"
  clone_case local "$fx/base.git"
  clone_case file "file://$fx/base.git"
  clone_case branch -b topic "$fx/base.git"
  clone_case nocheckout --no-checkout "$fx/base.git"
  clone_case origin --origin upstream "$fx/base.git"
  clone_case bareclone --bare "$fx/base.git"
  clone_case annotated -b annotated "$fx/base.git"
  # Shared directory boundaries and index attributes must survive forked writes.
  cp -a "$fx/seed" "$fx/parallel-seed"
  mkdir -p "$fx/parallel-seed/nested/shared"
  mkdir -p "$fx/parallel-seed/a"
  printf 'before directory\n' >"$fx/parallel-seed/a.txt"
  printf 'directory child\n' >"$fx/parallel-seed/a/sub.txt"
  printf 'after directory\n' >"$fx/parallel-seed/a0.txt"
  printf '*.txt text eol=crlf\n' >"$fx/parallel-seed/.gitattributes"
  for name in a b c d e f g h i j k l; do printf 'parallel checkout\n' >"$fx/parallel-seed/nested/shared/$name.txt"; done
  printf '#!/bin/sh\nexit 0\n' >"$fx/parallel-seed/nested/shared/run.sh"
  chmod +x "$fx/parallel-seed/nested/shared/run.sh"
  ln -s nested/shared/run.sh "$fx/parallel-seed/link"
  git -C "$fx/parallel-seed" -c core.safecrlf=false add -A
  git -C "$fx/parallel-seed" commit -qm 'parallel checkout fixture'
  git clone -q --template= "$fx/parallel-seed" "$fx/git-parallel"
  "$ngit" clone -q --no-checkout "$fx/parallel-seed" "$fx/ngit-parallel" || die 'parallel clone preparation failed'
  NGIT_TRANSPORT_CHECKOUT_REPO="$fx/ngit-parallel" timeout 90 nelisp --load "$root/test/transport-checkout.el" >"$out/checkout.out" 2>"$out/checkout.err" || die 'parallel initial checkout failed'
  compare_state parallel "$fx/git-parallel" "$fx/ngit-parallel"
  [ "$(readlink "$fx/git-parallel/link")" = "$(readlink "$fx/ngit-parallel/link")" ] || die 'parallel symlink mismatch'
  [ "$(stat -c %a "$fx/git-parallel/nested/shared/run.sh")" = "$(stat -c %a "$fx/ngit-parallel/nested/shared/run.sh")" ] || die 'parallel executable mode mismatch'
  git init -q --bare --template= -b unborn "$fx/empty.git"
  clone_case empty "$fx/empty.git"
  ;;
fetch)
  git clone -q --template= "$fx/base.git" "$fx/client"; copy_pair client
  commit "$fx/seed" three new.txt; git -C "$fx/seed" tag v2; git -C "$fx/seed" tag -a v2-annotated -m 'new annotated tag'; git -C "$fx/seed" tag -a v2-old-annotated -m 'tag on an existing commit' v1; git -C "$fx/seed" push -q --tags "$fx/base.git" main
  compare_run default "$fx/git-client" "$fx/ngit-client" fetch
  compare_state default-fetch "$fx/git-client" "$fx/ngit-client"
  compare_run explicit "$fx/git-client" "$fx/ngit-client" fetch origin main:refs/remotes/origin/copied
  commit "$fx/seed" refspec-dual refspec-dual.txt
  git -C "$fx/seed" tag -a refspec-order -m 'refspec mapping order'
  git -C "$fx/seed" push -q --tags "$fx/base.git" main
  compare_run dual-destination "$fx/git-client" "$fx/ngit-client" fetch origin main:refs/remotes/origin/dual-x main:refs/remotes/origin/dual-y
  compare_state dual-destination "$fx/git-client" "$fx/ngit-client"
  commit "$fx/seed" refspec-source refspec-source.txt; git -C "$fx/seed" push -q "$fx/base.git" main
  compare_run explicit-source "$fx/git-client" "$fx/ngit-client" fetch origin main
  compare_state explicit-source "$fx/git-client" "$fx/ngit-client"
  compare_run explicit-overlap "$fx/git-client" "$fx/ngit-client" fetch origin main main:refs/remotes/origin/overlap
  compare_state explicit-overlap "$fx/git-client" "$fx/ngit-client"
  compare_run repeated-destination "$fx/git-client" "$fx/ngit-client" fetch origin main:refs/remotes/origin/repeated main:refs/remotes/origin/repeated
  compare_state repeated-destination "$fx/git-client" "$fx/ngit-client"
  compare_run conflicting-append "$fx/git-client" "$fx/ngit-client" fetch --append origin main:refs/remotes/origin/conflict topic:refs/remotes/origin/conflict
  expect_status conflicting-append 128
  compare_state conflicting-append "$fx/git-client" "$fx/ngit-client"
  compare_run conflicting-destination "$fx/git-client" "$fx/ngit-client" fetch origin main:refs/remotes/origin/conflict topic:refs/remotes/origin/conflict
  expect_status conflicting-destination 128
  compare_state conflicting-destination "$fx/git-client" "$fx/ngit-client"
  compare_run tags "$fx/git-client" "$fx/ngit-client" fetch --tags
  compare_run nonff "$fx/git-client" "$fx/ngit-client" fetch origin topic:refs/remotes/origin/main
  expect_status nonff 1
  git --git-dir="$fx/base.git" update-ref refs/heads/prune HEAD; compare_run getprune "$fx/git-client" "$fx/ngit-client" fetch origin; git --git-dir="$fx/base.git" update-ref -d refs/heads/prune; compare_run prune "$fx/git-client" "$fx/ngit-client" fetch --prune origin
  compare_state fetch "$fx/git-client" "$fx/ngit-client"
  # Pull fast-forward, a merge, a rebase, and a rejected ff-only update.
  for kind in ff merge rebase reject unspecified; do git clone -q --template= "$fx/base.git" "$fx/$kind"; done
  commit "$fx/seed" four tracked.txt; git -C "$fx/seed" push -q "$fx/base.git" main
  copy_pair ff; compare_run pullff "$fx/git-ff" "$fx/ngit-ff" pull --ff-only; compare_state pullff "$fx/git-ff" "$fx/ngit-ff"
  for kind in merge rebase reject unspecified; do commit "$fx/$kind" "local-$kind" "local-$kind.txt"; copy_pair "$kind"; done
  compare_run pullmerge "$fx/git-merge" "$fx/ngit-merge" pull --no-rebase --no-edit; compare_state pullmerge "$fx/git-merge" "$fx/ngit-merge"
  compare_run pullrebase "$fx/git-rebase" "$fx/ngit-rebase" pull --rebase; compare_state pullrebase "$fx/git-rebase" "$fx/ngit-rebase"
  compare_run pullreject "$fx/git-reject" "$fx/ngit-reject" pull --ff-only; compare_state pullreject "$fx/git-reject" "$fx/ngit-reject"
  compare_run unspecified "$fx/git-unspecified" "$fx/ngit-unspecified" pull
  expect_status unspecified 128
  compare_state unspecified "$fx/git-unspecified" "$fx/ngit-unspecified"
  ;;
push)
  git clone -q --template= "$fx/base.git" "$fx/client"; commit "$fx/client" local local.txt; git -C "$fx/client" branch extra; git -C "$fx/client" tag push-tag
  for c in default refspec delete force tags upstream reject; do cp -a "$fx/client" "$fx/$c"; cp -a "$fx/base.git" "$fx/$c.git"; git -C "$fx/$c" remote set-url origin "$fx/$c.git"; done
  for c in default refspec delete force tags upstream reject; do copy_pair "$c"; cp -a "$fx/$c.git" "$fx/git-$c.git"; cp -a "$fx/$c.git" "$fx/ngit-$c.git"; git -C "$fx/git-$c" remote set-url origin "$fx/git-$c.git"; git -C "$fx/ngit-$c" remote set-url origin "$fx/ngit-$c.git"; done
  compare_run default "$fx/git-default" "$fx/ngit-default" push; expect_status default 0; compare_state default-remote "$fx/git-default.git" "$fx/ngit-default.git"; compare_state default-local "$fx/git-default" "$fx/ngit-default"
  compare_run refspec "$fx/git-refspec" "$fx/ngit-refspec" push origin main:refs/heads/copied; compare_run delete "$fx/git-refspec" "$fx/ngit-refspec" push origin --delete copied; compare_state refspec-remote "$fx/git-refspec.git" "$fx/ngit-refspec.git"
  for d in "$fx/git-force.git" "$fx/ngit-force.git"; do tree=$(git --git-dir="$d" rev-parse HEAD^{tree}); parent=$(git --git-dir="$d" rev-parse HEAD); newer=$(printf 'remote force target\n' | git --git-dir="$d" commit-tree "$tree" -p "$parent"); git --git-dir="$d" update-ref refs/heads/main "$newer"; done
  compare_run force "$fx/git-force" "$fx/ngit-force" push --force origin main; expect_status force 0; compare_state force-remote "$fx/git-force.git" "$fx/ngit-force.git"
  compare_run tags "$fx/git-tags" "$fx/ngit-tags" push --tags origin; compare_state tags-remote "$fx/git-tags.git" "$fx/ngit-tags.git"
  compare_run upstream "$fx/git-upstream" "$fx/ngit-upstream" push -u origin extra; compare_state upstream-local "$fx/git-upstream" "$fx/ngit-upstream"
  compare_run delete-missing "$fx/git-delete" "$fx/ngit-delete" push origin --delete missing
  expect_status delete-missing 1
  compare_run delete-empty "$fx/git-delete" "$fx/ngit-delete" push origin --delete
  expect_status delete-empty 128
  # Advance remote on a divergent line so a normal push is rejected with advice.
  git clone -q --template= "$fx/git-reject.git" "$fx/reject-advance"; git -C "$fx/reject-advance" switch -q topic; commit "$fx/reject-advance" remote-diverged remote.txt; git -C "$fx/reject-advance" push -q --force "$fx/git-reject.git" HEAD:main; rm -rf "$fx/ngit-reject.git"; cp -a "$fx/git-reject.git" "$fx/ngit-reject.git"
  compare_run reject "$fx/git-reject" "$fx/ngit-reject" push origin main; compare_state reject-remote "$fx/git-reject.git" "$fx/ngit-reject.git"
  expect_status reject 1
  ;;
remote)
  git clone -q --template= "$fx/base.git" "$fx/client"; copy_pair client
  compare_run add "$fx/git-client" "$fx/ngit-client" remote add second "$fx/base.git"
  compare_run malformed "$fx/git-client" "$fx/ngit-client" remote add 'bad..name' "$fx/base.git"
  compare_run verbose "$fx/git-client" "$fx/ngit-client" remote -v
  compare_run geturl "$fx/git-client" "$fx/ngit-client" remote get-url second
  compare_run seturl "$fx/git-client" "$fx/ngit-client" remote set-url second "file://$fx/base.git"
  compare_run rename "$fx/git-client" "$fx/ngit-client" remote rename second renamed
  compare_run lsremote "$fx/git-client" "$fx/ngit-client" ls-remote renamed
  compare_run remove "$fx/git-client" "$fx/ngit-client" remote remove renamed
  compare_run rename-tracking "$fx/git-client" "$fx/ngit-client" remote rename origin upstream
  compare_state renamed-tracking "$fx/git-client" "$fx/ngit-client"
  compare_run remove-tracking "$fx/git-client" "$fx/ngit-client" remote remove upstream
  compare_state remote "$fx/git-client" "$fx/ngit-client"
  # Renaming a packed namespace across another remote must preserve sorted
  # lookup order and keep annotated-tag peel lines beside their primary ref.
  git clone -q --template= "$fx/base.git" "$fx/packed"
  for name in alpha other; do
    git -C "$fx/packed" remote add "$name" "$fx/base.git"
    git -C "$fx/packed" update-ref "refs/remotes/$name/main" HEAD
  done
  git -C "$fx/packed" update-ref refs/remotes/origin/peeled refs/tags/annotated
  git -C "$fx/packed" pack-refs --all --prune
  copy_pair packed
  compare_run packed-rename "$fx/git-packed" "$fx/ngit-packed" remote rename origin z
  expect_status packed-rename 0
  compare_state packed-rename "$fx/git-packed" "$fx/ngit-packed"
  # compare_state does not show where refs are stored; git moves the
  # renamed refs out of packed-refs and keeps the rest with their peel lines.
  for t in git ngit; do
    (cd "$fx/$t-packed/.git" && cat packed-refs && find refs -type f | LC_ALL=C sort) >"$fx/$t-packed.layout"
  done
  cmp -s "$fx/git-packed.layout" "$fx/ngit-packed.layout" || die 'packed rename layout mismatch'
  compare_run packed-remove "$fx/git-packed" "$fx/ngit-packed" remote remove z
  expect_status packed-remove 0
  compare_state packed-remove "$fx/git-packed" "$fx/ngit-packed"
  ;;
ssh)
  cat >"$fx/fake-ssh" <<'EOF'
#!/bin/sh
while [ $# -gt 0 ]; do case $1 in -G) exit 0;; -o|-p|-l|-i|-F|-J) shift 2;; -*) shift;; *) break;; esac; done
[ $# -gt 0 ] && shift
exec sh -c "$1"
EOF
  chmod +x "$fx/fake-ssh"; export GIT_SSH_COMMAND="$fx/fake-ssh"
  # A relative scp path infers its destination from the repository component.
  sed '/exec sh -c/i cd "$(dirname "$0")"' "$fx/fake-ssh" >"$fx/fake-ssh-relative"
  chmod +x "$fx/fake-ssh-relative"
  export GIT_SSH_COMMAND="$fx/fake-ssh-relative"
  mkdir "$fx/git-inferred" "$fx/ngit-inferred"
  compare_run ssh-inferred "$fx/git-inferred" "$fx/ngit-inferred" clone --template= fake:base.git
  expect_status ssh-inferred 0
  [ -d "$fx/ngit-inferred/base" ] || die 'relative ssh clone inferred the wrong directory'
  compare_state ssh-inferred "$fx/git-inferred/base" "$fx/ngit-inferred/base"
  export GIT_SSH_COMMAND="$fx/fake-ssh"
  url="fake:$fx/base.git"
  rm -rf "$fx/git-ssh" "$fx/ngit-ssh"; run git sshclone "$fx" clone --template= "$url" "$fx/git-ssh"; run ngit sshclone "$fx" clone --template= "$url" "$fx/ngit-ssh"; same 'ssh clone output' "$out/git.sshclone.out" "$out/ngit.sshclone.out"; same 'ssh clone stderr' "$out/git.sshclone.err" "$out/ngit.sshclone.err"; same 'ssh clone exit' "$out/git.sshclone.status" "$out/ngit.sshclone.status"; compare_state sshclone "$fx/git-ssh" "$fx/ngit-ssh"
  commit "$fx/seed" sshremote ssh.txt; git -C "$fx/seed" push -q "$fx/base.git" main
  compare_run sshfetch "$fx/git-ssh" "$fx/ngit-ssh" fetch origin; compare_state sshfetch "$fx/git-ssh" "$fx/ngit-ssh"
  # The fake server deliberately does not forward SendEnv, exercising the
  # protocol-v0 ACK/NAK negotiation fallback for a second fresh clone.
  sed '/exec sh -c/i unset GIT_PROTOCOL' "$fx/fake-ssh" >"$fx/fake-ssh-v0"
  chmod +x "$fx/fake-ssh-v0"; export GIT_SSH_COMMAND="$fx/fake-ssh-v0"
  rm -rf "$fx/git-sshv0" "$fx/ngit-sshv0"; run git sshv0 "$fx" clone --template= "$url" "$fx/git-sshv0"; run ngit sshv0 "$fx" clone --template= "$url" "$fx/ngit-sshv0"; same 'ssh v0 output' "$out/git.sshv0.out" "$out/ngit.sshv0.out"; same 'ssh v0 stderr' "$out/git.sshv0.err" "$out/ngit.sshv0.err"; same 'ssh v0 exit' "$out/git.sshv0.status" "$out/ngit.sshv0.status"; compare_state sshv0 "$fx/git-sshv0" "$fx/ngit-sshv0"
  commit "$fx/seed" ssh-v0-remote ssh-v0.txt; git -C "$fx/seed" tag -a ssh-v0-annotated -m 'v0 annotated tag'; git -C "$fx/seed" tag -a ssh-v0-old-annotated -m 'v0 tag on an existing commit' v1; git -C "$fx/seed" push -q --tags "$fx/base.git" main
  compare_run sshv0fetch "$fx/git-sshv0" "$fx/ngit-sshv0" fetch origin
  compare_state sshv0fetch "$fx/git-sshv0" "$fx/ngit-sshv0"
  unset GIT_SSH_COMMAND; export GIT_SSH="$fx/fake-ssh"
  git -C "$fx/git-ssh" pull -q --ff-only; git -C "$fx/ngit-ssh" pull -q --ff-only
  commit "$fx/git-ssh" sshpush push.txt; commit "$fx/ngit-ssh" sshpush push.txt
  cp -a "$fx/base.git" "$fx/git-ssh.git"; cp -a "$fx/base.git" "$fx/ngit-ssh.git"; git -C "$fx/git-ssh" remote set-url origin "fake:$fx/git-ssh.git"; git -C "$fx/ngit-ssh" remote set-url origin "fake:$fx/ngit-ssh.git"
  compare_run sshpush "$fx/git-ssh" "$fx/ngit-ssh" push origin main; expect_status sshpush 0; compare_state sshpush "$fx/git-ssh.git" "$fx/ngit-ssh.git"
  url="ssh://fake$fx/base.git"
  clone_case sshurl "$url"
  clone_case ipv6 "ssh://[::1]$fx/base.git"
  unset GIT_SSH
  git -C "$fx/git-ssh" config core.sshCommand "$fx/fake-ssh"; git -C "$fx/ngit-ssh" config core.sshCommand "$fx/fake-ssh"
  compare_run ssh-config "$fx/git-ssh" "$fx/ngit-ssh" ls-remote origin
  ;;
esac
if [ "$fail" = 0 ]; then echo "transport $suite: PASS"; else echo "transport $suite: FAIL"; fi
[ "$fail" = 0 ]
