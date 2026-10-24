# Sourced by compare.sh's bisect case; uses its wrun/wsteps harness.
WTREE=1 WREFS=1 WSTDERR=1
sh "$root/test/make-bisect-fixture.sh" "$fx" || exit 2
r=$fx/bisect-linear
b=$fx/bisect-branch
tie=$fx/bisect-tie

wrun "$r" '' bisect start
wrun "$r" '' bisect start c8 c0
wrun "$r" '' bisect start HEAD HEAD~8
wrun "$r" '' bisect log
wrun "$r" '' bisect reset

wsteps "$r" '' 'bisect mark bounds separately' \
  'g bisect start; g bisect bad c8; g bisect good c0'
wsteps "$r" '' 'bisect starts with bad only' \
  'g bisect start c8; g bisect good c0'
wsteps "$r" '' 'bisect marks good before bad' \
  'g bisect start; g bisect good c0; g bisect bad c8'
wsteps "$r" '' 'bisect good and bad to completion' \
  'g bisect start c8 c0; g bisect good; g bisect bad; g bisect bad'
wsteps "$r" '' 'bisect skip and continue' \
  'g bisect start c8 c0; g bisect skip; g bisect good'
wsteps "$r" '' 'bisect log after marks' \
  'g bisect start c8 c0; g bisect good; g bisect log'
wsteps "$r" '' 'bisect reset active session' \
  'g bisect start c8 c0; g bisect bad; g bisect reset'
wsteps "$r" 'git checkout -q --detach c7' 'bisect reset detached start' \
  'g bisect start c8 c0; g bisect reset'
wsteps "$r" '' 'bisect restart preserves original branch' \
  'g bisect start c8 c0; g bisect start c8 c0; g bisect reset'
wsteps "$r" '' 'bisect only skipped candidates' \
  'g bisect start c2 c0; g bisect skip || [ $? -eq 2 ]'
wsteps "$r" '' 'bisect explicitly skips all candidates' \
  'g bisect start c3 c0; g bisect skip c1 c2 || [ $? -eq 2 ]'
wsteps "$r" 'printf %s\\n "#!/bin/sh" "echo run-out" "echo run-err >&2" "test \"\$(cat value)\" -lt 5" >check.sh; chmod +x check.sh' \
  'bisect run to completion' \
  'g bisect start c8 c0; g bisect run ./check.sh'
wsteps "$r" 'printf %s\\n "#!/bin/sh" "v=\$(cat value); [ \"\$v\" -eq 4 ] && exit 125; test \"\$v\" -lt 6" >check.sh; chmod +x check.sh' \
  'bisect run with skip' \
  'g bisect start c8 c0; g bisect run ./check.sh'
wsteps "$r" 'printf %s\\n "#!/bin/sh" "exit 130" >check.sh; chmod +x check.sh' \
  'bisect run rejects high exit' \
  'g bisect start c8 c0; g bisect run ./check.sh || [ $? -eq 126 ]'
wsteps "$r" '' 'bisect run missing command' \
  'g bisect start c8 c0; g bisect run ./missing || [ $? -eq 1 ]'
wsteps "$r" 'printf %s\\n "#!/bin/sh" "exit 126" >check.sh; chmod +x check.sh' \
  'bisect run rejects persistent 126' \
  'g bisect start c8 c0; g bisect run ./check.sh || [ $? -eq 1 ]'

wrun "$b" '' bisect start HEAD base
wsteps "$b" '' 'bisect branching history' \
  'g bisect start HEAD base; g bisect bad; g bisect good'
wrun "$tie" '' bisect start tip base
wrun "$tie" '' bisect start 'tip^2' 'tip^1'

WTREE= WREFS= WSTDERR=
