# Sourced after misc-suite.sh by test/compare.sh's misc case.

WTREE=1 WREFS=1 WSTDERR=1
checkout_fixture misc-audit

nested_ignored="printf 'outer/\n' > .git/info/exclude; mkdir -p outer/nested; git -C outer/nested init -q; printf 'private\n' > outer/nested/private"
wrun "$wt" "$nested_ignored" clean -f -d -X
wrun "$wt" "$nested_ignored" clean -f -f -d -X

WENV="HOME=$out/w/home"
wrun "$wt" "mkdir home; : > home/.gitconfig" config --global foo.bar value
WENV=

WTREE= WREFS= WSTDERR=
