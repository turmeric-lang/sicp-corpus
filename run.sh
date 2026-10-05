#!/usr/bin/env bash
# run.sh -- run every corpus program on both Turmeric back ends and compare
# its output with the `.expected` file next to it.
#
#   TUR=/path/to/tur bash run.sh            # every program
#   TUR=/path/to/tur bash run.sh ch1/1.1    # programs whose path contains it
#
# For each `<dir>/<name>.scm` with a `<dir>/<name>.expected`:
#   - compiled:    tur run       -I <repo> <file>
#   - interpreted: tur --interpret -I <repo> <file>
# Both outputs must equal the expected file.
#
# `<name>.xfail` marks a program blocked on an open Turmeric report: its first
# line names the report. A mismatch then PASSES as (xfail), and a MATCH FAILS
# with "delete <name>.xfail" -- the fix has landed. A build failure, crash or
# timeout counts as a mismatch for an xfail program, since that is usually
# what the blocking bug looks like.
#
# `<name>.xfail-compiled` / `<name>.xfail-interpreted` do the same for one
# back end only, when the blocking report is that back end's.
#
# `<name>.compiled-only` / `<name>.interp-only` skip the other back end; the
# first line says why.
set -u
cd "$(dirname "$0")"
ROOT=$(pwd)
TUR=${TUR:-tur}
FILTER=${1:-}
TIMEOUT=${SICP_TIMEOUT:-120}

if ! "$TUR" --version >/dev/null 2>&1; then
    echo "error: cannot run '$TUR' (set TUR=/path/to/tur)" >&2
    exit 2
fi
echo "tur: $("$TUR" --version 2>&1 | head -1)"

# `timeout` is not on stock macOS; perl's alarm survives exec.
run_limited() { perl -e "alarm $TIMEOUT; exec @ARGV" "$@"; }

pass=0; fail=0; xfail=0; skip=0
failed=()
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

for scm in $(find . -name '*.scm' -not -path './corpus/*' | sort); do
    rel=${scm#./}
    base=${rel%.scm}
    [ -n "$FILTER" ] && [[ "$rel" != *"$FILTER"* ]] && continue
    expected="$base.expected"
    if [ ! -f "$expected" ]; then
        echo "SKIP $rel -- no $expected"
        skip=$((skip + 1))
        continue
    fi
    for mode in compiled interpreted; do
        if [ "$mode" = compiled ] && [ -f "$base.interp-only" ]; then skip=$((skip + 1)); continue; fi
        if [ "$mode" = interpreted ] && [ -f "$base.compiled-only" ]; then skip=$((skip + 1)); continue; fi
        out="$tmp/out"
        if [ "$mode" = compiled ]; then
            run_limited "$TUR" run -I "$ROOT" "$rel" >"$out" 2>"$tmp/err"
        else
            run_limited "$TUR" --interpret -I "$ROOT" "$rel" >"$out" 2>"$tmp/err"
        fi
        rc=$?
        if [ $rc -eq 0 ] && cmp -s "$out" "$expected"; then
            matched=1
        else
            matched=0
        fi
        label="$rel ($mode)"
        marker=""
        if [ -f "$base.xfail-$mode" ]; then marker="$base.xfail-$mode"
        elif [ -f "$base.xfail" ]; then marker="$base.xfail"; fi
        if [ -n "$marker" ]; then
            if [ $matched -eq 1 ]; then
                echo "FAIL $label -- passes now; delete $marker ($(head -1 "$marker"))"
                fail=$((fail + 1)); failed+=("$label")
            else
                echo "PASS $label (xfail: $(head -1 "$marker"))"
                xfail=$((xfail + 1))
            fi
        elif [ $matched -eq 1 ]; then
            echo "PASS $label"
            pass=$((pass + 1))
        else
            echo "FAIL $label -- exit $rc"
            diff "$expected" "$out" | head -20 | sed 's/^/    /'
            grep -v 'TUR-W0060' "$tmp/err" | head -10 | sed 's/^/    stderr: /'
            fail=$((fail + 1)); failed+=("$label")
        fi
    done
done

echo
echo "summary: $pass passed, $xfail xfail, $fail failed, $skip skipped"
[ $fail -eq 0 ]
