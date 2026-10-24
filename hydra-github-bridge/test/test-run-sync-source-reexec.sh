#!/usr/bin/env bash
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
#
# Local regression test for the run-sync-source.sh root re-exec boundary.
#
# Needs no root, sudo, or identity change: `id` is a stub that only claims a
# root invocation, `setpriv` is a stub that really execs the remaining
# command, and initdb/pg_ctl are no-op stubs. The script under test is always
# mode 0644 and is invoked through Bash, mirroring the Nix testWrapper.
#
# Covers: boundary reachability, exact argv fidelity (spaces included),
# probe failure-status propagation, RED (direct `"$0"` exec fails when the
# fixture is not executable) vs GREEN (explicit-interpreter re-exec works),
# the uid-0 drop refusal, and the missing-setpriv refusal.
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
under_test=${1:-$script_dir/run-sync-source.sh}

fails=0
pass() { printf 'PASS: %s\n' "$1"; }
fail() { printf 'FAIL: %s\n' "$1"; fails=$((fails + 1)); }

work=$(mktemp -d /tmp/reexec-test.XXXXXX)
trap 'rm -rf "$work"' EXIT

bash_bin=$(command -v bash)
mktemp_bin=$(command -v mktemp)
mkdir_bin=$(command -v mkdir)
rm_bin=$(command -v rm)
cat_bin=$(command -v cat)

mkstub() {
  local dir=$1
  local name=$2
  printf '#!%s\n' "$bash_bin" >"$dir/$name"
  cat >>"$dir/$name"
  chmod 755 "$dir/$name"
}

# make_harness DIR SETPRIV_KIND: stub id/initdb/pg_ctl plus real-tool shims;
# SETPRIV_KIND is exec (really exec the remaining command), tripwire (record
# and fail if the fixture calls setpriv), or absent (no setpriv on PATH).
make_harness() {
  local dir=$1
  local kind=$2
  mkdir -p "$dir"
  mkstub "$dir" id <<'EOF'
if [ "${FAKE_DROPPED:-}" = 1 ]; then echo "${FAKE_DROPPED_UID:-65534}"; exit 0; fi
if [ "$1" = "-u" ] && [ $# -eq 1 ]; then echo "${FAKE_ROOT_UID:-0}"; exit 0; fi
if [ "$1" = "-u" ]; then echo "${FAKE_NOBODY_UID:-65534}"; exit 0; fi
if [ "$1" = "-g" ]; then echo "${FAKE_NOBODY_GID:-65534}"; exit 0; fi
echo "${FAKE_ROOT_UID:-0}"
EOF
  case "$kind" in
    exec)
      mkstub "$dir" setpriv <<'EOF'
while [ $# -gt 0 ]; do case "$1" in --*) shift ;; *) break ;; esac; done
export FAKE_DROPPED=1
exec "$@"
EOF
      ;;
    tripwire)
      mkstub "$dir" setpriv <<EOF
touch "$work/sentinel-setpriv-called"
echo "tripwire setpriv must not be called" >&2
exit 99
EOF
      ;;
  esac
  mkstub "$dir" initdb <<'EOF'
exit 0
EOF
  mkstub "$dir" pg_ctl <<'EOF'
exit 0
EOF
  mkstub "$dir" mktemp <<EOF
exec "$mktemp_bin" "\$@"
EOF
  mkstub "$dir" mkdir <<EOF
exec "$mkdir_bin" "\$@"
EOF
  mkstub "$dir" rm <<EOF
exec "$rm_bin" "\$@"
EOF
  mkstub "$dir" cat <<EOF
exec "$cat_bin" "\$@"
EOF
}

make_harness "$work/stub" exec
make_harness "$work/stub-trip" tripwire
mkdir -p "$work/nostub"
cp "$work/stub/id" "$work/nostub/id"

# Probe command: records argv exactly, marks the boundary, exits PROBE_STATUS.
printf '#!%s\n' "$bash_bin" >"$work/probe.sh"
cat >>"$work/probe.sh" <<EOF
printf '%s\n' "\$@" > "$work/args.out"
touch "$work/marker"
echo TEST-BOUNDARY
exit "\${PROBE_STATUS:-0}"
EOF
chmod 755 "$work/probe.sh"

got_status=0
run() {
  set +e
  "$@" >"$work/stdout.log" 2>"$work/stderr.log"
  got_status=$?
  set -e
}

cp "$under_test" "$work/fixture.sh"
chmod 644 "$work/fixture.sh"
if [ -x "$work/fixture.sh" ]; then
  fail "fixture copy unexpectedly executable"
else
  pass "fixture is mode 0644 and invoked through Bash"
fi

# GREEN: re-exec reaches the boundary, argv survives exactly, status kept.
rm -f "$work/marker" "$work/args.out"
export FAKE_ROOT_UID=0 FAKE_NOBODY_UID=65534 FAKE_NOBODY_GID=65534
export FAKE_DROPPED_UID=65534 PROBE_STATUS=0
unset FAKE_DROPPED || true
PATH="$work/stub:$PATH" run "$bash_bin" "$work/fixture.sh" \
  "$work/probe.sh" first "hello world" "a  b" ""
[ "$got_status" -eq 0 ] \
  && pass "green re-exec exits 0 (got $got_status)" \
  || fail "green re-exec exits 0 (got $got_status)"
[ -f "$work/marker" ] \
  && pass "green re-exec reaches the fixture/test boundary" \
  || fail "green re-exec reaches the fixture/test boundary"
printf 'first\nhello world\na  b\n\n' >"$work/args.want"
if diff "$work/args.want" "$work/args.out" >/dev/null 2>&1; then
  pass "arguments with spaces survive exactly"
else
  fail "arguments with spaces survive exactly"
fi
grep -q TEST-BOUNDARY "$work/stdout.log" \
  && pass "probe output flows through the fixture" \
  || fail "probe output flows through the fixture"

rm -f "$work/marker" "$work/args.out"
export PROBE_STATUS=7
unset FAKE_DROPPED || true
PATH="$work/stub:$PATH" run "$bash_bin" "$work/fixture.sh" \
  "$work/probe.sh" only-arg
[ "$got_status" -eq 7 ] \
  && pass "probe failure status 7 is preserved (got $got_status)" \
  || fail "probe failure status 7 is preserved (got $got_status)"
[ -f "$work/marker" ] \
  && pass "failing probe still reaches the boundary" \
  || fail "failing probe still reaches the boundary"

# RED: the pre-fix direct-"$0" re-exec cannot run a mode-0644 fixture.
sed 's|"${BASH:-/bin/bash}" "[$]0"|"$0"|' "$under_test" >"$work/red.sh"
chmod 644 "$work/red.sh"
if grep -q 'BASH:-' "$work/red.sh"; then
  fail "red variant still uses the explicit-interpreter re-exec"
else
  pass "red variant drops the explicit interpreter like the old form"
fi
if grep -q -- '--clear-groups "$0" "$@"' "$work/red.sh"; then
  pass "red variant re-execs via direct \$0"
else
  fail "red variant re-execs via direct \$0"
fi
rm -f "$work/marker" "$work/args.out"
export PROBE_STATUS=0
unset FAKE_DROPPED || true
PATH="$work/stub:$PATH" run "$bash_bin" "$work/red.sh" "$work/probe.sh" x
[ "$got_status" -ne 0 ] \
  && pass "red direct-\$0 re-exec fails on mode 0644 (got $got_status)" \
  || fail "red direct-\$0 re-exec fails on mode 0644 (got $got_status)"
[ ! -e "$work/marker" ] \
  && pass "red re-exec never reaches the boundary" \
  || fail "red re-exec never reaches the boundary"

# UID0 guard: a `nobody` resolving to uid 0 must be refused, no setpriv call.
rm -f "$work/marker" "$work/sentinel-setpriv-called"
export FAKE_NOBODY_UID=0 FAKE_NOBODY_GID=0
unset FAKE_DROPPED || true
PATH="$work/stub-trip:$PATH" run "$bash_bin" "$work/fixture.sh" \
  "$work/probe.sh" x
[ "$got_status" -eq 3 ] \
  && pass "uid-0 drop is refused with status 3 (got $got_status)" \
  || fail "uid-0 drop is refused with status 3 (got $got_status)"
[ ! -e "$work/marker" ] \
  && pass "uid-0 refusal never reaches the boundary" \
  || fail "uid-0 refusal never reaches the boundary"
[ ! -e "$work/sentinel-setpriv-called" ] \
  && pass "uid-0 refusal never calls setpriv" \
  || fail "uid-0 refusal never calls setpriv"

# Unsupported platform: no setpriv anywhere on PATH must fail, not skip.
rm -f "$work/marker"
export FAKE_NOBODY_UID=65534 FAKE_NOBODY_GID=65534
unset FAKE_DROPPED || true
# shellcheck disable=SC2093
run env -i PATH="$work/nostub" FAKE_ROOT_UID=0 \
  FAKE_NOBODY_UID=65534 FAKE_NOBODY_GID=65534 \
  "$bash_bin" "$work/fixture.sh" "$work/probe.sh" x
[ "$got_status" -eq 4 ] \
  && pass "missing setpriv fails with status 4 (got $got_status)" \
  || fail "missing setpriv fails with status 4 (got $got_status)"
[ ! -e "$work/marker" ] \
  && pass "missing-setpriv failure never reaches the boundary" \
  || fail "missing-setpriv failure never reaches the boundary"

if [ "$fails" -ne 0 ]; then
  printf '%s check(s) FAILED\n' "$fails" >&2
  exit 1
fi
echo "ALL RE-EXEC CHECKS PASSED"
