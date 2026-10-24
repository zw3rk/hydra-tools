#!/usr/bin/env bash
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
set -euo pipefail
umask 077
fixture=${1:?Usage: test-run-sync-source-ownership.sh FIXTURE}
work=$(mktemp -d /tmp/hydra-sync-ownership.XXXXXX)
trap 'rm -rf "$work"' EXIT
caller_uid=$(id -u)
caller_cwd=$PWD

# Nix copies the test executable into a mode-0700 directory. PostgreSQL
# must be unprivileged while this command keeps the caller's identity,
# cwd and arguments. Exercise real PostgreSQL instead of mocking argv.
printf '#!%s\n' "$BASH" >"$work/check"
cat >>"$work/check" <<'CHECK'
set -euo pipefail
[ "$(id -u)" = "$1" ]
[ "$PWD" = "$2" ]
[ "$4" = 'argument with spaces' ]
[ "$(psql "$HYDRA_SYNC_TEST_DATABASE" -Atc 'SELECT current_user')" = hydra_sync_test ]
printf '%s\n' "$HYDRA_SYNC_TEST_DATABASE" >"$3"
exit "$5"
CHECK
chmod 755 "$work/check"

verify_cleanup() {
  local connection socket
  connection=$(cat "$work/connection")
  socket=${connection#host=}
  socket=${socket%% *}
  [ ! -e "${socket%/socket}" ]
}

"$BASH" "$fixture" "$work/check" "$caller_uid" "$caller_cwd" "$work/connection" 'argument with spaces' 0
verify_cleanup
echo 'PASS private test-command identity/cwd/arguments and PostgreSQL ownership'

status=0
"$BASH" "$fixture" "$work/check" "$caller_uid" "$caller_cwd" "$work/connection" 'argument with spaces' 17 >"$work/failure.log" 2>&1 || status=$?
if [ "$status" -ne 17 ]; then
  cat "$work/failure.log" >&2
  echo "Expected test status 17, got $status" >&2
  exit 1
fi
verify_cleanup
echo 'PASS failed test status and private PostgreSQL cleanup'
