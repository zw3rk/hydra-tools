#!/usr/bin/env bash
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
set -euo pipefail
umask 077
if [ "$#" -eq 0 ]; then
  echo 'Usage: run-sync-source.sh TEST-COMMAND [ARG...]' >&2
  exit 2
fi

test_dir=$(mktemp -d /tmp/hydra-sync-source.XXXXXX)
started=false
cleanup() {
  status=$?
  trap - EXIT INT TERM
  if "$started"; then
    pg_ctl -D "$test_dir/db" -m immediate -w -t 10 stop >/dev/null || true
  fi
  if [ "$status" -ne 0 ]; then cat "$test_dir/postgres.log" >&2 || true; fi
  rm -rf "$test_dir"
  exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
mkdir "$test_dir/socket"
initdb --auth=trust --no-locale -E UTF8 -D "$test_dir/db" >/dev/null
started=true
pg_ctl -D "$test_dir/db" -l "$test_dir/postgres.log" \
  -o "-F -k $test_dir/socket -h '' -p 55432" -w -t 10 start >/dev/null
export HYDRA_SYNC_TEST_DATABASE="host=$test_dir/socket port=55432 dbname=postgres"
"$@"
