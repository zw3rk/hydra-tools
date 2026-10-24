#!/usr/bin/env bash
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
set -euo pipefail
umask 077
if [ "$#" -eq 0 ]; then
  echo 'Usage: run-sync-source.sh TEST-COMMAND [ARG...]' >&2
  exit 2
fi

# PostgreSQL refuses to run as root. When a root launcher invokes this
# fixture, re-exec it as the existing unprivileged `nobody` identity so the
# throwaway cluster is created by an ordinary user. Non-root invocations skip
# this block unchanged. No accounts are created or modified; when no supported
# drop mechanism exists, fail instead of skipping or faking success.
if [ "$(id -u)" -eq 0 ]; then
  drop_user=nobody
  drop_uid=$(id -u "$drop_user" 2>/dev/null || true)
  drop_gid=$(id -g "$drop_user" 2>/dev/null || true)
  if [ -z "$drop_uid" ] || [ -z "$drop_gid" ]; then
    echo "run-sync-source.sh: running as root but user '$drop_user' is unavailable; refusing to run PostgreSQL as root" >&2
    exit 3
  fi
  if command -v setpriv >/dev/null 2>&1; then
    exec setpriv --reuid="$drop_uid" --regid="$drop_gid" --clear-groups "$0" "$@"
  elif command -v runuser >/dev/null 2>&1; then
    exec runuser -u "$drop_user" -g "$drop_gid" -- "$0" "$@"
  else
    echo "run-sync-source.sh: running as root but neither setpriv nor runuser is available; refusing to run PostgreSQL as root" >&2
    exit 4
  fi
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
