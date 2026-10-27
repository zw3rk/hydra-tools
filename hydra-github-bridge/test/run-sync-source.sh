#!/usr/bin/env bash
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
set -euo pipefail
umask 077
if [ "$#" -eq 0 ]; then
  echo 'Usage: run-sync-source.sh TEST-COMMAND [ARG...]' >&2
  exit 2
fi

# Only PostgreSQL changes identity. Nix's test command can be inside a private
# directory owned by the caller, so it must retain that identity and cwd.
db_uid=
db_gid=
db_role=hydra_sync_test
if [ "$(id -u)" -eq 0 ]; then
  # Build workers need not provide any ordinary account in /etc/passwd.
  # Only database children receive this fixture-local NSS mapping, after a
  # real UID/GID drop. PostgreSQL itself must still run with a nonzero EUID.
  db_uid=65534
  db_gid=65534
  if [ ! -r "${HYDRA_SYNC_TEST_NSS_WRAPPER:-}" ]; then
    echo 'run-sync-source.sh: root launcher requires the declared nss_wrapper library' >&2
    exit 3
  fi
  if ! command -v setpriv >/dev/null 2>&1; then
    echo "run-sync-source.sh: running as root but setpriv is unavailable; refusing to run PostgreSQL as root" >&2
    exit 4
  fi
fi

test_dir=$(mktemp -d /tmp/hydra-sync-source.XXXXXX)
started=false
as_postgres() {
  if [ -n "$db_uid" ]; then
    # The caller's cwd may also be private. Database subprocesses need only
    # their own directory; the test command continues in the original cwd.
    (cd "$test_dir" && exec setpriv --reuid="$db_uid" --regid="$db_gid" --clear-groups \
      env LD_PRELOAD="$HYDRA_SYNC_TEST_NSS_WRAPPER" \
      NSS_WRAPPER_PASSWD="$test_dir/passwd" NSS_WRAPPER_GROUP="$test_dir/group" "$@")
  else
    "$@"
  fi
}
cleanup() {
  status=$?
  trap - EXIT INT TERM
  if "$started"; then
    as_postgres pg_ctl -D "$test_dir/db" -m immediate -w -t 10 stop >/dev/null || true
  fi
  if [ "$status" -ne 0 ] && [ -f "$test_dir/postgres.log" ]; then cat "$test_dir/postgres.log" >&2 || true; fi
  rm -rf "$test_dir"
  exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
if [ -n "$db_uid" ]; then
  printf 'hydra_sync_fixture:x:%s:%s:Private PostgreSQL fixture:%s:/bin/false\n' "$db_uid" "$db_gid" "$test_dir" >"$test_dir/passwd"
  printf 'hydra_sync_fixture:x:%s:\n' "$db_gid" >"$test_dir/group"
  chown "$db_uid:$db_gid" "$test_dir/passwd" "$test_dir/group"
  chown "$db_uid:$db_gid" "$test_dir"
  [ "$(as_postgres id -u)" = "$db_uid" ]
  [ "$(as_postgres id -un)" = hydra_sync_fixture ]
fi
as_postgres mkdir "$test_dir/socket"
as_postgres initdb --auth=trust --username="$db_role" --no-locale -E UTF8 -D "$test_dir/db" >/dev/null
started=true
as_postgres pg_ctl -D "$test_dir/db" -l "$test_dir/postgres.log" \
  -o "-F -k $test_dir/socket -h '' -p 55432" -w -t 10 start >/dev/null
export HYDRA_SYNC_TEST_DATABASE="host=$test_dir/socket port=55432 dbname=postgres user=$db_role"
"$@"
