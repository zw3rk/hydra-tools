# Local IOG integration batch, adapted to the current bridge.
#
# Source: upstream `flake/test/integration.nix` (iog/master), vendored
# fixtures under this directory. Adaptations for the current tree:
# - multi-instance module `services.hydra-github-bridge.all` via
#   ./setup-local.nix (Haskell-list install ids, shared secret-token);
# - bridge default keepnr is 2 (no `hydraKeepEvals` option), so jobset
#   assertions check `.keepnr == 2`;
# - flake URIs are the canonical `github:owner/repo/sha` form (see
#   Lib.Bridge.Source), not `git+https:...&submodules=1`;
# - webhook delivery via local ./send-webhook.nix (`iog-send-webhook`).
#
# Retry coverage: upstream fix-retries (#52, iog/master
# 11a0629/9bf8629/1e33bb3) — per-job eval failure check-runs
# (`ci/eval:<job>`) and clearing stale failure statuses on Hydra retry —
# is exercised by the "Retrying a failed eval ..." subtest below;
# see README.md.
{inputs, ...} @ parts: {
  perSystem = {
    config,
    pkgs,
    system,
    lib,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.iog-integration-local = let
        prOpenedPayload = ./pr_opened.payload.txt;
        pushedPayload = ./push.payload.txt;
        checkSuitePayload = ./check_suite.payload.txt;
        checkRunPayload = ./check_run.payload.txt;

        # Seed a fake failed build into the Hydra database
        seedFailedBuildSql = ./seed-failed-build.sql;
      in
        inputs.nixpkgs.lib.nixos.runTest ({nodes, ...}: {
          name = "iog-integration-local";
          hostPkgs = pkgs;

          # Pass packages to NixOS modules
          defaults._module.args.flakePackages = config.packages;

          nodes = {
            hydra = {...}: {
              imports = [
                parts.config.flake.nixosModules.hydra-github-bridge
                ./setup-local.nix
              ];

              virtualisation.memorySize = 2048;
            };
          };

          testScript = ''
            start_all()

            # The bridge needs its Hydra user to be created first
            hydra.systemctl("stop hydra-github-bridge-all.service")

            # Wait for Hydra to start
            hydra.wait_for_unit("hydra-server.service")
            # A running Starman unit can precede its listening socket.
            hydra.wait_for_open_port(3000)
            hydra.wait_for_unit("mock-github.service")
            hydra.wait_for_open_port(4010)

            # Create the bridge user and start the bridge
            hydra.succeed("hydra-create-user bridge --password hydra --role admin")
            hydra.systemctl("start hydra-github-bridge-all.service")

            # Wait for hydra-github-bridge (webhooks on 8811, SSE on 8812)
            hydra.wait_for_unit("hydra-github-bridge-all.service")
            hydra.wait_for_open_port(8811, timeout=15)
            hydra.wait_for_open_port(8812, timeout=15)

            with subtest("Opening a PR creates check runs"):
              # Send a PR opened webhook request
              hydra.succeed("iog-send-webhook http://localhost:8811/hook pull_request < ${prOpenedPayload}")

              # Verify the jobset was created
              hydra.wait_until_succeeds(
                "hydra-cli -H http://localhost:3000 project-show input-output-hk-sample -j | "
                "jq --exit-status 'map(select(.name == \"pullrequest-1347\")) | length > 0'",
                timeout=20
              )
              # Verify it has the desired properties (current default keepnr 2)
              hydra.wait_until_succeeds(
                "curl -H \"Accept: application/json\" "
                "http://localhost:3000/jobset/input-output-hk-sample/pullrequest-1347 | "
                "jq --exit-status '.keepnr == 2'",
                timeout=15
              )

              # Eval will fail because it can't connect to GitHub. That's okay, it will
              # still report at least two statuses to GitHub (in_progress and completed).
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 2'",
                timeout=20
              )

            with subtest("Rerequesting a check suite updates Hydra jobset"):
              # Send a check_suite rerequested webhook request
              hydra.succeed("iog-send-webhook http://localhost:8811/hook check_suite < ${checkSuitePayload}")

              # Verify the jobset was updated (canonical github: flake URI)
              hydra.wait_until_succeeds(
                "curl -H \"Accept: application/json\" "
                "http://localhost:3000/jobset/input-output-hk-sample/pullrequest-1347 | "
                "jq --exit-status '.flake == \"github:input-output-hk/sample/d6fde92930d4715a2b49857d24b940956b26d2d3\"'",
                timeout=20
              )

              # This should trigger another eval/build
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 4'",
                timeout=20
              )

            with subtest("Rerequesting a check run evals Hydra jobset"):
              # Send a check_run rerequested webhook request
              hydra.succeed("iog-send-webhook http://localhost:8811/hook check_run < ${checkRunPayload}")

              # Verify an eval/build was triggered
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 6'",
                timeout=20
              )

            with subtest("Closing a PR disables its Hydra jobset"):
              # Send a PR closed webhook request
              hydra.succeed(
                "jq '.action = \"closed\"' \"${prOpenedPayload}\" | "
                "iog-send-webhook http://localhost:8811/hook pull_request"
              )

              # The jobset should now be disabled
              hydra.wait_until_succeeds(
                "curl -H \"Accept: application/json\" "
                "http://localhost:3000/jobset/input-output-hk-sample/pullrequest-1347 | "
                "jq --exit-status '.enabled == 0'",
                timeout=15
              )

            with subtest("Pushing creates check runs"):
              # Send a push webhook request
              hydra.succeed("iog-send-webhook http://localhost:8811/hook push < ${pushedPayload}")

              # Verify the jobset was created
              hydra.wait_until_succeeds(
                "hydra-cli -H http://localhost:3000 project-show input-output-hk-sample -j | "
                "jq --exit-status 'map(select(.name == \"main\")) | length > 0'",
                timeout=20
              )
              # Verify it has the desired properties (current default keepnr 2)
              hydra.wait_until_succeeds(
                "curl -H \"Accept: application/json\" "
                "http://localhost:3000/jobset/input-output-hk-sample/main | "
                "jq --exit-status '.keepnr == 2'",
                timeout=15
              )

              # Verify an eval/build was triggered
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 8'",
                timeout=20
              )

            with subtest("Failed build with invalid log updates check runs"):
              # Seed a failed build in the Hydra DB
              hydra.succeed("psql hydra hydra -f ${seedFailedBuildSql}")

              # Create a build log file with invalid bytes (Windows UTF-16 BOM + raw binary).
              hydra.succeed(
                "mkdir -p /var/lib/hydra/build-logs/pl && "
                "printf '\\xff\\xfe\\x00build output\\x80\\x81\\x82' > "
                "/var/lib/hydra/build-logs/pl/y5hyh8fxwkp82jsp42h8l8aq6zw700-test.drv.bz2"
              )

              # Fire the build_finished notification directly via psql
              hydra.succeed(
                "psql hydra hydra -c "
                "\"SELECT pg_notify('build_finished', id::text) "
                "FROM (SELECT id FROM builds ORDER BY id DESC LIMIT 1) AS latest\""
              )

              # Verify another check run was created
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 9'",
                timeout=20
              )

              hydra.succeed("systemctl is-active hydra-github-bridge-all.service")

            with subtest("Events from unauthorized repos are ignored"):
              # Create a new PR for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${prOpenedPayload}\" > /tmp/pr_unauthorized.json"
              )
              hydra.succeed(
                "iog-send-webhook http://localhost:8811/hook pull_request < /tmp/pr_unauthorized.json"
              )

              # Create a push event for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${pushedPayload}\" > /tmp/push_unauthorized.json"
              )
              hydra.succeed(
                "iog-send-webhook http://localhost:8811/hook push < /tmp/push_unauthorized.json"
              )

              # Create a check_suite event for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${checkSuitePayload}\" > /tmp/check_suite_unauthorized.json"
              )
              hydra.succeed(
                "iog-send-webhook http://localhost:8811/hook check_suite < /tmp/check_suite_unauthorized.json"
              )

              # Create a check_run event for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${checkRunPayload}\" > /tmp/check_run_unauthorized.json"
              )
              hydra.succeed(
                "iog-send-webhook http://localhost:8811/hook check_run < /tmp/check_run_unauthorized.json"
              )

              # Wait for the queue to process, then verify project was not created
              hydra.sleep(20)
              hydra.fail(
                "hydra-cli -H http://localhost:3000 project-show unknown-owner-unknown -j"
              )

              # Send a PR closed. There's no way to verify the unknown project's jobset
              # wasn't updated. The request would have failed anyways, since the project
              # doesn't exist. Nevertheless, it's still better to exercise this than not.
              hydra.succeed(
                "jq '.action = \"closed\"' /tmp/pr_unauthorized.json | "
                "iog-send-webhook http://localhost:8811/hook pull_request"
              )

            # The remaining cases control evaluation rows explicitly. Drain
            # webhook commands first, then stop only the private VM producers
            # so they cannot overwrite the notification fixture's SQL state.
            hydra.wait_until_succeeds(
              "psql -At hydra hydra -c 'SELECT count(*) FROM github_commands WHERE processed IS NULL' | grep -qx 0",
              timeout=20
            )
            hydra.systemctl("stop hydra-evaluator.service hydra-queue-runner.service")

            with subtest("Retrying a failed eval clears stale per-job eval failures"):
              # Upstream fix-retries (#52: 11a0629, 9bf8629, 1e33bb3):
              # per-job eval failures are reported as `ci/eval:<job>`
              # check-runs, and a later successful eval overwrites each
              # stale failure with "Evaluation succeeded on retry". Forge
              # the fail-then-retry sequence against the PR jobset: point
              # it at a per-job eval error and notify the bridge, then
              # clear the error and notify again. Each phase is a single
              # statement so the shared jobset row cannot race Hydra's
              # evaluator between the update and the notification.
              # NOTE: the forged errormsg carries real newlines via \n escapes
              # (a literal source newline would be a Python syntax error):
              # `parseFailedJobEvals` only matches `in job ...` on its own line.
              hydra.succeed(
                "psql hydra hydra -c \""
                "WITH j AS (SELECT id FROM jobsets WHERE project = 'input-output-hk-sample' AND name = 'pullrequest-1347'), "
                "u AS (UPDATE jobsets SET errormsg = 'evaluation failed\nin job ‘retry-job’:\n  boom', fetcherrormsg = NULL WHERE id = (SELECT id FROM j)), "
                "e AS (INSERT INTO jobsetevals (jobset_id, timestamp, checkouttime, evaltime, hasnewbuilds, hash, flake) "
                "SELECT id, extract(epoch from now())::int, 1, 1, 1, 'd6fde92930d4715a2b49857d24b940956b26d2d3', 'github:input-output-hk/sample/d6fde92930d4715a2b49857d24b940956b26d2d3' FROM j RETURNING id, jobset_id) "
                "SELECT pg_notify('eval_added', '0 ' || (SELECT jobset_id FROM e) || ' ' || (SELECT id FROM e))\""
              )

              # The per-job failure is reported alongside the `ci/eval` failure.
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "map(.request.body | fromjson) | "
                "map(select(.name == \"ci/eval:retry-job\" and .conclusion == \"failure\")) | length >= 1'",
                timeout=20
              )

              # A failed eval never clears: no retry-success payload exists yet.
              hydra.succeed(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | " +
                "jq --exit-status '" +
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | " +
                "map(.request.body | fromjson) | " +
                "map(select(.name == \"ci/eval:retry-job\" and .conclusion == \"success\" and .output.title == \"Evaluation succeeded on retry\")) | length == 0'"
              )

              # Baseline `ci/eval` successes before the retry so the next
              # assertion proves the retry re-emitted success (all evals
              # share one flake hash, so an unscoped `>= 1` could match
              # history).
              ci_eval_success_before = int(hydra.succeed(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | "
                "jq 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "map(.request.body | fromjson) | "
                "map(select(.name == \"ci/eval\" and .conclusion == \"success\")) | length'"
              ).strip())

              # Retry the eval to success and notify the bridge again.
              hydra.succeed(
                "psql hydra hydra -c \""
                "WITH j AS (SELECT id FROM jobsets WHERE project = 'input-output-hk-sample' AND name = 'pullrequest-1347'), "
                "u AS (UPDATE jobsets SET errormsg = NULL, fetcherrormsg = NULL WHERE id = (SELECT id FROM j)), "
                "e AS (INSERT INTO jobsetevals (jobset_id, timestamp, checkouttime, evaltime, hasnewbuilds, hash, flake) "
                "SELECT id, extract(epoch from now())::int, 1, 1, 1, 'd6fde92930d4715a2b49857d24b940956b26d2d3', 'github:input-output-hk/sample/d6fde92930d4715a2b49857d24b940956b26d2d3' FROM j RETURNING id, jobset_id) "
                "SELECT pg_notify('eval_added', '0 ' || (SELECT jobset_id FROM e) || ' ' || (SELECT id FROM e))\""
              )

              # The retry re-emits `ci/eval` as success ...
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "map(.request.body | fromjson) | "
                f"map(select(.name == \"ci/eval\" and .conclusion == \"success\")) | length > {ci_eval_success_before}'",
                timeout=20
              )

              # ... and overwrites the stale per-job failure.
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "map(.request.body | fromjson) | "
                "map(select(.name == \"ci/eval:retry-job\" and .conclusion == \"success\" and .output.title == \"Evaluation succeeded on retry\")) | length == 1'",
                timeout=20
              )

              # A second successful eval finds nothing left to clear: the
              # cleared payload is not re-emitted.
              ci_eval_success_retry = int(hydra.succeed(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | " +
                "jq 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | " +
                "map(.request.body | fromjson) | " +
                "map(select(.name == \"ci/eval\" and .conclusion == \"success\")) | length'"
              ).strip())
              hydra.succeed(
                "psql hydra hydra -c \"" +
                "WITH j AS (SELECT id FROM jobsets WHERE project = 'input-output-hk-sample' AND name = 'pullrequest-1347'), " +
                "u AS (UPDATE jobsets SET errormsg = NULL, fetcherrormsg = NULL WHERE id = (SELECT id FROM j)), " +
                "e AS (INSERT INTO jobsetevals (jobset_id, timestamp, checkouttime, evaltime, hasnewbuilds, hash, flake) " +
                "SELECT id, extract(epoch from now())::int, 1, 1, 1, 'd6fde92930d4715a2b49857d24b940956b26d2d3', 'github:input-output-hk/sample/d6fde92930d4715a2b49857d24b940956b26d2d3' FROM j RETURNING id, jobset_id) " +
                "SELECT pg_notify('eval_added', '0 ' || (SELECT jobset_id FROM e) || ' ' || (SELECT id FROM e))\""
              )
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | " +
                "jq --exit-status '" +
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | " +
                "map(.request.body | fromjson) | " +
                f"map(select(.name == \"ci/eval\" and .conclusion == \"success\")) | length > {ci_eval_success_retry}'",
                timeout=20
              )
              hydra.succeed(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=200' | " +
                "jq --exit-status '" +
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | " +
                "map(.request.body | fromjson) | " +
                "map(select(.name == \"ci/eval:retry-job\" and .conclusion == \"success\" and .output.title == \"Evaluation succeeded on retry\")) | length == 1'"
              )

              # A recorded request alone may have received an HTTP error.
              # sent is set by the real status handler only after API success.
              hydra.wait_until_succeeds(
                "psql -At hydra hydra -c \"SELECT count(DISTINCT p.payload->>'conclusion') "
                "FROM github_status s JOIN github_status_payload p ON p.status_id = s.id "
                "WHERE s.owner = 'input-output-hk' AND s.repo = 'sample' "
                "AND s.headSha = 'd6fde92930d4715a2b49857d24b940956b26d2d3' "
                "AND s.name = 'ci/eval:retry-job' AND p.sent IS NOT NULL "
                "AND p.payload->>'conclusion' IN ('failure','success')\" | grep -qx 2",
                timeout=20
              )

              hydra.succeed("systemctl is-active hydra-github-bridge-all.service")
          '';
        });
    };
}
