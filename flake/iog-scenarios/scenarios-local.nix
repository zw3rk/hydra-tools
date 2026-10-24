# Local IOG scenario batch, adapted to the current bridge.
#
# Source: upstream `flake/test/scenarios.nix` (iog/master), vendored
# fixtures under this directory. Adaptations mirror integration-local.nix:
# multi-instance module via ./setup-local.nix, `iog-send-webhook` delivery,
# canonical `github:` flake URIs. Build-log handling (plain, bz2, invalid
# UTF-8 plain/bz2, missing) exercises the same HydraToGitHub paths.
{inputs, ...} @ parts: {
  perSystem = {
    config,
    pkgs,
    system,
    lib,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.iog-scenarios-local = let
        prOpenedPayload = ./pr_opened.payload.txt;
        createTestBuildSql = ./create-test-build.sql;
      in
        inputs.nixpkgs.lib.nixos.runTest ({nodes, ...}: {
          name = "iog-scenarios-local";
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
            hydra.wait_for_open_port(8811, timeout=20)
            hydra.wait_for_open_port(8812, timeout=20)

            # Use the bridge to create a jobset via webhook
            hydra.succeed("iog-send-webhook http://localhost:8811/hook pull_request < ${prOpenedPayload}")
            hydra.wait_until_succeeds(
              "hydra-cli -H http://localhost:3000 project-show input-output-hk-sample -j | "
              "jq --exit-status 'map(select(.name == \"pullrequest-1347\")) | length > 0'",
              timeout=20
            )

            # Helper to create a build with specific log scenarios
            def forge_failed_build(drv_path, drv_name, job_name="required.test"):
              # Record the existing check runs
              initial_check_runs = int(hydra.succeed(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length'"
              ).strip())

              # Forge a failed build in the Hydra database
              build_id = hydra.succeed(
                f"psql hydra hydra -q -t -v drv_path='{drv_path}' -v job_name='{job_name}' -f ${createTestBuildSql}"
              ).strip()

              # Trigger notification
              hydra.succeed(f"psql hydra hydra -c \"SELECT pg_notify('build_finished', '{build_id}')\"")

              # Verify check run was created
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status '"
                f"map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length > {initial_check_runs}'",
                timeout=20
              )

            with subtest("Build log is included"):
              # Create plain text log file
              hydra.succeed(
                "mkdir -p /var/lib/hydra/build-logs/aa && "
                "echo -e 'build log' > "
                "/var/lib/hydra/build-logs/aa/aabbbbccccddddeeeeffffgggghhhh-test1.drv"
              )

              # Forge a failed build and verify check run was created
              forge_failed_build(
                "/nix/store/aaaabbbbccccddddeeeeffffgggghhhh-test1.drv",
                "aaaabbbbccccddddeeeeffffgggghhhh-test1.drv",
              )

              # And that it includes the build log
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "last | .request.body | fromjson | .output.text | contains(\"build log\")'",
                timeout=20
              )

            with subtest("Compressed log is included"):
              # Create bz2 compressed log file
              hydra.succeed(
                "mkdir -p /var/lib/hydra/build-logs/ii && "
                "echo -e 'compressed build log' | "
                "bzip2 > /var/lib/hydra/build-logs/ii/iijjjjkkkkllllmmmmnnnnoooopppp-test2.drv.bz2"
              )

              # Forge a failed build and verify check run was created
              forge_failed_build(
                "/nix/store/iiiijjjjkkkkllllmmmmnnnnoooopppp-test2.drv",
                "iiiijjjjkkkkllllmmmmnnnnoooopppp-test2.drv",
                "required.compressed"
              )

              # And that it includes the build log
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "last | .request.body | fromjson | .output.text | contains(\"compressed build log\")'",
                timeout=20
              )

            with subtest("Handles invalid UTF-8 log gracefully"):
              # Create log with invalid UTF-8 bytes
              hydra.succeed(
                "mkdir -p /var/lib/hydra/build-logs/qq && "
                "printf 'Valid text\\n\\xff\\xfe\\x80\\x81Invalid bytes\\n' > "
                "/var/lib/hydra/build-logs/qq/qqrrrrssssttttuuuuvvvvwwwwxxxx-test3.drv"
              )

              # Forge a failed build and verify check run was created
              forge_failed_build(
                "/nix/store/qqqqrrrrssssttttuuuuvvvvwwwwxxxx-test3.drv",
                "qqqqrrrrssssttttuuuuvvvvwwwwxxxx-test3.drv",
                "required.invalid-plain"
              )

              # The bridge must still report the failure (not drop the
              # check run or send it without a result body)
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "last | .request.body | fromjson | "
                ".conclusion == \"failure\" and .output.text != null'",
                timeout=20
              )

            with subtest("Handles invalid compressed UTF-8 log gracefully"):
              # Create bz2 with invalid UTF-8
              hydra.succeed(
                "mkdir -p /var/lib/hydra/build-logs/yy && "
                "printf 'Start\\n\\xff\\xfe\\x80\\x81\\x82\\n' | "
                "bzip2 > /var/lib/hydra/build-logs/yy/yyzzzzaabbccddeeeeffff11112222-test4.drv.bz2"
              )

              # Forge a failed build and verify check run was created
              forge_failed_build(
                "/nix/store/yyyyzzzzaabbccddeeeeffff11112222-test4.drv",
                "yyyyzzzzaabbccddeeeeffff11112222-test4.drv",
                "required.invalid-compressed"
              )

              # The bridge must still report the failure (not drop the
              # check run or send it without a result body)
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "last | .request.body | fromjson | "
                ".conclusion == \"failure\" and .output.text != null'",
                timeout=20
              )

            with subtest("Handles missing log files gracefully"):
              # Don't create any log file

              # Forge a failed build and verify check run was created
              forge_failed_build(
                "/nix/store/33334444555566667777888899990000-test5.drv",
                "33334444555566667777888899990000-test5.drv",
                "required.missing"
              )

              # The bridge must still report the failure (not drop the
              # check run or send it without a result body)
              hydra.wait_until_succeeds(
                "curl -s 'http://localhost:4010/mockoon-admin/logs?limit=100' | "
                "jq --exit-status '"
                "map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | "
                "last | .request.body | fromjson | "
                ".conclusion == \"failure\" and .output.text != null'",
                timeout=20
              )
          '';
        });
    };
}
