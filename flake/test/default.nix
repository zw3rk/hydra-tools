{inputs, ...} @ parts: {
  perSystem = {
    config,
    pkgs,
    system,
    lib,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.test = let
        prOpenedPayload = ./pr_opened.payload.txt;
        prUnknownPayload = ./pr_opened_unknown.payload.txt;
        pushedPayload = ./push.payload.txt;
        checkSuitePayload = ./check_suite.payload.txt;
        checkRunPayload = ./check_run.payload.txt;
      in
        inputs.nixpkgs.lib.nixos.runTest ({nodes, ...}: {
          name = "test";
          hostPkgs = pkgs;

          # Pass packages to NixOS modules
          defaults._module.args.flakePackages = config.packages;

          nodes = {
            hydra = {...}: {
              imports = [
                parts.config.flake.nixosModules.hydra-github-bridge
                ./setup.nix
              ];

              virtualisation.memorySize = 2048;

              environment = {
                systemPackages = with pkgs; [
                  # Sending fake GitHub webhook requests
                  config.packages.fake-send-webhook

                  hydra-cli
                  jq
                ];

                variables = {
                  # Required by `fake-send-webhook` in the test script
                  WEBHOOK_SECRET = "secret-token";
                };
              };
            };
          };

          testScript = ''
            start_all()

            # The bridge needs its Hydra user to be created first
            hydra.systemctl("stop hydra-github-bridge.target")

            # Wait for Hydra to start
            hydra.wait_for_unit("hydra-server.service")
            hydra.wait_for_open_port(3000)

            # Create the bridge user and start the bridge
            hydra.succeed("hydra-create-user bridge --password hydra --role admin")
            hydra.systemctl("start hydra-github-bridge.target")

            # Wait for GitHub Mock server
            hydra.wait_for_unit("mock-github.service")
            hydra.wait_for_open_port(4010)

            # Wait for hydra-github-bridge
            hydra.wait_for_unit("hydra-github-bridge-all.service")
            hydra.wait_for_open_port(8811, timeout=15)

            with subtest("Opening a PR creates check runs"):
              # Send a PR opened webhook request
              hydra.succeed("fake-send-webhook http://localhost:8811/hook pull_request < ${prOpenedPayload}")

              # Verify the jobset was created
              hydra.wait_until_succeeds(
                "hydra-cli -H http://localhost:3000 project-show input-output-hk-sample -j | "
                "jq --exit-status 'map(select(.name == \"pullrequest-1347\")) | length > 0'",
                timeout=15
              )

              # Eval will fail because it can't connect to GitHub. That's okay, it will
              # still report at least two statuses to GitHub (in_progress and completed).
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 2'",
                timeout=15
              )

            with subtest("Rerequesting a check suite updates Hydra jobset"):
              # Send a check_suite rerequested webhook request
              hydra.succeed("fake-send-webhook http://localhost:8811/hook check_suite < ${checkSuitePayload}")

              # Verify the jobset was updated
              hydra.wait_until_succeeds(
                "curl -H \"Accept: application/json\" "
                "http://localhost:3000/jobset/input-output-hk-sample/pullrequest-1347 | "
                "jq --exit-status '.flake == \"github:input-output-hk/sample/d6fde92930d4715a2b49857d24b940956b26d2d3\"'",
                timeout=15
              )

              # This should trigger another eval/build
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 4'",
                timeout=15
              )

            with subtest("Rerequesting a check run evals Hydra jobset"):
              # Send a check_suite rerequested webhook request
              hydra.succeed("fake-send-webhook http://localhost:8811/hook check_run < ${checkRunPayload}")

              # Verify an eval/build was triggered
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 6'",
                timeout=15
              )

            with subtest("Closing a PR disables its Hydra jobset"):
              # Send a PR closed webhook request
              hydra.succeed(
                "jq '.action = \"closed\"' \"${prOpenedPayload}\" | "
                "fake-send-webhook http://localhost:8811/hook pull_request"
              )

              # The jobset should now be disabled
              hydra.wait_until_succeeds(
                "curl -H \"Accept: application/json\" "
                "http://localhost:3000/jobset/input-output-hk-sample/pullrequest-1347 | "
                "jq --exit-status '.enabled == 0'"
              )

            with subtest("Pushing creates check runs"):
              # Send a PR opened webhook request
              hydra.succeed("fake-send-webhook http://localhost:8811/hook push < ${pushedPayload}")

              # Verify the jobset was created
              hydra.wait_until_succeeds(
                "hydra-cli -H http://localhost:3000 project-show input-output-hk-sample -j | "
                "jq --exit-status 'map(select(.name == \"main\")) | length > 0'",
                timeout=15
              )

              # Verify an eval/build was triggered
              hydra.wait_until_succeeds(
                "curl http://localhost:4010/mockoon-admin/logs?limit=100 | "
                "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 8'",
                timeout=15
              )

            with subtest("Events from unauthorized repos are ignored"):
              # Create a new PR for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${prOpenedPayload}\" > /tmp/pr_unauthorized.json"
              )
              hydra.succeed(
                "fake-send-webhook http://localhost:8811/hook pull_request < /tmp/pr_unauthorized.json"
              )

              # Create a push event for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${pushedPayload}\" > /tmp/push_unauthorized.json"
              )
              hydra.succeed(
                "fake-send-webhook http://localhost:8811/hook push < /tmp/push_unauthorized.json"
              )

              # Create a check_suite event for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${checkSuitePayload}\" > /tmp/check_suite_unauthorized.json"
              )
              hydra.succeed(
                "fake-send-webhook http://localhost:8811/hook check_suite < /tmp/check_suite_unauthorized.json"
              )

              # Create a check_run event for an unauthorized repo
              hydra.succeed(
                "jq '.repository.full_name = \"unknown-owner/unknown\" | "
                ".repository.owner.login = \"unknown-owner\" | "
                ".organization.login = \"unknown-owner\" | "
                ".installation.id = 2' \"${checkRunPayload}\" > /tmp/check_run_unauthorized.json"
              )
              hydra.succeed(
                "fake-send-webhook http://localhost:8811/hook check_run < /tmp/check_run_unauthorized.json"
              )

              # Wait for the queue to process, then verify project was not created
              hydra.sleep(15)
              hydra.fail(
                "hydra-cli -H http://localhost:3000 project-show unknown-owner-unknown -j"
              )

              # Send a PR closed. There's no way to verify the unknown project's jobset
              # wasn't updated. The request would have failed anyways, since the project
              # doesn't exist. Nevertheless, it's still better to exercise this than not.
              hydra.succeed(
                "jq '.action = \"closed\"' /tmp/pr_unauthorized.json | "
                "fake-send-webhook http://localhost:8811/hook pull_request"
              )
          '';
        });
    };
}
