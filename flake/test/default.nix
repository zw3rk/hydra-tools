{inputs, ...} @ parts: {
  perSystem = {
    config,
    pkgs,
    system,
    lib,
    ...
  } @ perSystem:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.test = let
        prOpenedPayload = ./pr_opened.payload.txt;
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
            # Wait for Hydra to start
            hydra.wait_for_unit("hydra-server.service")
            hydra.wait_for_open_port(3000)
            # Add a Hydra API user
            hydra.succeed("hydra-create-user bridge --password hydra --role admin")

            # Wait for GitHub Mock server
            hydra.wait_for_unit("mock-github.service")
            hydra.wait_for_open_port(4010)

            # Start hydra-github-bridge
            hydra.systemctl("start hydra-github-bridge-all.service")
            hydra.wait_for_unit("hydra-github-bridge-all.service")
            hydra.wait_for_open_port(8811, timeout=15)

            # Send a fake GitHub webhook
            hydra.succeed("fake-send-webhook http://localhost:8811/hook pull_request < ${prOpenedPayload}")

            # Verify the jobset was created. Eval will fail because it can't connect to
            # GitHub. That's okay, we can use the DB to trigger build status.
            hydra.wait_until_succeeds(
              "hydra-cli -H http://localhost:3000 project-show input-output-hk-sample -j | "
              "jq --exit-status 'map(select(.name == \"pullrequest-1347\")) | length > 0'",
              timeout=15
            )

            # Eval will fail because it can't connect to GitHub. That's okay, it will
            # still report at least two statuses to GitHub (in_progress and completed).
            hydra.wait_until_succeeds(
              "curl http://localhost:4010/mockoon-admin/logs | "
              "jq --exit-status 'map(select(.request.urlPath == \"/repos/input-output-hk/sample/check-runs\")) | length >= 2'",
              timeout=15
            )
          '';
        });
    };
}
