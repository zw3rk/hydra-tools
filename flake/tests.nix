{inputs, ...} @ parts: {
  perSystem = {
    config,
    pkgs,
    system,
    lib,
    ...
  } @ perSystem:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.test = inputs.nixpkgs.lib.nixos.runTest ({nodes, ...}: {
        name = "test";

        hostPkgs = pkgs;

        nodes = {
          hydra = {...}: {
            imports = [
              parts.config.flake.nixosModules.hydra-github-bridge
            ];

            environment.systemPackages = [
              config.packages.mockoon-cli
              pkgs.postgresql
            ];

            services = {
              hydra = {
                enable = true;
                hydraURL = "http://localhost:3000";
                notificationSender = "admin@iohk.io";
                useSubstitutes = false;
              };

              hydra-github-bridge.all = {
                enable = true;
                ghAppId = 12345;
                ghAppInstallIds = "[(\"input-output-hk\", 98765)]";
                ghUserAgent = "hydra-github-bridge";
                ghAppKeyFile =
                  pkgs.runCommand
                  "app-key-file"
                  {nativeBuildInputs = [pkgs.openssl];}
                  "openssl genrsa -out $out";
                ghTokenFile = pkgs.writeText "gh-secret-file" "secret-token";
                ghSecretFile = pkgs.writeText "gh-secret-file" "secret-token";
                hydraHost = "http://localhost:3000";
                hydraUser = "bridge";
                hydraPassFile = pkgs.writeText "hydra-pass-file" "hydra";
                port = 8811;
                environmentFile = pkgs.writeText "env" ''
                  GITHUB_ENDPOINT_URL=http://localhost:4010
                '';
              };
            };

            systemd.services = {
              hydra-init.postStart = ''
                # Create a Hydra user for the bridge

                # Run required SQL migration
                # TODO: It would be better if this were in the application startup logic
                ${pkgs.postgresql}/bin/psql hydra hydra <<EOF
                  CREATE TABLE IF NOT EXISTS github_commands (
                      id SERIAL PRIMARY KEY,
                      command JSONB NOT NULL,
                      created TIMESTAMP DEFAULT NOW(),
                      processed TIMESTAMP DEFAULT NULL
                  );

                  CREATE TABLE github_status (
                      id SERIAL PRIMARY KEY,
                      owner TEXT NOT NULL,
                      repo TEXT NOT NULL,
                      headSha TEXT NOT NULL,
                      name TEXT NOT NULL,
                      UNIQUE (owner, repo, headSha, name)
                  );

                  CREATE TABLE github_status_payload (
                      id SERIAL PRIMARY KEY,
                      status_id INTEGER NOT NULL,
                      payload JSONB NOT NULL,
                      created TIMESTAMP DEFAULT NOW(),
                      sent TIMESTAMP DEFAULT NULL,
                      tries INTEGER DEFAULT 0,
                      FOREIGN KEY (status_id) REFERENCES github_status (id) ON DELETE CASCADE
                  );
                EOF
              '';

              # Start the mock server
              mock-github = {
                wantedBy = ["multi-user.target"];
                serviceConfig.ExecStart = "${config.packages.mockoon-cli}/bin/mockoon-cli start --data ${../mock-github-data.json} --port 4010 --repair";
              };

              hydra-github-bridge-all = {
                # These will fail until Hydra and Mock GitHub are running, so we'll start
                # them manually
                wantedBy = lib.mkForce [];
                # We'll want the test to fail if they crash
                serviceConfig.Restart = lib.mkForce "no";
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
        '';
      });
    };
}
