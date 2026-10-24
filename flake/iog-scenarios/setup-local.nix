# Shared NixOS fragment for the local IOG-scenario batch.
#
# Adapted from upstream `flake/test/setup.nix` (iog/master) to the current
# multi-instance module API (`services.hydra-github-bridge.<name>` with
# Haskell-list `ghAppInstallIds`) and the current bridge defaults
# (keepnr 2, `github:owner/repo/sha` flake URIs, no `hydraKeepEvals` /
# `waitForHydraServerPort` options). All fixtures and endpoints are
# private/local only: loopback Hydra, loopback mock GitHub, shared
# `secret-token` secret, installation id 1.
{
  pkgs,
  lib,
  flakePackages,
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Local webhook signer (openssl + jq + curl; no hand crypto).
    (import ./send-webhook.nix {inherit pkgs;})

    hydra-cli
    jq
    curl
    bzip2
    postgresql
  ];

  # The fixture creates flake jobsets and exercises Hydra's flake evaluator.
  nix.settings.experimental-features = ["nix-command" "flakes"];

  environment.variables = {
    # Required by `iog-send-webhook` in the test scripts.
    WEBHOOK_SECRET = "secret-token";
  };

  services = {
    hydra = {
      enable = true;
      hydraURL = "http://localhost:3000";
      notificationSender = "admin@iohk.io";
      useSubstitutes = false;
      buildMachinesFiles = [];
    };

    hydra-github-bridge.all = {
      enable = true;
      ghAppId = 12345;
      # Haskell-list syntax; installation id 1 matches the vendored payloads.
      # Unknown owners / other installation ids are ignored (configured-owner
      # filtering) and exercised by the integration batch.
      ghAppInstallIds = "[(\"input-output-hk\", 1)]";
      ghUserAgent = "hydra-github-bridge";
      ghAppKeyFile = ./github-app.key;
      ghSecretFile = pkgs.writeText "gh-secret-file" "secret-token";
      ghTokenFile = pkgs.writeText "gh-token-file" "secret-token";
      hydraHost = "http://localhost:3000";
      # Exercises the public-URL check-link path; same loopback host.
      hydraPublicUrl = "http://localhost:3000";
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
      # Run required SQL migration
      # TODO: It would be better if this were in the application startup logic
      ${pkgs.postgresql}/bin/psql hydra hydra <<EOF
        CREATE TABLE IF NOT EXISTS github_commands (
            id SERIAL PRIMARY KEY,
            command JSONB NOT NULL,
            created TIMESTAMP DEFAULT NOW(),
            processed TIMESTAMP DEFAULT NULL
        );

        CREATE TABLE IF NOT EXISTS github_status (
            id SERIAL PRIMARY KEY,
            owner TEXT NOT NULL,
            repo TEXT NOT NULL,
            headSha TEXT NOT NULL,
            name TEXT NOT NULL,
            UNIQUE (owner, repo, headSha, name)
        );

        CREATE TABLE IF NOT EXISTS github_status_payload (
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
      serviceConfig.ExecStart = let
        mockoonBin = lib.getExe flakePackages.mockoon-cli;
        mockData = ./mock-github-data.json;
      in "${mockoonBin} start --data ${mockData} --port 4010 --repair";
    };

    # We'll want the test to fail if it crashes
    hydra-github-bridge-all.serviceConfig.Restart = lib.mkForce "no";
  };

  # These will fail until Hydra and Mock GitHub are running
  systemd.services.hydra-github-bridge-all.after = lib.mkForce ["mock-github.service"];
}
