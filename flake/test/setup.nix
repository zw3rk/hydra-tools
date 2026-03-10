{
  pkgs,
  lib,
  flakePackages,
  ...
}: {
  environment.systemPackages = with pkgs; [
    # GitHub mock server
    flakePackages.mockoon-cli

    postgresql
  ];

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
      ghAppInstallIds = {
        input-output-hk = 1;
        IntersectMBO = 2;
      };
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
        mockData = ../../mock-github-data.json;
      in "${mockoonBin} start --data ${mockData} --port 4010 --repair";
    };

    # We'll want the test to fail if they crash
    hydra-github-bridge-all.serviceConfig.Restart = lib.mkForce "no";
  };

  # These will fail until Hydra and Mock GitHub are running
  systemd.targets.hydra-github-bridge.after = ["mock-github.service"];
}
