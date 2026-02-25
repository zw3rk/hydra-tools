{moduleWithSystem, ...}: {
  flake.nixosModules = {
    hydra-github-bridge = moduleWithSystem (perSystem @ {config}: {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.services.hydra-github-bridge;
    in {
      options.services.hydra-github-bridge = with lib;
        mkOption {
          type = types.attrsOf (types.submodule {
            options = {
              enable =
                mkEnableOption "hydra github bridge"
                // {
                  default = true;
                };

              package = mkOption {
                type = types.package;
                default = perSystem.config.packages.hydra-github-bridge;
                defaultText = "hydra-github-bridge";
                description = "The hydra to github webhook bridge";
              };

              ghAppId = mkOption {
                type = types.int;
                default = 0;
                description = ''
                  The GitHub App ID to sign authentication JWTs with.
                '';
              };

              ghAppInstallIds = mkOption {
                type = types.str;
                default = "[]";
                description = ''
                  The GitHub App installation IDs to authenticate with. (owner, id)
                  Formatted as Haskell list syntax. e.g. [(\"owner\", id)]
                '';
              };

              ghUserAgent = mkOption {
                type = types.str;
                default = null;
                description = ''
                  The user agent to use for authorization with the GitHub API.
                  This must match the app name if you authenticate using a GitHub App token.
                '';
              };

              ghAppKeyFile = mkOption {
                type = types.path;
                default = "";
                description = ''
                  Path to a file containing the GitHub App private key for authorization with GitHub.
                '';
              };

              ghTokenFile = mkOption {
                type = with types; nullOr path;
                default = null;
                description = ''
                  Path to a file containing the GitHub token for authorization with GitHub.
                '';
              };

              ghSecretFile = mkOption {
                type = types.nullOr types.path;
                default = null;
                description = ''
                  The agreed upon secret with GitHub for the Webhook payloads.
                '';
              };

              hydraHost = mkOption {
                type = types.str;
                example = "http://hydra.example.com:8080";
                default = "localhost";
                description = ''
                  The host or URL of hydra.
                '';
              };

              hydraUser = mkOption {
                type = types.str;
                default = "";
                description = ''
                  The user to authenticate as with hydra.
                '';
              };

              hydraPassFile = mkOption {
                type = types.nullOr types.path;
                default = null;
                description = ''
                  A file containing the password to authenticate with against hydra.
                '';
              };

              hydraDb = mkOption {
                type = types.str;
                default = "";
                description = ''
                  Hydra DB host string. Empty means unix socket.
                '';
              };

              checkRunPrefix = mkOption {
                type = types.str;
                default = "ci/hydra-build:";
                description = ''
                  Prefix for GitHub check-run names. Set to empty string to disable.
                '';
              };

              port = mkOption {
                type = types.port;
                default = 8811;
                description = ''
                  The port to listen on for webhooks.
                '';
              };

              environmentFile = mkOption {
                type = types.nullOr types.path;
                default = null;
                description = ''
                  plaintext environment file, containing and `HYDRA_DB_USER`, and `HYDRA_DB_PASS`.
                '';
              };
            };
          });
        };

      config.systemd = lib.mkIf (cfg != {}) {
        services = lib.flip lib.mapAttrs' cfg (
          name: iCfg:
            lib.nameValuePair "hydra-github-bridge-${name}" {
              inherit (iCfg) enable;

              wantedBy = ["hydra-github-bridge.target"];
              after = ["postgresql.service"];
              partOf = ["hydra-github-bridge.target"];

              startLimitIntervalSec = 0;

              serviceConfig =
                {
                  User = config.users.users.hydra.name;
                  Group = config.users.groups.hydra.name;

                  Restart = "always";
                  RestartSec = "10s";

                  LoadCredential =
                    lib.optional (iCfg.ghTokenFile != null) "github-token:${iCfg.ghTokenFile}"
                    ++ lib.optional (iCfg.ghAppKeyFile != null) "github-app-key-file:${iCfg.ghAppKeyFile}"
                    ++ lib.optional (iCfg.hydraPassFile != null) "hydra-pass:${iCfg.hydraPassFile}"
                    ++ lib.optional (iCfg.ghSecretFile != null) "github-secret:${iCfg.ghSecretFile}";

                  StateDirectory = "hydra";
                }
                // lib.optionalAttrs (iCfg.environmentFile != null)
                {EnvironmentFile = builtins.toPath iCfg.environmentFile;};

              environment =
                {
                  HYDRA_HOST = iCfg.hydraHost;
                  HYDRA_DB = iCfg.hydraDb;
                  PORT = toString iCfg.port;
                }
                // lib.optionalAttrs (iCfg.ghUserAgent != "") {
                  GITHUB_USER_AGENT = iCfg.ghUserAgent;
                }
                // lib.optionalAttrs (iCfg.ghAppId != 0) {
                  GITHUB_APP_ID = toString iCfg.ghAppId;
                }
                // lib.optionalAttrs (iCfg.ghAppInstallIds != "[]") {
                  GITHUB_APP_INSTALL_IDS = iCfg.ghAppInstallIds;
                }
                // lib.optionalAttrs (iCfg.hydraUser != "") {
                  HYDRA_USER = iCfg.hydraUser;
                }
                // {
                  CHECK_RUN_PREFIX = iCfg.checkRunPrefix;
                };

              script = ''
                ${lib.optionalString (iCfg.ghTokenFile != null) ''export GITHUB_WEBHOOK_SECRET=$(< "$CREDENTIALS_DIRECTORY"/github-token)''}
                ${lib.optionalString (iCfg.ghAppKeyFile != null) ''export GITHUB_APP_KEY_FILE="$CREDENTIALS_DIRECTORY"/github-app-key-file''}
                ${lib.optionalString (iCfg.hydraPassFile != null) ''export HYDRA_PASS=$(< "$CREDENTIALS_DIRECTORY"/hydra-pass)''}
                ${lib.optionalString (iCfg.ghSecretFile != null) ''export KEY=$(< "$CREDENTIALS_DIRECTORY"/github-secret)''}

                export HYDRA_STATE_DIR="$STATE_DIRECTORY"

                exec ${lib.getExe iCfg.package}
              '';
            }
        );

        targets.hydra-github-bridge.wantedBy = ["hydra-server.service"];
      };
    });

    hydra-attic-bridge = moduleWithSystem (perSystem @ {config}: {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.services.hydra-attic-bridge;
    in {
      options.services.hydra-attic-bridge = with lib; {
        enable = mkEnableOption "github attic bridge";
        package = mkOption {
          type = types.package;
          default = perSystem.config.packages.hydra-attic-bridge;
          defaultText = "hydra-attic-bridge";
          description = "The hydra to attic bridge";
        };
        host = mkOption {
          type = types.str;
          default = "";
          description = ''
            Hydra DB host string. Empty means unix socket.
          '';
        };
        attic = mkOption {
          type = types.str;
          default = "localhost:8080";
          description = ''
            The attic URL to use for the bridge.
          '';
        };
        cache = mkOption {
          type = types.str;
          description = ''
            The attic cache name.
          '';
        };
        workers = mkOption {
          type = types.int;
          default = 1;
          description = ''
            Number of concurrent worker threads handling attic uploads.
          '';
        };
        environmentFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            plaintext environment file, containing and `HYDRA_USER`, `HYDRA_PASS`, and `ATTIC_TOKEN`.
          '';
        };
      };
      config = lib.mkIf cfg.enable {
        systemd.services.hydra-attic-bridge = {
          wantedBy = ["multi-user.target"];
          after = ["postgresql.service"];
          partOf = ["hydra-server.service"]; # implies after (systemd/systemd#13847)

          startLimitIntervalSec = 0;

          serviceConfig =
            {
              ExecStart = "@${cfg.package}/bin/hydra-attic-bridge hydra-attic-bridge";

              User = config.users.users.hydra.name;
              Group = config.users.groups.hydra.name;

              Restart = "always";
              RestartSec = "10s";
            }
            // lib.optionalAttrs (cfg.environmentFile != null)
            {EnvironmentFile = builtins.toPath cfg.environmentFile;};
          environment = {
            ATTIC_HOST = cfg.attic;
            ATTIC_CACHE = cfg.cache;
            HYDRA_HOST = cfg.host;
            ATTIC_WORKERS = toString cfg.workers;
          };
        };
      };
    });
  };
}
