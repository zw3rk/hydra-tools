{moduleWithSystem, ...}: {
  flake.nixosModules = {
    github-hydra-bridge = moduleWithSystem (perSystem @ {config}: {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.services.github-hydra-bridge;
    in {
      options.services.github-hydra-bridge = with lib; {
        enable = mkEnableOption "github hydra bridge";

        package = mkOption {
          type = types.package;
          default = perSystem.config.packages.github-hydra-bridge;
          defaultText = "github-hydra-bridge";
          description = "The github to hydra webhook bridge";
        };

        hydraHost = mkOption {
          type = types.str;
          example = "hydra.ci.iog.io:8080";
          description = ''
            The host (domain or IP address, with optional port) of hydra.
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

        ghSecretFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            The agreed upon secret with GitHub for the Webhook payloads.
          '';
        };

        hydraDb = mkOption {
          type = types.str;
          default = "";
          description = ''
            Hydra DB host string. Empty means unix socket.
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
            plaintext environment file, containing and `HYDRA_DB`, `HYDRA_DB_USER`, and `HYDRA_DB_PASS`.
          '';
        };
      };

      config = lib.mkIf cfg.enable {
        systemd.services.github-hydra-bridge = {
          wantedBy = ["hydra-server.service"];
          after = ["postgresql.service" "hydra-server.service"];
          partOf = ["hydra-server.service"];

          startLimitIntervalSec = 0;

          serviceConfig =
            {
              User = config.users.users.hydra.name;
              Group = config.users.groups.hydra.name;

              Restart = "always";
              RestartSec = "10s";

              StateDirectory = "hydra";
            }
            // lib.optionalAttrs (cfg.hydraPassFile != "" || cfg.ghSecretFile != null)
            {
              LoadCredential =
                lib.optional (cfg.hydraPassFile != null) "hydra-pass:${cfg.hydraPassFile}"
                ++ lib.optional (cfg.ghSecretFile != null) "github-secret:${cfg.ghSecretFile}";
            }
            // lib.optionalAttrs (cfg.environmentFile != null)
            {EnvironmentFile = builtins.toPath cfg.environmentFile;};

          script = ''
            ${lib.optionalString (cfg.hydraPassFile != null) ''export HYDRA_PASS=$(< "$CREDENTIALS_DIRECTORY"/hydra-pass)''}
            ${lib.optionalString (cfg.ghSecretFile != null) ''export KEY=$(< "$CREDENTIALS_DIRECTORY"/github-secret)''}

            export HYDRA_STATE_DIR="$STATE_DIRECTORY"

            exec ${lib.getExe cfg.package}
          '';

          environment =
            {
              HYDRA_DB = cfg.hydraDb;
              HYDRA_HOST = cfg.hydraHost;
              PORT = toString cfg.port;
            }
            // lib.optionalAttrs (cfg.hydraUser != "") {HYDRA_USER = cfg.hydraUser;};
        };
      };
    });

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

              hydraHost = mkOption {
                type = types.str;
                default = "localhost";
                description = ''
                  The host (domain or IP address, with optional port) of hydra.
                '';
              };

              hydraDb = mkOption {
                type = types.str;
                default = "";
                description = ''
                  Hydra DB host string. Empty means unix socket.
                '';
              };

              environmentFile = mkOption {
                type = types.nullOr types.path;
                default = null;
                description = ''
                  plaintext environment file, containing and `HYDRA_USER`, and `HYDRA_PASS`.
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
                    ++ lib.optional (iCfg.ghAppKeyFile != "") "github-app-key-file:${iCfg.ghAppKeyFile}";

                  StateDirectory = "hydra";
                }
                // lib.optionalAttrs (iCfg.environmentFile != null)
                {EnvironmentFile = builtins.toPath iCfg.environmentFile;};

              environment =
                {
                  HYDRA_HOST = iCfg.hydraHost;
                  HYDRA_DB = iCfg.hydraDb;
                }
                // lib.optionalAttrs (iCfg.ghUserAgent != "") {
                  GITHUB_USER_AGENT = iCfg.ghUserAgent;
                }
                // lib.optionalAttrs (iCfg.ghAppId != 0) {
                  GITHUB_APP_ID = toString iCfg.ghAppId;
                }
                // lib.optionalAttrs (iCfg.ghAppInstallIds != []) {
                  GITHUB_APP_INSTALL_IDS = iCfg.ghAppInstallIds;
                };

              script = ''
                ${lib.optionalString (iCfg.ghTokenFile != null) ''export GITHUB_TOKEN=$(< "$CREDENTIALS_DIRECTORY"/github-token)''}
                ${lib.optionalString (iCfg.ghAppKeyFile != null) ''export GITHUB_APP_KEY_FILE="$CREDENTIALS_DIRECTORY"/github-app-key-file''}

                export HYDRA_STATE_DIR="$STATE_DIRECTORY"
                export QUEUE_DIR="$STATE_DIRECTORY/hydra-github-bridge/"${lib.escapeShellArg name}

                mkdir -p "$QUEUE_DIR"

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
