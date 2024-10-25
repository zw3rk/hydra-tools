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
          wantedBy = ["multi-user.target"];
          after = ["postgresql.service" "hydra-server.service"];
          partOf = ["hydra-server.service"];

          startLimitIntervalSec = 0;

          serviceConfig = {
            User = config.users.users.hydra.name;
            Group = config.users.groups.hydra.name;

            Restart = "always";
            RestartSec = "10s";

            StateDirectory = "hydra";
          } // lib.optionalAttrs (cfg.environmentFile != null)
          { EnvironmentFile = builtins.toPath cfg.environmentFile; };

          script = ''
            export HYDRA_STATE_DIR="$STATE_DIRECTORY"

            exec ${lib.getExe cfg.package}
          '';

          environment =
            {
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

              ghAppInstallId = mkOption {
                type = types.int;
                default = 0;
                description = ''
                  The GitHub App installation ID to authenticate with.
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

      config.systemd.services = lib.flip lib.mapAttrs' cfg (
        name: iCfg:
          lib.nameValuePair "hydra-github-bridge-${name}" {
            inherit (iCfg) enable;

            wantedBy = ["multi-user.target"];
            after = ["postgresql.service"];

            startLimitIntervalSec = 0;

            serviceConfig = {
              User = config.users.users.hydra.name;
              Group = config.users.groups.hydra.name;

              Restart = "always";
              RestartSec = "10s";

              LoadCredential =
                lib.optional (iCfg.ghTokenFile != null) "github-token:${iCfg.ghTokenFile}"
                ++ lib.optional (iCfg.ghAppKeyFile != "") "github-app-key-file:${iCfg.ghAppKeyFile}";

              StateDirectory = "hydra";
            } // lib.optionalAttrs (iCfg.environmentFile != null)
            { EnvironmentFile = builtins.toPath iCfg.environmentFile; };

            script = ''
              ${lib.optionalString (iCfg.ghTokenFile != null) ''export GITHUB_TOKEN=$(< "$CREDENTIALS_DIRECTORY"/github-token)''}
              ${lib.optionalString (iCfg.ghAppKeyFile != null) ''export GITHUB_APP_KEY_FILE="$CREDENTIALS_DIRECTORY"/github-app-key-file''}

              export HYDRA_STATE_DIR="$STATE_DIRECTORY"

              exec ${lib.getExe iCfg.package}
            '';

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
              // lib.optionalAttrs (iCfg.ghAppInstallId != 0) {
                GITHUB_APP_INSTALL_ID = toString iCfg.ghAppInstallId;
              };
          }
      );
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
            partOf = [ "hydra-server.service" ]; # implies after (systemd/systemd#13847)

            startLimitIntervalSec = 0;

            serviceConfig = {
              ExecStart = "@${cfg.package}/bin/github-hydra-bridge github-hydra-bridge";

              User = config.users.users.hydra.name;
              Group = config.users.groups.hydra.name;

              Restart = "always";
              RestartSec = "10s";
            } // lib.optionalAttrs (cfg.environmentFile != null)
            { EnvironmentFile = builtins.toPath cfg.environmentFile; };
        };
      };
    });
  };
}
