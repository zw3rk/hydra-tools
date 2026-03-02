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

              filterJobs = mkOption {
                type = types.bool;
                default = true;
                description = ''
                  When true, only report required/nonrequired jobs and failures
                  to GitHub. When false, report ALL jobs as check-runs.
                '';
              };

              enableSse = mkOption {
                type = types.bool;
                default = true;
                description = ''
                  Enable the SSE (Server-Sent Events) endpoint for real-time
                  build status streaming. Clients can connect to
                  GET /status/:owner/:repo/:sha/events for live updates.
                '';
              };

              ssePort = mkOption {
                type = types.port;
                default = 8812;
                description = ''
                  The port for the SSE status endpoint.
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
                  FILTER_JOBS = lib.boolToString iCfg.filterJobs;
                  SSE_ENABLED = lib.boolToString iCfg.enableSse;
                  SSE_PORT = toString iCfg.ssePort;
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

          # nix is needed for the local store probe (nix path-info --offline).
          path = [ pkgs.nix ];

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

    hydra-web = moduleWithSystem (perSystem @ {config}: {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.services.hydra-web;
    in {
      options.services.hydra-web = with lib; {
        enable = mkEnableOption "hydra web frontend";

        package = mkOption {
          type = types.package;
          default = perSystem.config.packages.hydra-web;
          defaultText = "hydra-web";
          description = "The Haskell hydra web frontend package.";
        };

        listenAddr = mkOption {
          type = types.str;
          default = "127.0.0.1:4000";
          description = ''
            Address and port to listen on (host:port).
          '';
        };

        baseURL = mkOption {
          type = types.str;
          default = "http://localhost:4000";
          description = ''
            Public-facing base URL for generating absolute links.
          '';
        };

        basePath = mkOption {
          type = types.str;
          default = "";
          description = ''
            URL prefix when deployed behind a reverse proxy at a sub-path
            (e.g. "/hydra"). Empty for root deployment.
          '';
        };

        databaseURL = mkOption {
          type = types.str;
          default = "postgres://hydra-web@/hydra?host=/run/postgresql";
          description = ''
            PostgreSQL connection string.
          '';
        };

        hydraBackendURL = mkOption {
          type = types.str;
          default = "http://127.0.0.1:3000";
          description = ''
            Upstream Hydra backend URL for proxied write operations.
          '';
        };

        superAdmins = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            GitHub usernames to bootstrap as super-admins on startup.
          '';
        };

        # Credential files for secrets (loaded via systemd LoadCredential).
        sessionSecretFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            File containing the session signing secret.
          '';
        };

        encryptionKeyFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            File containing the AES-256-GCM encryption key (hex-encoded)
            for encrypting stored GitHub tokens.
          '';
        };

        # GitHub OAuth / App integration.
        github = {
          appID = mkOption {
            type = types.int;
            default = 0;
            description = "GitHub App ID.";
          };

          appKeyFile = mkOption {
            type = types.nullOr types.path;
            default = null;
            description = "Path to GitHub App private key file.";
          };

          clientID = mkOption {
            type = types.str;
            default = "";
            description = "GitHub OAuth client ID.";
          };

          clientSecretFile = mkOption {
            type = types.nullOr types.path;
            default = null;
            description = "File containing the GitHub OAuth client secret.";
          };

          installationIDs = mkOption {
            type = types.attrsOf types.int;
            default = {};
            description = ''
              GitHub App installation IDs per organization.
              Example: { "zw3rk" = 12345; "other-org" = 67890; }
            '';
          };
        };

        environmentFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            Optional environment file with additional secrets.
          '';
        };
      };

      config = lib.mkIf cfg.enable {
        # Create a dedicated system user for the web frontend.
        users.users.hydra-web = {
          isSystemUser = true;
          group = "hydra-web";
          description = "Hydra Web Frontend";
        };
        users.groups.hydra-web = {};

        systemd.services.hydra-web = {
          wantedBy = ["multi-user.target"];
          after = ["postgresql.service"];
          partOf = ["hydra-server.service"];

          startLimitIntervalSec = 0;

          serviceConfig =
            {
              User = "hydra-web";
              Group = "hydra-web";

              Restart = "always";
              RestartSec = "10s";

              # GHC RTS initialisation + DB migration can take a while on
              # cold start, especially when the binary is first loaded
              # from disk.  The default 90 s is too tight.
              TimeoutStartSec = "180s";

              # Static assets are bundled alongside the executable.
              StateDirectory = "hydra";

              LoadCredential =
                lib.optional (cfg.sessionSecretFile != null) "session-secret:${cfg.sessionSecretFile}"
                ++ lib.optional (cfg.encryptionKeyFile != null) "encryption-key:${cfg.encryptionKeyFile}"
                ++ lib.optional (cfg.github.appKeyFile != null) "github-app-key:${cfg.github.appKeyFile}"
                ++ lib.optional (cfg.github.clientSecretFile != null) "github-client-secret:${cfg.github.clientSecretFile}";
            }
            // lib.optionalAttrs (cfg.environmentFile != null)
            {EnvironmentFile = builtins.toPath cfg.environmentFile;};

          environment =
            {
              HYDRA_WEB_LISTEN = cfg.listenAddr;
              HYDRA_WEB_BASE_URL = cfg.baseURL;
              HYDRA_WEB_DATABASE_URL = cfg.databaseURL;
              HYDRA_WEB_HYDRA_BACKEND = cfg.hydraBackendURL;
              HYDRA_WEB_STATIC_DIR = "${cfg.package}/share/hydra-web/static";
              HYDRA_WEB_GITHUB_CLIENT_ID = cfg.github.clientID;
            }
            // lib.optionalAttrs (cfg.basePath != "") {
              HYDRA_WEB_BASE_PATH = cfg.basePath;
            }
            // lib.optionalAttrs (cfg.superAdmins != []) {
              HYDRA_WEB_SUPER_ADMINS = lib.concatStringsSep "," cfg.superAdmins;
            }
            // lib.optionalAttrs (cfg.github.appID != 0) {
              HYDRA_WEB_GITHUB_APP_ID = toString cfg.github.appID;
            }
            // lib.optionalAttrs (cfg.github.installationIDs != {}) {
              HYDRA_WEB_GITHUB_INSTALLATION_IDS =
                lib.concatStringsSep "," (lib.mapAttrsToList (org: id: "${org}=${toString id}") cfg.github.installationIDs);
            };

          script = ''
            ${lib.optionalString (cfg.sessionSecretFile != null) ''export HYDRA_WEB_SESSION_SECRET=$(< "$CREDENTIALS_DIRECTORY"/session-secret)''}
            ${lib.optionalString (cfg.encryptionKeyFile != null) ''export HYDRA_WEB_ENCRYPTION_KEY=$(< "$CREDENTIALS_DIRECTORY"/encryption-key)''}
            ${lib.optionalString (cfg.github.appKeyFile != null) ''export HYDRA_WEB_GITHUB_APP_KEY_FILE="$CREDENTIALS_DIRECTORY"/github-app-key''}
            ${lib.optionalString (cfg.github.clientSecretFile != null) ''export HYDRA_WEB_GITHUB_CLIENT_SECRET=$(< "$CREDENTIALS_DIRECTORY"/github-client-secret)''}

            exec ${lib.getExe cfg.package}
          '';
        };
      };
    });
  };
}
