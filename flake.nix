{
    description = "hydra-tools: components/replacements to make hydra suck less.";

    inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
    inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    inputs.flake-utils.url = "github:numtide/flake-utils";

    outputs = { self, nixpkgs, flake-utils, haskellNix }: 
    let overlays = [
            haskellNix.overlay
            (final: prev: {
                github-hydra-bridge = final.haskell-nix.project' {
                    src = ./github-hydra-bridge;
                    compiler-nix-name = "ghc8107";
                };
            })
            (final: prev: {
                hydra-crystal-notify = final.callPackage ./hydra-crystal-notify {};
            })
        ];
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.github-hydra-bridge.flake { };
    in flake // rec {
      # Built by `nix build .`
      packages.github-hydra-bridge  = flake.packages."github-hydra-bridge:exe:github-hydra-bridge";
      packages.hydra-crystal-notify = pkgs.hydra-crystal-notify.hydra-crystal-notify;
      hydraJobs.github-hydra-bridge = packages.github-hydra-bridge;
      hydraJobs.hydra-crystal-notify = if system == "x86_64-linux" then packages.hydra-crystal-notify else null;
    }) // {
        nixosModules.github-hydra-bridge = { config, lib, pkgs, ...}: 
        with lib;
        let cfg = config.services.github-hydra-bridge;
        inherit (lib) mkIf mkOption types mkEnableOption concatStringsSep optionals optionalAttrs;
        in {
            options = {
                services.github-hydra-bridge = {
                    enable = mkEnableOption "github hydra bridge";
                    package = mkOption {
                        type = types.package;
                        default = self.packages.${pkgs.system}.github-hydra-bridge;
                        defaultText = "github-hydra-bridge";
                        description = "The github to hydra webhook bridge";
                    };
                    hydraUser = mkOption {
                        type = types.str;
                        default = "";
                        description = ''
                        The user to authenticate as with hydra.
                        '';
                    };
                    hydraPass = mkOption {
                        type = types.str;
                        default = "";
                        description = ''
                        The password to authenticate as with hydra.
                        '';
                    };
                    ghSecret = mkOption {
                        type = types.str;
                        default = "";
                        description = ''
                        The agreed upon secret with GitHub for the Webhook
                        payloards.
                        '';
                    };
                    port = mkOption {
                        type = types.int;
                        default = 8811;
                        description = ''
                        the port to listen on for webhooks.
                        '';
                    };
                };
            };
            config = mkIf cfg.enable {
                systemd.services.github-hydra-bridge = {
                    wantedBy = [ "multi-user.target" ];
                    after = [ "postgresql.service" ];
                    startLimitIntervalSec = 0;

                    script = ''
                        ${cfg.package}/bin/github-hydra-bridge
                    '';

                    serviceConfig = {
                        User = "hydra";
                        Group = "hydra";
                        Restart = "always";
                        RestartSec = "10s";
                    };

                    environment = {
                        KEY = cfg.ghSecret;
                        HYDRA_USER = cfg.hydraUser;
                        HYDRA_PASS = cfg.hydraPass;
                        PORT = "${toString cfg.port}";
                    };
                };
            };            
        };
        nixosModules.hydra-crystal-notify = { config, lib, pkgs, ... }:
        with lib;
        let cfg = config.services.hydra-crystal-notify;
        inherit (lib) mkIf mkOption types mkEnableOption concatStringsSep optionals optionalAttrs;
        in {
            options = {
                services.hydra-crystal-notify = {
                enable = mkEnableOption "hydra crystal notify";

                package = mkOption {
                    type = types.package;
                    default = self.packages.${pkgs.system}.hydra-crystal-notify;
                    defaultText = "hydra-crystal-notify";
                    description = " The hydra crystal notify package to be used";
                };

                logLevel = mkOption {
                    type = types.enum [ "DEBUG" "VERBOSE" "INFO" "WARNING" "ERROR" "FATAL" "NONE" ];
                    default = "INFO";
                    description = ''
                    The log level for the hydra crystal notify service. Valid levels are:
                    DEBUG VERBOSE INFO WARNING ERROR FATAL NONE
                    '';
                };

                # TODO: add a different logging target with the new log module in crystal 0.34
                #logFile = mkOption {
                #  type = types.str;
                #  default = "/var/lib/hydra/notification-debug.log";
                #  description = "The default path an alternate log file if not logging to STDOUT";
                #};

                mockMode = mkOption {
                    type = types.bool;
                    default = false;
                    description = "If set to true, any API calls won't only be logged, but not actually made";
                };

                currentMode = mkOption {
                    type = types.bool;
                    default = true;
                    description = "If set to true, only build notifications with an evaluation marked as `iscurrent` will be processed";
                };

                configFile = mkOption {
                    type = types.str;
                    default = "/var/lib/hydra/github-notify.conf";
                    description = "The default path to the hydra crystal notify config file";
                };

                baseUri = mkOption {
                    type = types.str;
                    default = "https://hydra.iohk.io";
                    description = "The default base URI path for composing hydra link references";
                };

                dbUser = mkOption {
                    type = types.str;
                    default = "";
                    description = ''
                    The default database user to connect to hydra postgres with.
                    NOTE: This works with a blank string when deploying to the hydra
                            server and run as a service with the hydra user.
                    '';
                };

                dbDatabase = mkOption {
                    type = types.str;
                    default = "hydra";
                    description = "The default database to connect to hydra postgres with";
                };

                dbHost = mkOption {
                    type = types.str;
                    default = "/run/postgresql";
                    description = "The default host to connect to hydra postgres with";
                };

                dbRetryDelay = mkOption {
                    type = types.int;
                    default = 2;
                    description = "The default database retry delay in seconds for re-connection attempts";
                };

                dbRetryAttempts = mkOption {
                    type = types.int;
                    default = 15;
                    description = "The default database re-connection attempt number prior to exception";
                };

                notifyUrl = mkOption {
                    type = types.str;
                    default = "DEFAULT";
                    description = ''
                    The default notify URL to use host to connect to hydra postgres with.
                    If "DEFAULT" is used, hydra crystal notify will use its default url.
                    If any string other than "DEFAULT" is provided, that will be directly used
                    as the url.  Note that crystal string interpolation `#{...}` can be provided.

                    Examples are:

                    # Live github status submission url
                    "https://api.github.com/repos/#{m["owner"]}/#{m["repo"]}/statuses/#{rev}"

                    # Test submissions on a non-github test server
                    "http://<HOST>:<PORT>/api.github.com/repos/#{m["owner"]}/#{m["repo"]}/statuses/#{rev}"

                    # Test submissions on github on a throw-away branch with a test commit
                    "https://api.github.com/repos/<OWNER>/<REPO>/statuses/<COMMIT>"
                    '';
                };

                apiPeriod = mkOption {
                    type = types.int;
                    default = 3600;
                    description = ''
                    The API time period in seconds used by github prior to API refresh.
                    This value is used to calculate a damping function that is applied
                    to a time-averaging API rate limit calculation.
                    '';
                };

                notifiedTtl = mkOption {
                    type = types.int;
                    default = 8 * 3600;
                    description = ''
                    The default time period used for maintaining repo-commit key state
                    values in memory before they expire.  Default build expirations are
                    typically set to 8 hours, which is also the default for this parameter.
                    '';
                };

                maintChecks = mkOption {
                    type = types.int;
                    default = 300;
                    description = ''
                    The default time period to perform a maintenance check of the
                    repo-commit key state held in memory and expire any aged key value
                    pairs, followed by logging a status update to the logger.
                    '';
                };

                commitRateLimit = mkOption {
                    type = types.int;
                    default = 10;
                    description = ''
                    The default value to rate limit API notifications to github at on a
                    repo-commit key basis.  Only one API call may happen within this time
                    period per repo-commit.  Final notifications for aggregate or target
                    jobs are exempted from this limit so that status checks will receive
                    a final update successfully.
                    '';
                };
                };
            };

            config = mkIf cfg.enable {
                systemd.services.hydra-crystal-notify = {
                wantedBy = [ "multi-user.target" ];
                after = [ "postgresql.service" ];
                startLimitIntervalSec = 0;

                script = ''
                    ${cfg.package}/bin/hydra-crystal-notify
                '';

                serviceConfig = {
                    User = "hydra";
                    Group = "hydra";
                    Restart = "always";
                    RestartSec = "10s";
                };

                environment = {
                    CRYSTAL_LOG_LEVEL = cfg.logLevel;
                    CRYSTAL_LOG_SOURCES = "*";
                    LOG_FILE = "/var/lib/hydra/notification-debug.log";
                    MOCK_MODE = if cfg.mockMode then "TRUE" else "FALSE";
                    CURRENT_MODE = if cfg.currentMode then "TRUE" else "FALSE";
                    CFG_FILE = cfg.configFile;
                    BASE_URI = cfg.baseUri;
                    DB_USER = cfg.dbUser;
                    DB_DATABASE = cfg.dbDatabase;
                    DB_HOST = cfg.dbHost;
                    DB_RETRY_DELAY = toString cfg.dbRetryDelay;
                    DB_RETRY_ATTEMPTS = toString cfg.dbRetryAttempts;
                    NOTIFY_URL = cfg.notifyUrl;
                    API_PERIOD = toString cfg.apiPeriod;
                    NOTIFIED_TTL = toString cfg.notifiedTtl;
                    MAINT_CHECKS = toString cfg.maintChecks;
                    COMMIT_RATE_LIMIT = toString cfg.commitRateLimit;
                };
                };
            };
        };
    };

    # --- Flake Local Nix Configuration ----------------------------
    nixConfig = {
      extra-substituters = [
        "https://cache.iog.io"
        # We only have zw3rk cache in here, because it provide aarch64-linux and aarch64-darwin.
        "https://cache.zw3rk.com"
      ];
      extra-trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      ];
      # post-build-hook = "./upload-to-cache.sh";
      allow-import-from-derivation = "true";
    };    
}
