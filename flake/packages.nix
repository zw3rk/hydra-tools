{
  inputs,
  config,
  lib,
  withSystem,
  ...
}: let
  haskellPkgSet = system:
    (import inputs.nixpkgs {
      inherit system;
      inherit (inputs.haskellNix) config;

      overlays = [
        inputs.haskellNix.overlay
        (final: prev: {
          hydra-tools = final.haskell-nix.project' {
            src = ../.;
            compiler-nix-name = "ghc92";
            inputMap = {
              "https://github.com/input-output-hk/servant-github-webhook" = inputs.servant-github-webhook;
              "https://github.com/cuedo/github-webhooks" = inputs.github-webhooks;
            };
          };
        })
      ];
    })
    .hydra-tools;
in {
  perSystem = {
    system,
    pkgs,
    ...
  }: {
    packages = let
      haskellPkgSet' = haskellPkgSet system;
    in {
      github-hydra-bridge = haskellPkgSet'.getComponent "github-hydra-bridge:exe:github-hydra-bridge";
      hydra-github-bridge = haskellPkgSet'.getComponent "hydra-github-bridge:exe:hydra-github-bridge";
      hydra-attic-bridge = haskellPkgSet'.getComponent "hydra-attic-bridge:exe:hydra-attic-bridge";
    };
  };

  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    {
      system,
      pkgs,
      config,
      ...
    }: let
      jobs = (haskellPkgSet system).flake'.hydraJobs;
      packages = config.packages;
    in
      jobs
      // packages
      // {
        required = pkgs.releaseTools.aggregate {
          name = "required";
          constituents = lib.collect lib.isDerivation (jobs // packages);
        };
      }
  ));
}
