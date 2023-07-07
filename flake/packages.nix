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
            inputMap."https://github.com/input-output-hk/servant-github-webhook" = inputs.servant-github-webhook;
          };
        })
      ];
    })
    .hydra-tools;

  packages = lib.flip withSystem ({pkgs, ...}: {
    hydra-crystal-notify = (pkgs.callPackage ../hydra-crystal-notify {}).hydra-crystal-notify;
  });
in {
  perSystem = {
    system,
    pkgs,
    ...
  }: {
    packages = let
      haskellPkgSet' = haskellPkgSet system;
    in
      packages system
      // {
        github-hydra-bridge = haskellPkgSet'.getComponent "github-hydra-bridge:exe:github-hydra-bridge";
        hydra-github-bridge = haskellPkgSet'.getComponent "hydra-github-bridge:exe:hydra-github-bridge";
        disk-store = haskellPkgSet'.getComponent "disk-store:lib:disk-store";
        ds-queue = haskellPkgSet'.getComponent "ds-queue:lib:ds-queue";
      };
  };

  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    {
      system,
      pkgs,
      ...
    }: let
      jobs =
        packages system
        // (haskellPkgSet system).flake'.hydraJobs;
    in
      jobs
      // {
        required = pkgs.releaseTools.aggregate {
          name = "required";
          constituents = lib.collect lib.isDerivation jobs;
        };
      }
  ));
}
