{
  inputs,
  config,
  lib,
  withSystem,
  ...
}: {
  perSystem = {
    system,
    pkgs,
    ...
  }: {
    packages = let
      haskellPkgs = import inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskellNix) config;

        overlays = [
          inputs.haskellNix.overlay
          (final: prev: {
            github-hydra-bridge = final.haskell-nix.project' {
              src = ../github-hydra-bridge;
              compiler-nix-name = "ghc8107";
            };
            hydra-github-bridge = final.haskell-nix.project' {
              src = ../hydra-github-bridge;
              compiler-nix-name = "ghc927";
            };
          })
        ];
      };
    in {
      github-hydra-bridge = haskellPkgs.github-hydra-bridge.getComponent "github-hydra-bridge:exe:github-hydra-bridge";
      hydra-github-bridge = haskellPkgs.hydra-github-bridge.getComponent "hydra-github-bridge:exe:hydra-github-bridge";

      hydra-crystal-notify = (pkgs.callPackage ../hydra-crystal-notify {}).hydra-crystal-notify;
    };
  };

  flake.hydraJobs =
    builtins.mapAttrs
    (lib.flip withSystem (
      {pkgs, ...}: v:
        v
        // {
          required = pkgs.releaseTools.aggregate {
            name = "required";
            constituents = lib.collect lib.isDerivation v;
          };
        }
    ))
    config.flake.packages;
}
