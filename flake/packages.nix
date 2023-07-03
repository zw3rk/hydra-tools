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
            hydra-tools = final.haskell-nix.project' {
              src = ../.;
              compiler-nix-name = "ghc92";
              inputMap."https://github.com/input-output-hk/servant-github-webhook" = inputs.servant-github-webhook;
            };
          })
        ];
      };
    in {
      github-hydra-bridge = haskellPkgs.hydra-tools.getComponent "github-hydra-bridge:exe:github-hydra-bridge";
      hydra-github-bridge = haskellPkgs.hydra-tools.getComponent "hydra-github-bridge:exe:hydra-github-bridge";

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
