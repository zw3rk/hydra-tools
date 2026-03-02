{
  inputs,
  config,
  lib,
  withSystem,
  ...
}: let
  haskellPkgSet = {
    system,
    config,
    ...
  }:
    (import inputs.nixpkgs {
      inherit system;
      inherit (inputs.haskellNix) config;

      overlays = [
        inputs.haskellNix.overlay

        (final: prev: {
          hydra-tools = final.haskell-nix.project' {
            src = ../../../.;
            compiler-nix-name = "ghc9122";
            inputMap = {
              "https://github.com/input-output-hk/servant-github-webhook" = inputs.servant-github-webhook;
              "https://github.com/cuedo/github-webhooks" = inputs.github-webhooks;
            };

            shell = {
              tools = {
                cabal = "latest";
                haskell-language-server = "2.12.0.0";
                hlint = "3.10";
                weeder = "2.10.0";
              };

              inputsFrom = [config.treefmt.build.devShell];

              buildInputs = [
                (withSystem system ({config, ...}: config.packages.mockoon-cli))
              ];
            };
          };
        })
      ];
    })
    .hydra-tools;
in {
  perSystem = ctx: let
    haskellPkgSet' = haskellPkgSet ctx;
  in {
    inherit (haskellPkgSet'.flake') devShells;

    packages = {
      hydra-github-bridge = haskellPkgSet'.getComponent "hydra-github-bridge:exe:hydra-github-bridge";
      hydra-attic-bridge = haskellPkgSet'.getComponent "hydra-attic-bridge:exe:hydra-attic-bridge";

      # hydra-web needs the static assets (CSS, JS) bundled alongside the
      # executable so the NixOS module can reference them via $out/share/...
      hydra-web = let
        exe = haskellPkgSet'.getComponent "hydra-web:exe:hydra-web";
        pkgs = import inputs.nixpkgs { system = ctx.system; };
      in pkgs.runCommand "hydra-web-with-static" { meta = exe.meta or {}; } ''
        mkdir -p $out/bin $out/share/hydra-web
        ln -s ${exe}/bin/hydra-web $out/bin/hydra-web
        cp -r ${../../../hydra-web/static} $out/share/hydra-web/static
      '';
    };
  };

  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    {pkgs, ...} @ ctx: let
      jobs =
        (haskellPkgSet ctx).flake'.hydraJobs
        // {
          inherit (ctx.config) packages checks devShells;
        };
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
