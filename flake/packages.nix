{
  inputs,
  config,
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
              compiler-nix-name = "ghc8107";
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

  flake.hydraJobs = config.flake.packages;
}
