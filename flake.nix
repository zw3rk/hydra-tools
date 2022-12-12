{
    description = "hydra-tools: components/replacements to make hydra suck less.";

    inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
    inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    inputs.flake-utils.url = "github:numtide/flake-utils";

    outputs = { self, nixpkgs, flake-utils, haskellNix }: flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
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
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.github-hydra-bridge.flake { };
    in flake // rec {
      # Built by `nix build .`
      packages.default = flake.packages."github-hydra-bridge:exe:github-hydra-bridge";
      packages.hydra-crystal-notify = pkgs.hydra-crystal-notify.hydra-crystal-notify;
      hydraJobs.github-hydra-bridge = packages.default;
      hydraJobs.hydra-crystal-notify = packages.hydra-crystal-notify;
    });
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