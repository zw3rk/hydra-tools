{
  description = "hydra-tools: components/replacements to make hydra suck less.";

  inputs = {
    flake-parts.url = github:hercules-ci/flake-parts;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = github:input-output-hk/haskell.nix;
    treefmt-nix = {
      url = github:numtide/treefmt-nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    servant-github-webhook.url = github:input-output-hk/servant-github-webhook;
    servant-github-webhook.flake = false;
    github-webhooks.url = github:cuedo/github-webhooks;
    github-webhooks.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        flake/formatter.nix
        flake/packages.nix
        flake/nixosModules.nix
      ];

      systems = ["x86_64-linux" "x86_64-darwin"];
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com" # provides aarch64-linux and aarch64-darwin
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}
