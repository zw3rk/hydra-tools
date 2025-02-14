{inputs, ...}: {
  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem = {pkgs, ...}: {
    treefmt = {
      projectRootFile = "flake.nix";

      programs = {
        alejandra.enable = true;
        ormolu.enable = true;
      };
    };
  };
}
