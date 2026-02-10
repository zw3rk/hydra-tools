{
  inputs,
  config,
  lib,
  withSystem,
  ...
}: {
  perSystem = {
    config,
    pkgs,
    ...
  }: {
    packages.mockoon-cli = pkgs.buildNpmPackage rec {
      pname = "mockoon-cli";
      version = "9.5.0";

      src = pkgs.fetchFromGitHub {
        owner = "mockoon";
        repo = "mockoon";
        rev = "v${version}";
        hash = "sha256-/TGpt6Mc4B7C0DcyYK72KC1hRXlvWi6sgsTp+mNFZXI=";
      };

      npmDeps = pkgs.importNpmLock {
        npmRoot = src;

        # Replace the package-lock, which does not contain all the required entries. To
        # regenerate it from the mockoon source, run:
        #
        #     rm -rf node_modules package-lock.json
        #     npm install --package-lock-only
        packageLock = pkgs.lib.importJSON ./package-lock.json;
      };

      npmConfigHook = pkgs.importNpmLock.npmConfigHook;
      npmRebuildFlags = ["--ignore-scripts"];
      npmBuildScript = "build:cli";
      dontNpmPrune = true;

      prePatch = "cp ${./package-lock.json} package-lock.json";
      preBuild = "npm run build:libs";
      postBuild = "ls packages/cli";

      postInstall = ''
        mkdir -p $out/bin

        # Add required build artifacts to the output
        cp -r \
          packages/commons/dist \
          $out/lib/node_modules/@mockoon/mockoon/node_modules/@mockoon/commons/
        cp -r \
          packages/commons-server/dist \
          $out/lib/node_modules/@mockoon/mockoon/node_modules/@mockoon/commons-server/
        cp -r \
          packages/cli/dist \
          packages/cli/oclif.manifest.json \
          $out/lib/node_modules/@mockoon/mockoon/packages/cli/

        # Add the executable to PATH
        ln -s \
          $out/lib/node_modules/@mockoon/mockoon/node_modules/@mockoon/cli/bin/run.js \
          $out/bin/mockoon-cli
      '';
    };
  };
}
