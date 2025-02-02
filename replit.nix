{ pkgs }: {
  deps = [
    pkgs.postgresql
    pkgs.cabal-install
    pkgs.zlib
  ];
}
