# Local IOG-scenario batch: VM integration + focused scenario tests.
#
# Separate from `flake/tests.nix` (the existing private smoke check) and
# from the Haskell suites (`sync-source`, `token-refresh`, `public-url`),
# which stay independent and intact. Importing this directory via
# `flake.nix` registers the batches as separate `checks.iog-*-local`
# outputs (x86_64-linux only); run them with
# `nix build .#checks.<system>.iog-integration-local` or
# `make test-iog-integration` / `make test-iog-scenarios`.
{
  imports = [
    # Broad webhook-to-Hydra-to-GitHub workflows end to end.
    ./integration-local.nix

    # Focused build-log edge cases and curated situations.
    ./scenarios-local.nix
  ];
}
