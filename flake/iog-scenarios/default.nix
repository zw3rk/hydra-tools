# Local IOG-scenario batch: VM integration + focused scenario tests.
#
# Separate from `flake/tests.nix` (the existing private smoke check) and
# from the Haskell suites (`sync-source`, `token-refresh`, `public-url`),
# which stay independent and intact. Nothing here is built or run by
# default; wire-up is opt-in via the flake import.
{
  imports = [
    # Broad webhook-to-Hydra-to-GitHub workflows end to end.
    ./integration-local.nix

    # Focused build-log edge cases and curated situations.
    ./scenarios-local.nix
  ];
}
