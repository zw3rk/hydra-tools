# Local IOG-scenario batch

Adapted from upstream `flake/test/{integration,scenarios,setup}.nix` plus
fixtures (`iog/master`), rewired to the current tree. Separate batch on
purpose: the existing `flake/tests.nix` smoke check and the Haskell suites
(`sync-source`, `token-refresh`, `public-url`) stay independent and intact.

## What was adapted (current-tree semantics)

- Multi-instance module: `services.hydra-github-bridge.all` with
  Haskell-list `ghAppInstallIds = "[(\"input-output-hk\", 1)]"`.
- Jobset defaults: `keepnr == 2` (no `hydraKeepEvals` option), flake URIs in
  canonical `github:owner/repo/sha` form (`Lib.Bridge.Source`).
- Webhook delivery: local `iog-send-webhook` (`send-webhook.nix`, openssl +
  jq + curl only, no hand crypto) instead of the upstream package.
- SSE port 8812 is waited on; `hydraPublicUrl` is set so check links go
  through the public-URL path. Configured-owner filtering is exercised by
  the "unauthorized repos are ignored" subtest. Disabled-project SQL,
  not-yet-created mappings, token refresh, Attic queue, and sync
  reconciliation stay covered by the existing Haskell suites and are not
  duplicated here.
- All fixtures/endpoints are private/local only (loopback Hydra on :3000,
  mock GitHub on :4010, `secret-token` secret, test-only App key).

## Run (requires Nix VM builds; NOT run in this session)

```bash
nix build .#checks.x86_64-linux.iog-integration-local
nix build .#checks.x86_64-linux.iog-scenarios-local
```

## Pending RED (not implemented, not claimed green)

Upstream `fix-retries` (#52: `11a0629`, `9bf8629`, `1e33bb3`): per-job eval
failures reported as `ci/eval:<job>` check-runs, and stale failure statuses
cleared ("Evaluation succeeded on retry") when Hydra retries an eval. The
product change is out of scope for this batch; the integration file carries
a PENDING marker where the subtest belongs.
