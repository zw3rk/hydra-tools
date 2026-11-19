# Revision history for hydra-github-bridge

## Unreleased

* New aggregate `hydra` check-run per commit (with the `CHECK_RUN_PREFIX`).
  It is in progress until the evaluation has finished and none of its builds
  is unfinished, then succeeds when the evaluation had no errors and every
  build succeeded, and fails otherwise.  Its state is kept in the new
  `github_hydra_state` table, and unfinished ones are recomputed every five
  minutes.
* New `hydra` environment gate: GitHub Actions jobs with
  `environment: hydra` wait (without a runner) until the bridge approves
  them once the commit's `hydra` check-run has succeeded, or rejects them
  once it has failed.  The bridge handles the `deployment_protection_rule`
  webhook, records waiting runs in the new `github_gate` table, and recovers
  missed webhooks in the periodic sync.  Needs the App permissions
  Deployments (read and write) and Actions (read), the "Deployment
  protection rule" event, and a `hydra` environment with the App's rule in
  each repository; see `docs/hydra-gate.md`.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
