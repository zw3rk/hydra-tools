# Waiting for Hydra in GitHub Actions

Hydra evaluations and builds can take hours. A GitHub Actions job that needs
Hydra's results should not sit on a runner polling Hydra until then. The bridge
offers two things instead:

- **The `hydra` check-run.** There is one per commit. It is `in_progress`
  from the start of the evaluation until the evaluation has finished and no
  build of it is unfinished. Then it is `success` if the evaluation had no
  errors and every build succeeded, and `failure` otherwise. A restarted
  build makes it `in_progress` again. It takes the bridge's
  `CHECK_RUN_PREFIX`, as the build check-runs do. It can be made a required
  status check of a branch ruleset, to gate merging on Hydra.
- **The `hydra` environment gate.** A workflow job that declares the
  environment waits, without a runner, until the bridge approves it. That
  happens when the commit's `hydra` check-run succeeds. If it fails, the
  bridge rejects the job:

  ```yaml
  jobs:
    after-hydra:
      environment: hydra   # held until Hydra is done with this commit
      runs-on: ubuntu-latest
      steps:
        - run: ...           # Hydra's results are complete when this runs
  ```

  The job belongs to the workflow's normal `push` or `pull_request` run, so
  its result shows on the commit or pull request as usual.

## How the gate works

This uses GitHub's
[custom deployment protection rules](https://docs.github.com/en/actions/how-tos/deploy/configure-and-manage-deployments/create-custom-protection-rules),
which are available on public repositories.

1. When a job using the environment is ready to run, GitHub sends the App a
   `deployment_protection_rule` webhook. The bridge records the waiting run
   in `github_gate`.
2. The bridge decides which commit the run is waiting on:
   - for a `pull_request` run, it is the PR head that Hydra builds (the
     payload's `sha` is GitHub's merge commit);
   - for anything else, it is the payload's `sha`.
3. The gate worker reviews waiting runs when a gate is recorded, when a
   commit's `hydra` state finishes, and at least every minute:
   - **`hydra` succeeded:** the run is approved, with a link to the Hydra
     evaluation.
   - **`hydra` failed:** the run is rejected and the job fails. After
     restarting the failed builds on Hydra, re-run the job on GitHub.
   - **Still running:** the bridge posts a "waiting for Hydra" status report
     on the run once.
   - **No evaluation of the commit after 24 hours:** the run is rejected.
     For a pull request it is rejected sooner, 30 minutes after the PR's
     jobset has moved on to another commit.
4. The periodic sync lists each repository's waiting runs. This picks up any
   whose webhook was missed.

## One-time setup

### The GitHub App (once, by its owner)

- **Repository permissions:**
  - add *Deployments: Read and write*;
  - add *Actions: Read-only*.
- **Events:** subscribe to *Deployment protection rule*.
- **Installations:** each installation then has to accept the new
  permissions. Until it does, the bridge keeps working with the permissions
  it has, and only the gate is unavailable.

### Each repository that wants the gate

In *Settings → Environments*:

1. Create an environment named `hydra`.
2. Under *Deployment protection rules*, enable the bridge's App.
3. Leave *Deployment branches and tags* unrestricted. Jobs from pull requests
   run on `refs/pull/<n>/merge`, which a branch restriction would exclude.

No bridge configuration is needed. The webhook is honoured for any
environment that has the App's rule enabled. The sync's recovery of missed
webhooks only looks for the `hydra` environment.

### The bridge

The bridge creates its tables, `github_hydra_state` and `github_gate`, at
startup.
