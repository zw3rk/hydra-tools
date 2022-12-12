# uci: a simple github <-> hydra bridge

Hydra is a nix CI system, which notably lacks proper github webhook integration.
It does however have a REST api. We'd ideally want hydra to trigger for new PRs
and updates to PRs, and then report the results back. Hydras design is
fundamentally poll driven. As such the github plugin in hydra uses declarative
jobsets, which are created on a polling interval. This leads to many requests to
github, and can together with the status plugin quickly lead to situations where
the github rate limits are reached.

GitHub webhooks are a event driven solutions. We could extend hydras api to be
able to properly deal with each of github's push events, this however means we'd
have to deal with the ungodly mess of perl code and add to that. We are no
perlmongers, and as such a simple bridge written in a language we are more
familiar with has to suffice.

The idea is to have a proper gh webhook target that translates the github events
into the corresponding hydra api calls.

## User events

- Create a new Pull Request 
  This will trigger a pullrequest with an `action:opened`.  The expected result
  is that we open a corresponding jobset on hydra for the specific project.

- Push a commit to a Pull Request 
  This will trigger a push event, (and should also trigger a pullrequest event
  with `action:synchronize`). The expected result is that hydra starts to 
  re-evaluate the pull request.

- Close a Pull Request 
  This will trigger a pullrequest with an `action:closed`.  The expected result
  is that the jobset is being removed from hydra.

We can also respond to the general push events, and trigger rebuilts on the primary branch, but this could also just be done with hydras generic polling ever few minutes.

## Hacking guide

Use `input-output-hk/devx` to provision your dev env (e.g. in this repo).
```
nix develop github:input-output-hk/devx#ghc8107 --no-write-lock-file --system aarch64-darwin --refresh
```

```
cabal run
```
will build and run the project. 
