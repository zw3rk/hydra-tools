# Hydra Tools

This is a collection of tools for the [hydra ci](https://github.com/nixos/hydra), to improve GitHub fideity.

## Hydra GitHub Bridge

This is a program that listens on GitHub Webhooks, and calls the hydra API, effectivly translating GitHub
WebHook events into hydra jobsets. This removes the need for convoluted declarative jobset definitions in
hydra, and especially the annoying polling setup, which eats fast into github rate limits.

## Hydra Crystal Notifier

This is a replacement for Hydras GitHub notification plugin.  It listens directly on the database events
and pushes statuses to github commits as the events happen.