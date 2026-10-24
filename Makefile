.DEFAULT_GOAL := help

# Build targets
hydra-github-bridge: ## Build hydra-github-bridge package (bidirectional GitHub ↔ Hydra bridge)
	cabal build -j hydra-github-bridge

hydra-attic-bridge: ## Build hydra-attic-bridge package
	cabal build -j hydra-attic-bridge

hydra-web: ## Build hydra-web package (Haskell frontend for Hydra CI)
	cabal build -j hydra-web

all: hydra-github-bridge hydra-attic-bridge hydra-web ## Build all packages

test: ## Run all test suites
	bash hydra-github-bridge/test/run-sync-source.sh cabal test -j all

test-web: ## Run hydra-web test suite
	cabal test -j hydra-web-test

test-sync: ## Test PR source recovery against a private temporary PostgreSQL cluster
	bash hydra-github-bridge/test/run-sync-source.sh cabal test -j1 hydra-github-bridge:sync-source --test-show-details=direct

test-iog-integration: ## Build local IOG integration VM check (x86_64-linux only, needs Nix VM builds)
	nix build .#checks.x86_64-linux.iog-integration-local --show-trace

test-iog-scenarios: ## Build local IOG scenario VM checks (x86_64-linux only, needs Nix VM builds)
	nix build .#checks.x86_64-linux.iog-scenarios-local --show-trace

test-bridge-token-refresh: ## Check installation-token refresh without network or database services
	cabal test hydra-github-bridge:token-refresh

clean: ## Clean build artifacts
	cabal clean

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.PHONY: help hydra-github-bridge hydra-attic-bridge hydra-web all test test-web test-sync test-iog-integration test-iog-scenarios test-bridge-token-refresh clean
