.DEFAULT_GOAL := help

# Build targets
hydra-github-bridge: ## Build hydra-github-bridge package (bidirectional GitHub ↔ Hydra bridge)
	cabal build -j hydra-github-bridge

hydra-attic-bridge: ## Build hydra-attic-bridge package
	cabal build -j hydra-attic-bridge

all: hydra-github-bridge hydra-attic-bridge ## Build all packages

clean: ## Clean build artifacts
	cabal clean

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.PHONY: help hydra-github-bridge hydra-attic-bridge all clean
