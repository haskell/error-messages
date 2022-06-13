.PHONY: help
help: ## Show this help.
	@echo Haskell error messages.
	@echo
	@echo The following make commands are available for ease of use.
	@echo
	@grep -E '^[a-zA-Z_-]+:.*?\#\# .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?\#\# "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: clean
clean: ## Cleans the built site.
ifneq (,$(wildcard ./bin/site))
	./bin/site clean
else
	@echo Site builder not installed. Please run \`make build install\`.
endif

.PHONY: site-watch
site-watch: ## Runs the site watch, for local development. Defaults to http://localhost:8000
	./bin/site watch

.PHONY: site-build
site-build: ## Builds the website, new entries only
	./bin/site build

.PHONY: site-rebuild
site-rebuild: ## Rebuilds the whole website, useful when changing templates etc.
	./bin/site rebuild

.PHONY: build
build: ## Builds the site builder itself.
	cabal build

.PHONY: build-m1
build-m1: ## Builds the site builder itself. Works around an M1 mac issue.
	cabal build --ghc-option=$(shell pkg-config --cflags libffi)

.PHONY: install
install: ## Installs the site builder to ./bin, for ease of use.
	cp "$(shell cabal list-bin .)" ./bin/


