PLUGIN_DIR=plugin
PLUGIN_BUILD_DIR=_build/default/plugin_build
SERVER_BIN=_build/default/server/main.exe
DATA_DIR=data
IMDB_BASE_URL=https://datasets.imdbws.com


.PHONY: build
build: ## Build the project
build:
	@dune build

.PHONY: build-release
build-release: ## Build the project (release mode)
build-release:
	@dune build --profile=release

.PHONY: server
server: ## Build the server binary
server:
	@dune build server/main.exe

.PHONY: server-release
server-release: ## Build the server binary (release mode)
server-release:
	@dune build --profile=release server/main.exe

.PHONY: run-server
run-server: server ## Run the server (requires --data-dir argument)
run-server:
	@$(SERVER_BIN) $(ARGS)

DATA_FILES=$(DATA_DIR)/title.basics.tsv.gz $(DATA_DIR)/title.ratings.tsv.gz

$(DATA_DIR)/title.basics.tsv.gz:
	@mkdir -p $(DATA_DIR)
	@curl -# -o $(DATA_DIR)/title.basics.tsv.gz $(IMDB_BASE_URL)/title.basics.tsv.gz

$(DATA_DIR)/title.ratings.tsv.gz:
	@mkdir -p $(DATA_DIR)
	@curl -# -o $(DATA_DIR)/title.ratings.tsv.gz $(IMDB_BASE_URL)/title.ratings.tsv.gz

.PHONY: run
run: server $(DATA_FILES) ## Run the server with default data directory
run:
	@$(SERVER_BIN) --data-dir $(DATA_DIR)

.PHONY: test-server
test-server: ## Run fuzzy search tests against the server
test-server:
	@./test/test_fuzzy.sh

.PHONY: plugin
plugin: ## Create the plugin directory via dune alias
	@rm -f $(PLUGIN_DIR)
	@dune build --profile=release @plugin
	@ln -sf $(PLUGIN_BUILD_DIR) $(PLUGIN_DIR)


.PHONY: dist
dist: plugin ## Backwards-compatible target; use `make plugin`

.PHONY: clean
clean: ## Remove build and package artifacts
clean:
	rm -rf $(PLUGIN_DIR)
	dune clean

.PHONY: help
help: ## Show this help
help:
	@grep -h -E '^[.a-zA-Z_-]+:.*## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
