PACKAGE_DIR=dist
JS_SRC=_build/default/src/main.bc.js
JS_DST=$(PACKAGE_DIR)/main.js
MANIFEST=manifest.json

.PHONY: all
all: ## Build and package the extension for Edge
	build package

.PHONY: build
build: ## Build the project
build:
	dune build src/main.bc.js

.PHONY: package
package: ## Create the installation package directory for Edge
package: build
	@mkdir -p $(PACKAGE_DIR)
	cp $(JS_SRC) $(JS_DST)
	cp $(MANIFEST) $(PACKAGE_DIR)/

.PHONY: clean
clean: ## Remove build and package artifacts
clean:
	rm -rf $(PACKAGE_DIR)
	dune clean

.PHONY: help
help: ## Show this help
help:
	@grep -h -E '^[.a-zA-Z_-]+:.*## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
