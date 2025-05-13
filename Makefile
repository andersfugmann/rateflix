DIST_DIR=dist
JS_SRC=_build/default/src/main.bc.js
JS_DST=$(DIST_DIR)/main.js
POPUP_JS_SRC=_build/default/src/popup.bc.js
POPUP_JS_DST=$(DIST_DIR)/popup.js
MANIFEST=manifest.json
POPUP_HTML=popup.html


.PHONY: build
build: ## Build the project
build:
	dune build src/main.bc.js src/popup.bc.js

.PHONY: dist
dist: ## Create the installation package directory for Edge
dist: build
	@mkdir -p $(DIST_DIR)
	@cp $(JS_SRC) $(JS_DST)
	@cp $(POPUP_JS_SRC) $(POPUP_JS_DST)
	@cp $(MANIFEST) $(DIST_DIR)/
	@cp $(POPUP_HTML) $(DIST_DIR)/
	@chmod +rw $(JS_DST) $(POPUP_JS_DST)

.PHONY: clean
clean: ## Remove build and package artifacts
clean:
	rm -rf $(DIST_DIR)
	dune clean

.PHONY: help
help: ## Show this help
help:
	@grep -h -E '^[.a-zA-Z_-]+:.*## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
