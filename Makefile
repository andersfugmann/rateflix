PLUGIN_DIR=plugin
PLUGIN_BUILD_DIR=_build/default/plugin


.PHONY: build
build: ## Build the project
build:
	@dune build

.PHONY: build-release
build-release: ## Build the project (release mode)
build-release:
	@dune build --profile=release

.PHONY: plugin
plugin: ## Create the plugin directory via dune alias
plugin: icons
	@dune build --profile=release @plugin
	@rm -fr $(PLUGIN_DIR)
	@mkdir -p $(PLUGIN_DIR)
	@cp -a $(PLUGIN_BUILD_DIR)/* $(PLUGIN_DIR)

.PHONY: dist
dist: plugin ## Backwards-compatible target; use `make plugin`

.PHONY: clean
clean: ## Remove build and package artifacts
clean:
	rm -rf $(DIST_DIR)
	dune clean

.PHONY: icons
ICON_SIZES=16 32 48 128
icons: ## Create icons
icons: $(foreach size,$(ICON_SIZES),icons/icon$(size).png)

icons/icon%.png: icons/icon.svg
	convert -background none -scale $*x$* $< $@


.PHONY: help
help: ## Show this help
help:
	@grep -h -E '^[.a-zA-Z_-]+:.*## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
