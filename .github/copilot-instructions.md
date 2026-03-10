# Copilot Instructions for Rateflix

## Build and Test Commands

```bash
# Install dependencies
opam install . --deps-only

# Build everything
make build

# Build browser plugin (release mode, creates plugin/ symlink)
make plugin

# Build server binary
make server

# Run server (downloads IMDB data on first run)
make run

# Run fuzzy search tests against running server
make test-server
# Or run against custom URL:
SERVER_URL=http://localhost:8080 ./test/test_fuzzy.sh
```

## Architecture

This project has two main components:

### Browser Plugin (`src/`)
- OCaml compiled to JavaScript via js_of_ocaml
- Content scripts for each streaming service: `netflix.ml`, `prime.ml`, `hbomax.ml`, `appletv.ml`, `disneyplus.ml`
- Each service script defines CSS selectors to find movie titles and where to place rating badges
- Shared library in `plugin.ml` handles DOM observation, badge rendering, and rating fetching
- Ratings fetched from OMDb API (`omdb.ml`), cached in browser storage (`storage.ml`)
- Uses Lwt for async operations

### Rating Server (`server/`)
- OCaml with Eio for concurrent I/O (not Lwt)
- Loads IMDB TSV datasets into memory, builds trigram index for fuzzy search
- HTTP endpoint `/lookup` accepts JSON array of title queries, returns IMDB ratings
- Multi-domain architecture: worker domains process queries from shared queue
- Supports hot reload via SIGHUP signal

## Key Conventions

### Adding a New Streaming Service
1. Create `src/<service>.ml` following the pattern in `netflix.ml`
2. Use `Plugin.process` with CSS selectors specific to that service's DOM structure
3. Call `Plugin.start_plugin ~add_ratings:process ()` to start the observer
4. Add the new executable to `src/dune`
5. Update `plugin_build/dune` and `manifest.json` with content script entry

### OCaml Style
- Use labeled arguments (`~f:`, `~key:`) extensively
- Open `StdLabels`, `ListLabels`, `MoreLabels` in plugin code for labeled stdlib
- Server code uses Jane Street's `Base` library instead of stdlib
- PPX preprocessors: `js_of_ocaml-ppx` (plugin), `ppx_deriving_yojson` (JSON serialization)

### Plugin Badge Placement
The `Plugin.process` function takes:
- `~selector`: CSS selector to find movie containers
- `~title_selector`: Where to extract the movie title from
- `~title`: How to get title (`` `Attribute "attr"`` or `` `Function f``)
- `~rating_selector`: Where to attach the rating badge
- `~transparency_selector`: Where to attach the transparency overlay
- `~size`: Badge size (`` `Regular``, `` `Medium``, `` `Large``)
