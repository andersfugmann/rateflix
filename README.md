# Rateflix

A browser extension that adds IMDB rating overlays to popular streaming services.

## Supported streaming services
* Netflix
* HBO Max (Max)
* Prime Video
* Apple TV+
* Disney+

## Architecture

Rateflix has two components:

1. **Browser plugin** — content scripts injected into streaming sites that display IMDB rating badges
2. **Rating server** — an HTTP service that loads IMDB datasets into memory and provides fuzzy title lookup

The plugin queries the server for ratings and caches results in browser storage.

## Installing the plugin

### From a release archive

1. Download `rateflix-plugin-<version>.tar.gz` from the [latest release](https://github.com/andersfugmann/rateflix/releases/latest)
2. Extract the archive: `tar xzf rateflix-plugin-*.tar.gz`
3. Open Chrome/Edge and navigate to `chrome://extensions`
4. Enable **Developer mode** (toggle in the top-right corner)
5. Click **Load unpacked** and select the extracted `rateflix-plugin` directory

### Building from source

```bash
opam install . --deps-only
make plugin
```

Then load the `plugin/` directory as an unpacked extension (see above).

## Installing the server

### From a Debian package

Download `rateflix-server_<version>_amd64.deb` from the [latest release](https://github.com/andersfugmann/rateflix/releases/latest) and install:

```bash
sudo dpkg -i rateflix-server_*.deb
```

The server is managed via systemd and starts automatically. IMDB data is downloaded on first start.

### Building from source

```bash
opam install . --deps-only
make server-release
```

Run with:

```bash
_build/default/server/rateflix_server.exe --data-dir data/
```

On first run, download the IMDB data files:

```bash
make download
```

### Building a Debian package locally

```bash
dpkg-buildpackage -b -uc
```

The `.deb` is written to the parent directory.

## Server options

```
rateflix-server [--data-dir=DIR] [--port=PORT]
```

| Option | Default | Description |
|--------|---------|-------------|
| `--data-dir DIR` | `.` | Directory containing IMDB data files |
| `--port PORT` | `1913` | Port to listen on |

The server listens on all interfaces (IPv4 and IPv6) on the specified port.

## Configuring the server address in the plugin

The plugin connects to the rating server at `rateflix:1913` by default. To change this:

1. Click the Rateflix extension icon in your browser toolbar
2. In the **Server Host** field, enter the address as `host:port` (e.g. `127.0.0.1:1913` or `myserver.local:8080`)
3. Click **Save**

> **Note:** If you change the server host to a different origin, you may need to update `host_permissions` in `manifest.json` to match (e.g. `"http://myserver.local:8080/*"`).
