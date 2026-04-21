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

The plugin connects to the rating server at `127.0.0.1:1913` by default, which works for a locally running server.

To change the server address:

1. Click the Rateflix extension icon in your browser toolbar
2. In the **Server Host** field, enter the address (e.g. `127.0.0.1:1913` or `https://rateflix.example.com`)
3. Click **Save**

The field accepts `host:port` (uses `http://`) or a full URL with protocol (`https://host:port`).

### Remote server with HTTPS

Browsers block plain HTTP requests from HTTPS pages (mixed content), so a remote server must be accessed over HTTPS. The simplest approach is to put an nginx reverse proxy in front of the rateflix server:

```nginx
server {
    listen 443 ssl;
    server_name rateflix.example.com;

    ssl_certificate     /etc/letsencrypt/live/rateflix.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/rateflix.example.com/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:1913;
        proxy_set_header Host $host;
    }
}
```

Then configure the plugin with `https://rateflix.example.com`.
