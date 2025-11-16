# Rateflix

This browser plugin adds rating overlay to popular streaming services.

Supported streaming services
* Netflix
* HBO Max
* Prime video

## Requirements
To display rating, you will need an omdb api key.
You can obtain one at https://www.omdbapi.com/

## Build
The plugin is build using js_of_ocaml. You will need to install opam.
Then do
```
$ opam install . --deps-only
$ make plugin

```

## Installation
In chrome/edge, open the plugin and choose 'load unpacked'. Then
navigate to and select the `<workspace>/plugin` folder.
Under the plugin settings, enter your omdb api key and press save.
