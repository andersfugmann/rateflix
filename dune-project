(lang dune 3.10)

(generate_opam_files true)

(package
 (version 0.1)
 (license MIT)
 (authors "<your-name>")
 (maintainers "<your-email@example.com>")
 (source (github <your-github>/omdb_netflix))
 (name omdb_netflix)
 (synopsis "Netflix IMDb Score Plugin")
 (description "Adds random IMDb scores to Netflix titles using OCaml, Lwt, and js_of_ocaml.")
 (depends
   ocaml
   dune
   js_of_ocaml
   js_of_ocaml-lwt
   lwt
   ppx_deriving_yojson
 )
 (allow_empty)
)
