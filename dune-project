(lang dune 3.17)

(generate_opam_files true)

(package
 (version 0.1)
 (license MIT)
 (authors "Anders Fugmann")
 (maintainers "<anders@fugmann.net>")
 (homepage "https://github.com/andersfugmann/rateflix")
 (bug_reports "https://github.com/andersfugmann/rateflix/issues")
 (source (github andersfugmann/rateflix))
 (name rateflix)
 (synopsis "Rateflix streaming score overlay")
 (description "Rateflix adds IMDb scores to popular streaming titles using OCaml, Lwt, and js_of_ocaml.")
 (depends
   ocaml
   js_of_ocaml
   js_of_ocaml-lwt
   lwt
   ppx_deriving_yojson
   (odoc :with-doc)
 )
 (allow_empty)
)

(package
 (name rateflix-server)
 (version 0.1)
 (license MIT)
 (authors "Anders Fugmann")
 (maintainers "<anders@fugmann.net>")
 (synopsis "IMDB rating lookup server")
 (description "HTTP service for fuzzy movie/title lookup against local IMDB data files.")
 (depends
   ocaml
   base
   bin_prot
   ppx_bin_prot
   cohttp-eio
   eio
   eio_main
   tls-eio
   ca-certs
   mirage-crypto-rng
   decompress
   uutf
   uunf
   uucp
   yojson
   ppx_deriving_yojson
   cmdliner
   spelll
   (ppx_expect :with-test)
   (ppx_inline_test :with-test)
 )
)
