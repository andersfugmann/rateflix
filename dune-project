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
