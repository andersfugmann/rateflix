open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

let exclude =
  let parents =
    ["Trailers";
     "Episodes";
     "Bonus Content";
     "Continue Watching";]
  in
  let childs =
    [ "person-lockup";
      "how-to-watch-card";
      "epic-showcase-item";
    ]
  in

  let closest =
    List.map ~f:(Printf.sprintf "[aria-label^='%s']") parents
    |> String.concat ~sep:", "
  in
  let exists =
    List.map ~f:(Printf.sprintf "[data-testid^='%s']") childs
    |> String.concat ~sep:", "
  in
  [ `Closest closest;
    `Exists exists;
    `Page_title "MLS .*"
  ]


let process_item =
  Plugin.process
    ~selector:"[slot='item'], [name='item']"
    ~title_selector:".artwork-component__contents"
    ~title:(`Attribute "alt")
    ~exclude:((`Exists "[data-testid='epic-showcase-item']") :: exclude)
    ~size:`Regular
    ~z_index:2
    ~border_radius:14

let process_item2 =
  Plugin.process
    ~selector:"[slot='item'], [process_item2]"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~exclude:((`Exists "[data-testid='epic-showcase-item']") :: exclude)
    ~size:`Regular
    ~z_index:2
    ~border_radius:14

let process_epic =
  Plugin.process
    ~selector:"[slot='item']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~exclude
    ~size:`Regular
    ~z_index:2
    ~border_radius:20

let process_main =
  Plugin.process
    ~selector:"[class^='content-logo']"
    ~title_selector:"[alt]"
    ~title:(`Attribute "alt")
    ~exclude
    ~size:`Regular
    ~z_index:1
    ~transparent:false


let process () =
  let* () = process_item () in
  let* () = process_item2 () in
  let* () = process_epic () in
  let* () = process_main () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
