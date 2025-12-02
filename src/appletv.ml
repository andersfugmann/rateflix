open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

let process_item =
  Plugin.process
    ~selector:"[slot='item'], [name='item']"
    ~title_selector:"[class$='artwork-component__contents']"
    ~title:(`Attribute "alt")
    ~size:`Regular
    ~z_index:2
    ~transparent:true

let process_item2 =
  Plugin.process
    ~selector:"[slot='item'], [name='item2']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~exclude:(`List
                [`Exists "[data-testid='person-lockup'], [data-testid='how-to-watch-card']";
                 `Closest "[aria-label='Trailers'], [aria-label='Episodes'], [aria-label='Bonus Content']"
                ]
             )
    ~size:`Regular
    ~z_index:2
    ~border_radius:14
    ~transparent:true

let process_tabpanel =
  Plugin.process
    ~selector:"[role='tabpanel']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~size:`Large
    ~transparent:true


let process_main =
  Plugin.process
    ~selector:"[class^='content-logo']"
    ~title_selector:"[alt]"
    ~title:(`Attribute "alt")
    ~size:`Regular
    ~z_index:1
    ~transparent:false


let process () =
  let* () = process_item () in
  let* () = process_item2 () in
  let* () = process_tabpanel () in
  let* () = process_main () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
