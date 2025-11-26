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
    ~selector:"[slot='item']"
    ~title_selector:"[class$='artwork-component__contents']"
    ~title:(`Attribute "alt")
    ~size:`Regular
    ~transparent:false

let process_item2 =
  Plugin.process
    ~selector:"[slot='item']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~size:`Regular
    ~transparent:false

let process_tabpanel =
  Plugin.process
    ~selector:"[role='tabpanel']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~size:`Large
    ~transparent:false

let process () =
  let* () = process_item () in
  let* () = process_item2 () in
  let* () = process_tabpanel () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
