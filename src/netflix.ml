open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

let process_billboard () =
  Plugin.process
    ~selector:"[class='billboard-title']"
    ~title_selector:"[class='title-logo']"
    ~title:(`Attribute "title")
    ~size:`Large
    ~transparent:false
    ()

(** Normal card details *)
let process_details =
  Plugin.process
    ~selector:"[class='previewModal--player_container has-smaller-buttons mini-modal previewModal--player-not-playable']"
    ~title_selector:".previewModal--boxart"
    ~title:(`Attribute "alt")
    ~size:`Medium
    ~transparent:false

let process_large_details =
  Plugin.process
    ~selector:"[class='previewModal--player_container has-smaller-buttons detail-modal previewModal--player-not-playable']"
    ~title_selector:"[class='playerModel--player__storyArt has-smaller-buttons detail-modal']"
    ~title:(`Attribute "alt")
    ~size:`Large
    ~transparent:false

let process_tiles =
  Plugin.process
    ~selector:"[class^='title-card']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~size:`Regular

let process_recommendations =
  Plugin.process
    ~selector:"[class='titleCard--container']"
    ~title:(`Attribute "aria-label")
    ~size:`Regular


let process () =
  let* () = process_billboard () in
  let* () = process_details () in
  let* () = process_large_details () in
  let* () = process_tiles () in
  let* () = process_recommendations () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
