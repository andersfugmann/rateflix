open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

let is_game (el, title) =
  let game_titles = Hashtbl.create 10 in
  let is_mobile_game el =
    el##querySelector (Js.string "[class^='mobile-game-title']")
    |> Js.Opt.test
  in
  match Hashtbl.mem game_titles title with
  | false when is_mobile_game el ->
    Hashtbl.add game_titles ~key:title ~data:();
    true
  | false -> false
  | true -> true

let process = Plugin.process ~filter:(fun e -> is_game e |> not)

let process_billboard () =
  process
    ~selector:"[class='billboard-title']"
    ~title_selector:"[class='title-logo']"
    ~title:(`Attribute "title")
    ~size:`Large
    ~transparent:false
    ()

let process_details1 =
  process
    ~selector:"[class^='previewModal--player_container has-smaller-buttons mini-modal']"
    ~title_selector:".previewModal--boxart"
    ~title:(`Attribute "alt")
    ~size:`Medium
    ~transparent:false

let process_details2 =
  process
    ~selector:"[class^='previewModal--player_container has-smaller-buttons detail-modal']"
    ~title_selector:".previewModal--boxart"
    ~title:(`Attribute "alt")
    ~size:`Large
    ~transparent:false

let process_recommendations =
  process
    ~selector:"[class='titleCard--container']"
    ~title:(`Attribute "aria-label")
    ~size:`Regular

let process_tiles =
  process
    ~selector:"[class^='title-card']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~size:`Regular

let process () =
  let* () = process_billboard () in
  let* () = process_recommendations () in
  let* () = process_details1 () in
  let* () = process_details2 () in
  let* () = process_tiles () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
