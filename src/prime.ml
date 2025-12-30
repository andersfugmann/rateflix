open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib


let parse_title =
  let title_year_regex =
    (* Match everything in parens - Also things like (4K UHD), which will be dropped *)
    Regexp.regexp "^(.+) \\((.*)\\)$"
  in
  fun title ->
    match Regexp.string_match title_year_regex title 0 with
    | None ->
      title, None
    | Some result ->
      match Regexp.matched_group result 1, Regexp.matched_group result 2 with
      | Some title, Some year ->
        title, int_of_string_opt year
      | _ ->
        title, None


let process_hero_card =
  Plugin.process
    ~selector:"[data-testid$='hero-card']"
    ~rating_selector:"[data-testid='title-art']"
    ~transparency_selector:"[data-testid='title-art']"
    ~title_selector:"[data-testid='title-art']"
    ~title:(`Attribute "aria-label")
    ~parse_title
    ~size:`Medium
    ~transparent:false
    ~z_index:5

let process_carousel =
  Plugin.process
    ~selector:"[data-testid$='carousel-card']"
    ~title:(`Attribute "aria-label")
    ~title_selector:"[aria-label]"
    ~parse_title
    ~size:`Regular
    ~z_index:5

let process_card =
  Plugin.process
    ~selector:"[data-testid='card']"
    ~rating_selector:"[data-testid='packshot']"
    ~transparency_selector:"[data-testid='packshot']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~parse_title
    ~exclude:(`Attribute ("data-card-entity-type", ["EVENT"; "LIVE_EVENT_ITEM"; "VOD_EVENT_ITEM"]))
    ~size:`Regular
    ~transparent:true
    ~z_index:5

let process_detailed_info =
  Plugin.process
    ~selector:"[data-automation-id='hero-background']"
    ~parent:1
    ~rating_selector:"[data-testid='title-art']"
    ~title_selector:"[data-testid='base-image']"
    ~title:(`Attribute "alt")
    ~parse_title
    ~exclude:(`Exists "[aria-label='UPCOMING', [aria-label='REPLAY']")
    ~size:`Large
    ~transparent:false
    ~z_index:5


let process () =
  let* () = process_card () in
  let* () = process_carousel () in
  let* () = process_hero_card () in
  let* () = process_detailed_info () in
  Lwt.return_unit

(* Initialize plugin *)
let () = Plugin.start_plugin ~add_ratings:process ()
