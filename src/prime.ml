open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

(* Seems we can reuse the css *)

(** Add custom CSS for IMDb rating badges on Prime Video *)

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

let add_score_icon ~level ~title ~size elt =
  let parsed_title, year = parse_title title in
  let* rating = Plugin.get_rating ?year parsed_title in
  Log.log `Info "Add rating for %s: %.1f" title (Option.value ~default:0.0 rating);
  Plugin.add_rating_badge ~level ~size ~rating elt;
  Lwt.return_unit

let process_elements ~selector ~size ~level () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun elt -> Plugin.has_imdb_overlay elt |> not)
  |> List.filter_map ~f:(fun elt ->
    elt##querySelector (Js.string "[aria-label]")
    |> Js.Opt.to_option
    |> Option.map (fun elt -> elt##getAttribute (Js.string "aria-label") |> Js.Opt.to_option)
    |> Option.join
    |> Option.map (fun title -> elt, Js.to_string title)
  )
  |> Lwt_list.iter_p (fun (elt, title) ->
    add_score_icon ~level ~title ~size elt
  )

(* To delete, delete the parent (i.e. two steps up) *)
(* Maybe relay this info to process elements? *)
let process_tiles =
  process_elements
    ~level:0
    ~selector:"[data-testid='card']"
    ~size:`Regular


let process_carousel =
  process_elements
    ~selector:"[data-testid='super-carousel-card'], [data-testid='intermission-hero-card']"
    ~size:`Large
    ~level:0

(* Main processing function that runs on every iteration *)

let process () =
  let* () = process_tiles () in
  let* () = process_carousel () in
  Lwt.return_unit

(* Initialize plugin *)
let () = Plugin.start_plugin ~add_ratings:process ()



(* HBO:

   'data-sonic-type:show' : Movies and series.
   Get title from first sub-element that contains aria-hidden=true and has innerText



*)
