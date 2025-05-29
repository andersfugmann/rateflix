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
let add_custom_styles () =
  let doc = Dom_html.document in
  let style = Dom_html.createStyle doc in

  let css = {|
    .imdb-rating-overlay {
      position: absolute;
      background-color: #F5C518; /* IMDb yellow */
      color: #222;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-weight: bold;
      box-shadow: 0px 2px 8px rgba(0,0,0,0.15);
    }

    /* Prime Video specific positions adjustments */
    .imdb-rating-overlay.prime-tile {
      top: 8px;
      right: 8px;
      width: 16px;
      height: 16px;
      font-size: 9px;
    }

    .imdb-rating-overlay.prime-detail {
      top: 16px;
      right: 16px;
      width: 32px;
      height: 32px;
      font-size: 14px;
    }
  |} in

  Dom.appendChild style (doc##createTextNode (Js.string css));
  Dom.appendChild doc##.head style;
  Lwt.return_unit

let has_imdb_overlay =
  let query = Js.string ".imdb-rating-overlay" in
  fun el ->
    el##querySelector query
    |> Js.Opt.test

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

let add_score_icon ~title ~class_ elt =
  let doc = Dom_html.document in
  let div = Dom_html.createSpan doc in

  let parsed_title, year = parse_title title in

  let* rating = Plugin.get_rating ?year parsed_title in
  let rating_text = match rating with
    | Some rating -> Printf.sprintf "%.1f" rating
    | None -> "N/A"
  in
  Log.log `Info "Add rating for %s: %s" title rating_text;

  div##.textContent := Js.some (Js.string rating_text);
  div##.className := Js.string class_;

  let parent =
    match Js.Opt.to_option elt##.parentNode with
    | Some p -> (Js.Unsafe.coerce p : Dom_html.element Js.t)
    | None -> elt
  in
  let () =
    match parent##.style##.position |> Js.to_string with
    | "" -> parent##.style##.position := Js.string "relative";
    | _ -> ()
  in
  Dom.appendChild elt div;
  Lwt.return_unit

let process_elements ~selector ~class_ () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun elt -> has_imdb_overlay elt |> not)
  |> List.filter_map ~f:(fun elt ->
    elt##querySelector (Js.string "[aria-label]")
    |> Js.Opt.to_option
    |> Option.map (fun elt -> elt##getAttribute (Js.string "aria-label") |> Js.Opt.to_option)
    |> Option.join
    |> Option.map (fun title -> elt, Js.to_string title)
  )
  |> Lwt_list.iter_p (fun (elt, title) ->
    add_score_icon ~title ~class_ elt
  )

let process_tiles =
  process_elements
    ~selector:"[data-testid='card-section']"
    ~class_:"imdb-rating-overlay prime-tile"

let process_carousel =
  process_elements
    ~selector:"[data-testid='super-carousel-card']"
    ~class_:"imdb-rating-overlay prime-detail"


(* Main processing function that runs on every iteration *)
let process () =
  let* () = process_tiles () in
  let* () = process_carousel () in
(*  let* () = process_detail_view () in
    let* () = process_recommendations () in
*)
  Lwt.return_unit

(* Initialize plugin *)
let () = Plugin.start_plugin ~init:add_custom_styles ~add_ratings:process ()



(* HBO:

   'data-sonic-type:show' : Movies and series.
   Get title from first sub-element that contains aria-hidden=true and has innerText



*)
