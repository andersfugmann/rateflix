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

(* If the element has a parent that has
   data-sonic-id="home-page-rail-continue-watching-experiment-1063", then we may want to strip the first word
   (se | watch)
 *)
let parse_title =
  let top_elements =
    Regexp.regexp "^[A-za-z]+ [0-9]?[0-9]: (.*)$"
  in

  let rec has_parent_element ~f elt =
    match f elt with
    | true -> true
    | false ->
      elt##.parentNode
      |> Js.Opt.to_option
      |> Option.map (fun elt -> match elt##.nodeType with Dom.ELEMENT -> Some elt | _ -> None)
      |> Option.join
      |> function
      | Some parent -> has_parent_element ~f (Js.Unsafe.coerce parent)
      | None -> false
  in

  let is_continue_watching_element =
    let cont_regex = Regexp.regexp ".*continue-watching.*" in
    fun elt ->
      elt##getAttribute (Js.string "data-sonic-id")
      |> Js.Opt.to_option
      |> Option.map Js.to_string
      |> Option.map (fun v -> Regexp.string_match cont_regex v 0)
      |> Option.join
      |> Option.is_some
  in

  fun elt title ->
    let title =
      Regexp.string_match top_elements title 0
      |> Option.map (fun result -> Regexp.matched_group result 1)
      |> Option.join
      |> Option.value ~default:title
      |> String.split_on_char ~sep:'.'
      |> List.hd
    in
    match has_parent_element ~f:is_continue_watching_element elt with
     | true ->
       Log.log `Info "Found continue watching title: %s" title;
       String.split_on_char ~sep:' ' title
       |> List.tl
       |> String.concat ~sep:" "
     | false -> title

let add_score_icon ~title ~class_ elt =
  let doc = Dom_html.document in
  let div = Dom_html.createSpan doc in

  let parsed_title = parse_title elt title in
  Log.log `Info "Lookup title: %s (was %s)" parsed_title title;

  let* rating = Plugin.get_rating parsed_title in
  let rating_text = match rating with
    | Some rating -> Printf.sprintf "%.1f" rating
    | None -> "N/A"
  in
  div##.textContent := Js.some (Js.string rating_text);
  div##.className := Js.string class_;

  let () =
    let parent =
      match Js.Opt.to_option elt##.parentNode with
      | Some p -> (Js.Unsafe.coerce p : Dom_html.element Js.t)
      | None -> elt
    in
    match parent##.style##.position |> Js.to_string with
    | "" -> parent##.style##.position := Js.string "relative";
    | _ -> ()
  in
  Dom.appendChild elt div;
  Lwt.return_unit


let _tap ~f x = f x; x

let has_schedule elt =
  elt##querySelector (Js.string "[data-testid='metadata_schedule']")
  |> Js.Opt.test

let process_elements ~selector ~class_ () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun elt -> has_imdb_overlay elt |> not)
  |> List.filter_map ~f:(fun elt ->
    elt##getAttribute (Js.string "aria-label")
    |> Js.Opt.to_option
    |> Option.map Js.to_string
    |> Option.map (fun title ->
      elt, title)
  )
  |> List.filter ~f:(fun (elt, _title) -> has_schedule elt |> not)
  |> Lwt_list.iter_p (fun (elt, title) ->
    add_score_icon ~title ~class_ elt
  )

(* data-testid="metadata_schedule" *)

let process_tiles =
  process_elements
    ~selector:"[data-sonic-type='show'], [data-sonic-type='video']"
    ~class_:"imdb-rating-overlay prime-tile"

(* Main processing function that runs on every iteration *)
let process () =
  let* () = process_tiles () in
  Lwt.return_unit

let () = Plugin.start_plugin ~init:add_custom_styles ~add_ratings:process ()
