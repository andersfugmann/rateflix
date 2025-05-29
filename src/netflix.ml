open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels

(** Add custom CSS for rating badges *)
let add_custom_styles () =
  let doc = Dom_html.document in
  let style = Dom_html.createStyle doc in

  let css = {|
    .imdb-rating-overlay.preview-badge {
      width: 32px;
      height: 32px;
      font-size: 16px;
    }

    .imdb-rating-overlay.recommendation-badge {
      width: 24px;
      height: 24px;
      font-size: 12px;
      top: 6px;
      right: 6px;
    }

    .imdb-rating-overlay {
      position: absolute;
      top: 8px;
      right: 8px;
      width: 16px;
      height: 16px;
      background-color: #F5C518;
      color: #222;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-weight: bold;
      font-size: 9px;
      box-shadow: 0px 2px 8px rgba(0,0,0,0.15);
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

let add_score_icon ~title ~class_ elt =
  let doc = Dom_html.document in
  let div = Dom_html.createSpan doc in
  let* rating = Lib.Plugin.get_rating title in
  let rating = match rating with Some rating -> Printf.sprintf "%.1f" rating | None -> "N/A" in
  div##.textContent := Js.some (Js.string rating);
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

let is_game el ~title =
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


let process_netflix ?title_selector ~selector ~title_extract_f ~class_ () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun el -> has_imdb_overlay el |> not)
  |> List.filter_map ~f:(fun elt ->
    (match title_selector with
     | None -> Some elt
     | Some selector -> elt##querySelector (Js.string selector) |> Js.Opt.to_option)
    |> Option.map (fun el ->
      title_extract_f el
      |> Js.Opt.to_option
      |> Option.map Js.to_string
    )
    |> Option.join
    |> Option.map (fun title -> elt, title)
  )
  |> List.filter ~f:(fun (elt, title) -> not (is_game ~title elt))
  |> Lwt_list.iter_p (fun (elt, title) ->
    add_score_icon ~title ~class_ elt
  )

let process_hover_tiles () =
  process_netflix
    ~title_selector:".previewModal--boxart"
    ~selector:".videoMerchPlayer--boxart-wrapper"
    ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "alt"))
    ~class_:"imdb-rating-overlay preview-badge"
    ()


(** Process recommendation tiles and add IMDb rating badges *)
let process_recommendation_tiles () =
  process_netflix
    ~selector:".titleCard--container"
    ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "aria-label"))
    ~class_:"imdb-rating-overlay recommendation-badge"
    ()

let process_tiles () =
  process_netflix
    ~title_selector:".fallback-text"
    ~selector:".title-card"
    ~title_extract_f:(fun el -> el##.textContent)
    ~class_:"imdb-rating-overlay"
    ()

let process () =
  let* () = process_tiles () in
  let* () = process_hover_tiles () in
  let* () = process_recommendation_tiles () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~init:add_custom_styles ~add_ratings:process ()
