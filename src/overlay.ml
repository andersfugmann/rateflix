open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels

(* Cache to hold game titles *)
let game_titles = Hashtbl.create 10
(** Add custom CSS for rating badges *)
let add_custom_styles () =
  let doc = Dom_html.document in
  let style = Dom_html.createStyle doc in

  let css = {|
    .imdb-rating-overlay.preview-rating-badge {
      width: 32px;
      height: 32px;
      font-size: 16px;
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
      z-index: 1000;
      box-shadow: 0px 2px 8px rgba(0,0,0,0.15);
    }

  |} in

  Dom.appendChild style (doc##createTextNode (Js.string css));
  Dom.appendChild doc##.head style

let has_imdb_overlay =
  let query = Js.string (".imdb-rating-overlay") in
  fun el ->
    el##querySelector query
    |> Js.Opt.test

let fetch_imdb_rating =
  let in_progress = Hashtbl.create 19 in
  fun ~title ->
    match Hashtbl.find_opt in_progress title with
    | Some result ->
      begin
        match Lwt.is_sleeping result with
        | false -> Hashtbl.remove in_progress title
        | true -> ()
      end;
      result
      (* If the promise has been fulfilled, then remove it from the table *)
    | None ->
      let result = Lib.Omdb.fetch_imdb_rating ~title in
      Hashtbl.add in_progress ~key:title ~data:result;
      result


(* This function should take a title as argument, and lookup the title in the cache *)
let get_rating title =
  let* rating = Lib.Storage.load_rating title in
  match rating with
  | Some rating ->
    Console.console##info (Printf.sprintf "Loaded rating from cache: %.1f: %s" rating title);
    Lwt.return (Some rating)
  | None ->
    let* result = fetch_imdb_rating ~title in
    match result with
    | Ok (Some rating) ->
      Console.console##info (Printf.sprintf "Fetched rating from OMDb: %.1f: %s" rating title);
      let* () = Lib.Storage.save_rating ~title ~rating:(Some rating) in
      Lwt.return (Some rating)
    | Ok None ->
      Console.console##info (Printf.sprintf "No rating available for: %s" title);
      (* Cache the negative result too *)
      let* () = Lib.Storage.save_rating ~title ~rating:None in
      Lwt.return None
    | Error err ->
        Console.console##warn (Printf.sprintf "Error fetching rating for %s: %s" title err);
        let* () = Lib.Storage.save_rating ~title ~rating:None in
        Lwt.return None

let extract_movie_metadata el =
  let is_game el =
    el##querySelector (Js.string "[class^='mobile-game-title']")
    |> Js.Opt.test
  in

  let get_text_by_selector el selector =
    el##querySelectorAll (Js.string selector)
    |> Dom.list_of_nodeList
    |> List.map ~f:(fun node -> Js.Unsafe.get node "innerText")
    |> List.filter ~f:(function "" -> false | _ -> true)
    |> function [] -> None | x :: _ -> Some x
  in
  let title = get_text_by_selector el ".fallback-text" in

  match is_game el, title  with
  | true, Some title ->
    Hashtbl.add game_titles ~key:title ~data:();
    Some title
  | _, title -> title


let add_score_icon ~title ~class_ elt =
  let doc = Dom_html.document in
  let div = Dom_html.createSpan doc in
  let* rating = get_rating title in
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

let process_hover_tiles () =
  let title_selector = ".previewModal--boxart" in
  let hover_selector = ".videoMerchPlayer--boxart-wrapper" in

  Dom_html.document##querySelectorAll (Js.string hover_selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun el -> has_imdb_overlay el |> not)
  |> List.map ~f:(fun elt ->
    let title =
      elt##querySelectorAll (Js.string title_selector)
      |> Dom.list_of_nodeList
      |> List.map ~f:(fun el -> Js.Unsafe.get el "alt" |> Js.to_string)
      |> List.find_opt ~f:(function "" -> false | _ -> true)
    in
    elt, title
  )
  |> List.filter_map ~f:(function (_, None) -> None | (elt, Some title) -> Some (elt, title))
  |> List.filter ~f:(fun (_, title) -> not (Hashtbl.mem game_titles title))
  |> Lwt_list.iter_p (fun (elt, title) -> add_score_icon ~title ~class_:"imdb-rating-overlay preview-rating-badge" elt)

(* We know that this is not called too often.
   It will only be called by the daemon process *)
let process_tiles () =
  Dom_html.document##querySelectorAll (Js.string ".title-card")
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun el -> has_imdb_overlay el |> not)
  |> List.filter_map ~f:(fun node ->
    match extract_movie_metadata node with
    | Some title -> Some (node, title)
    | None -> None)
  |> List.filter ~f:(fun (_, title) -> not (Hashtbl.mem game_titles title))
  |> Lwt_list.iter_p (fun (node, title) -> add_score_icon ~title ~class_:"imdb-rating-overlay" node)

let rec daemon condition () =
  let* () = process_tiles () in
  let* () = process_hover_tiles () in
  let* () = Lwt_condition.wait condition in
  let* () = Lwt_js.sleep 2.0 in
  daemon condition ()

let observe_dom_changes condition =
  (* This should kick the daemon process *)
  let target = Dom_html.document##.body in
  let node = (target :> Dom.node Js.t) in
  let f _records _observer =
    Lwt_condition.signal condition ()
  in
  MutationObserver.observe ~node ~f
    ~attributes:true ~child_list:true ~subtree:true ~character_data:true
    ()
  |> ignore

let start () =
  let condition = Lwt_condition.create () in
  Lwt.async (daemon condition);
  observe_dom_changes condition;
  Lwt.return_unit

let () =
  add_custom_styles ();
  Lwt.ignore_result (start ())
