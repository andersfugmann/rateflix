open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

let add_score_icon ?transparent ~title ~size elt =
  let* rating = Lib.Plugin.get_rating title in
  Plugin.add_rating_badge ?transparent ~rating ~size elt;
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


let process ?transparent ?title_selector ~selector ~title_extract_f ~size () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun el -> Plugin.has_imdb_overlay el |> not)
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
      add_score_icon ?transparent ~title ~size elt
  )

let process_billboard () =
  process
    ~selector:"[class='billboard-title']"
    ~title_selector:"[class='title-logo']"
    ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "title"))
    ~size:`Large
    ~transparent:false
    ()

let process_details () =
  let* () =
    process
      ~selector:"[class^='previewModal--player_container has-smaller-buttons mini-modal']"
      ~title_selector:".previewModal--boxart"
      ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "alt"))
      ~size:`Medium
      ~transparent:false
      ()
  in
  let* () =
    process
      ~selector:"[class^='previewModal--player_container has-smaller-buttons detail-modal']"
      ~title_selector:".previewModal--boxart"
      ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "alt"))
      ~size:`Large
      ~transparent:false
      ()
  in
  Lwt.return ()

let process_recommendations () =
  process
    ~selector:"[class='titleCard--container']"
    ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "aria-label"))
    ~size:`Regular
    ()

let process_tiles () =
  process
    ~selector:"[class^='title-card']"
    ~title_selector:"[aria-label]"
    ~title_extract_f:(fun elt -> elt##getAttribute (Js.string "aria-label"))
    ~size:`Regular
    ()

let process () =
  let* () = process_billboard () in
  let* () = process_recommendations () in
  let* () = process_details () in
  let* () = process_tiles () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
