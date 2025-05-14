open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! StdLabels
open! ListLabels
(* Unique class to mark overlays and prevent duplicates *)
let imdb_overlay_class = "imdb-rating-overlay"

let has_imdb_overlay (el : Dom_html.element Js.t) : bool =
  let res =
    Js.Unsafe.meth_call el "querySelector"
      [|Js.Unsafe.inject (Js.string ("." ^ imdb_overlay_class))|]
    |> Js.Unsafe.coerce
  in
  Js.to_bool res

let get_score () =
  Lwt.return (Random.float 10.0)

let extract_movie_metadata (el : Dom_html.element Js.t) : (string * string) =

  (* Try to extract the title and other available info from the element *)
  let get_text_by_selector selector =
    el##querySelectorAll (Js.string selector)
    |> Dom.list_of_nodeList
    |> List.map ~f:(fun node -> Js.Unsafe.get node "innerText")
    |> List.filter ~f:(function "" -> false | _ -> true)
    |> function [] -> None | x :: _ -> Some x
  in
  let title = get_text_by_selector ".fallback-text, .previewModal--player-titleTreatment-logo, .title-card-container .title-card-title, .title-card .title" in
  let year = get_text_by_selector ".year, .title-info-metadata-item-year" in

  (Option.value ~default:"<unknown>" title, Option.value ~default:"<unknown>" year)

let add_score_icon elt =
  Console.console##info __FUNCTION__;
  let doc = Dom_html.document in
  let span = Dom_html.createSpan doc in
  let span = (span :> Dom_html.element Js.t) in
  let* score = get_score () in
  span##.textContent := Js.some (Js.string (Printf.sprintf "%.1f" score));
  span##.className := Js.string imdb_overlay_class;
  span##.style##.position := Js.string "absolute";
  span##.style##.top := Js.string "8px";
  span##.style##.right := Js.string "8px";
  span##.style##.width := Js.string "16px";
  span##.style##.height := Js.string "16px";
  span##.style##.backgroundColor := Js.string "#FFD600";
  span##.style##.color := Js.string "#222";
  span##.style##.borderRadius := Js.string "50%";
  span##.style##.display := Js.string "flex";
  let _ = span##.style##setProperty (Js.string "align-items") (Js.string "center") Js.Optdef.empty in
  let _ = span##.style##setProperty (Js.string "justify-content") (Js.string "center") Js.Optdef.empty in
  span##.style##.fontWeight := Js.string "bold";
  span##.style##.fontSize := Js.string "8px";
  span##.style##.zIndex := Js.string "1000";
  let _ = span##.style##setProperty (Js.string "box-shadow") (Js.string "0 2px 8px rgba(0,0,0,0.15)") Js.Optdef.empty in
  let parent =
    match Js.Opt.to_option elt##.parentNode with
    | Some p -> (Js.Unsafe.coerce p : Dom_html.element Js.t)
    | None -> elt
  in
  if parent##.style##.position = Js.string "" then
    parent##.style##.position := Js.string "relative";
  Dom.appendChild elt span;
  Lwt.return_unit

let process_movie_el (el : Dom_html.element Js.t) =
  match has_imdb_overlay el with
  | true -> Lwt.return_unit
  | false ->
    add_score_icon el

(* We know that this is not called too often.
   It will only be called by the daemon process *)
let process_all_movies () =
  Console.console##info __FUNCTION__;
  let nodes =
    Dom_html.document##querySelectorAll (Js.string ".title-card")
    |> Dom.list_of_nodeList
    |> List.filter ~f:(fun el -> has_imdb_overlay el |> not)
  in
  List.map ~f:extract_movie_metadata nodes
  |> List.iter ~f:(fun (title, year) ->
    let s = Printf.sprintf "%s (%s)" title year in
    Console.console##info (Printf.sprintf "Found movie: %s" s);
  );
  Lwt_list.iter_p process_movie_el nodes

let rec daemon condition () =
  let* () = process_all_movies () in
  let* () = Lwt_condition.wait condition in
  daemon condition ()

let observe_dom_changes condition =
  (* This should kick the daemon process *)
  Console.console##info __FUNCTION__;
  let target = Dom_html.document##.body in
  let node = (target :> Dom.node Js.t) in
  let f _records _observer =
    Console.console##info "Dom changes: Wakeup daemon. ";
    Lwt_condition.signal condition ()
  in
  MutationObserver.observe ~node ~f
    ~attributes:true ~child_list:true ~character_data:true
    ()
  |> ignore

let start () =
  Console.console##info "Starting plugin";
  let condition = Lwt_condition.create () in
  Lwt.async (daemon condition);
  observe_dom_changes condition;
  Lwt.return_unit

let () =
  Lwt.ignore_result (start ())
