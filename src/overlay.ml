open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
(* Unique class to mark overlays and prevent duplicates *)
let imdb_overlay_class = ".imdb-rating-overlay"

let has_imdb_overlay (el : Dom_html.element Js.t) : bool =
  let res =
    el##querySelector (Js.string imdb_overlay_class)
  in
  Js.Opt.test res

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
      (* let result = Lib.Omdb.fetch_imdb_rating ~title in *)
      let result = Lwt_js.sleep 5.0 >>= fun () -> Lwt_result.return (Random.float 10.0 |> Option.some) in
      Hashtbl.add in_progress ~key:title ~data:result;
      result

(* This function should take a title as argument, and lookup the title in the cache *)
let get_rating title =
  match Lib.Storage.load_rating title with
  | Some rating ->
    Console.console##info (Printf.sprintf "Loaded rating from cache: %.1f: %s" rating title);
    Lwt.return (Some rating)
  | None ->
    let* result = fetch_imdb_rating ~title in
    match result with
    | Ok (Some rating) ->
      Console.console##info (Printf.sprintf "Fetched rating from OMDb: %.1f: %s" rating title);
      Lib.Storage.save_rating ~title ~rating:(Some rating);
      Lwt.return (Some rating)
    | Ok None ->
      Console.console##info (Printf.sprintf "No rating available for: %s" title);
      (* Cache the negative result too *)
      Lib.Storage.save_rating ~title ~rating:None;
      Lwt.return None
    | Error err ->
        Console.console##warn (Printf.sprintf "Error fetching rating for %s: %s" title err);
        Lib.Storage.save_rating ~title ~rating:None;
        Lwt.return None

let extract_movie_metadata el =
  (* Try to extract the title and other available info from the element *)
  let get_text_by_selector selector =
    el##querySelectorAll (Js.string selector)
    |> Dom.list_of_nodeList
    |> List.map ~f:(fun node -> Js.Unsafe.get node "innerText")
    |> List.filter ~f:(function "" -> false | _ -> true)
    |> function [] -> None | x :: _ -> Some x
  in
  let title = get_text_by_selector ".fallback-text, .previewModal--player-titleTreatment-logo, .title-card-container .title-card-title, .title-card .title" in
  Option.value ~default:"<unknown>" title

let add_score_icon ~title elt =
  let doc = Dom_html.document in
  let span = Dom_html.createSpan doc in
  let span = (span :> Dom_html.element Js.t) in
  let* rating = get_rating title in
  let rating = match rating with Some rating -> Printf.sprintf "%.1f" rating | None -> "N/A" in
  span##.textContent := Js.some (Js.string rating);
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

(* We know that this is not called too often.
   It will only be called by the daemon process *)
let process_all_movies () =
  Console.console##info __FUNCTION__;
  Dom_html.document##querySelectorAll (Js.string ".title-card")
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun el -> has_imdb_overlay el |> not)
  |> List.map ~f:(fun node -> node, extract_movie_metadata node)
  |> Lwt_list.iter_p (fun (node, title) -> add_score_icon ~title node)

let rec daemon condition () =
  let* () = process_all_movies () in
  let* () = Lwt.pick [
    (Lwt_condition.wait condition);
    (Lwt_js.sleep 5.0)
  ]
  in
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
