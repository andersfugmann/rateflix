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
    .imdb-rating-overlay.large-badge {
      width: 32px;
      height: 32px;
      font-size: 16px;
      top: 16px;
      right: 16px;
    }

    .imdb-rating-overlay.medium-badge {
      width: 24px;
      height: 24px;
      font-size: 12px;
      top: 12px;
      right: 12px;
    }

    .imdb-rating-overlay.regular-badge {
      top: 6px;
      right: 20px;
      width: 20px;
      height: 20px;
      font-size: 10px;
    }

    .imdb-rating-overlay {
      position: absolute;
      background-color: #F5C518;
      color: #222;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-weight: bold;
      box-shadow: 0px 2px 8px rgba(0,0,0,0.15);
    }

  |} in

  Dom.appendChild style (doc##createTextNode (Js.string css));
  Dom.appendChild doc##.head style;
  ()

let add_rating_badge ~size ~rating elt =
  let class_name =
    let size = match size with
      | `Regular -> "regular"
      | `Medium -> "medium"
      | `Large -> "large"
    in
    Printf.sprintf "imdb-rating-overlay %s-badge" size
  in
  let rating_text = match rating with
    | Some rating -> Printf.sprintf "%.1f" rating
    | None -> "--"
  in
  let doc = Dom_html.document in
  let div = Dom_html.createSpan doc in
  div##.textContent := Js.some (Js.string rating_text);
  div##.className := Js.string class_name;
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
  ()


let has_imdb_overlay =
  let query = Js.string ".imdb-rating-overlay" in
  fun (el : Dom_html.element Js.t) ->
    el##querySelector query
    |> Js.Opt.test

let fetch_imdb_rating =
  let in_progress = Hashtbl.create 19 in
  fun ?year title ->
    match Hashtbl.find_opt in_progress (title, year) with
    | Some result ->
      begin
        match Lwt.is_sleeping result with
        | false -> Hashtbl.remove in_progress (title, year)
        | true -> ()
      end;
      result
      (* If the promise has been fulfilled, then remove it from the table *)
    | None ->
      let result = Omdb.fetch_imdb_rating ?year title in
      Hashtbl.add in_progress ~key:(title, year) ~data:result;
      result


let get_rating ?year title =
  let* rating = Storage.load_rating ?year title in
  match rating with
  | Some rating ->
    Log.log `Info "Loaded rating from cache: %.1f: %s" rating title;
    Lwt.return (Some rating)
  | None ->
    let* result = fetch_imdb_rating ?year title in
    match result with
    | Ok (Some rating) ->
      Log.log `Info "Fetched rating from OMDb: %.1f: %s" rating title;
      let* () = Storage.save_rating ~title ~rating:(Some rating) in
      Lwt.return (Some rating)
    | Ok None ->
      Log.log `Info "No rating available for: %s" title;
      (* Cache the negative result too *)
      let* () = Storage.save_rating ~title ~rating:None in
      Lwt.return None
    | Error err ->
        Log.log `Warn "Error fetching rating for %s: %s" title err;
        let* () = Storage.save_rating ~title ~rating:None in
        Lwt.return None


let rec daemon ~f condition () =
  let* () = f () in
  let* () = Lwt_condition.wait condition in
  let* () = Lwt_js.sleep 1.0 in
  daemon ~f condition ()

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

let start ~add_ratings () =
  add_custom_styles ();
  let condition = Lwt_condition.create () in
  Lwt.async (daemon ~f:add_ratings condition);
  observe_dom_changes condition;
  Lwt.return_unit

let start_plugin ~add_ratings () =
  Lwt.ignore_result (start ~add_ratings ())
