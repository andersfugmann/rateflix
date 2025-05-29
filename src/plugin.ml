open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels

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

let start ~init ~add_ratings () =
  let condition = Lwt_condition.create () in
  let* () = init () in
  Lwt.async (daemon ~f:add_ratings condition);
  observe_dom_changes condition;
  Lwt.return_unit

let start_plugin ~init ~add_ratings () =
  Lwt.ignore_result (start ~init ~add_ratings ())
