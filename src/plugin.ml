open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels

(** Set to true to not call omdb, but return a rating derived from the title *)
let debug = false

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
      right: 60px;
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
      z-index: 0;
      pointer-events: auto;
    }

    .movie.overlay {
      position: absolute;
      border-radius: 0%;
      top: 0%;
      left: 0%;
      height: 100%;
      width: 100%;
      background: rgba(0, 0, 0, var(--transparency));
      pointer-events: auto;
      z-index: 0;
    }
    .movie.overlay:hover {
        background-color: rgba(0,0,0,0); /* remove transparency on hover */
    }
  |}
  in

  Dom.appendChild style (doc##createTextNode (Js.string css));
  Dom.appendChild doc##.head style;
  ()

let rec get_parent: level:int -> Dom_html.element Js.t -> Dom_html.element Js.t = fun  ~level elt ->
  match level with
  | 0 -> elt
  | _ ->
    let parent =
      elt##.parentNode
      |> Js.Opt.to_option
      |> Option.map (fun elt -> Dom_html.CoerceTo.element elt |> Js.Opt.to_option)
      |> Option.join
      |> Option.value ~default:elt
    in
    get_parent ~level:(level - 1) parent

let add_rating_badge ?debug ?(transparent=true) ?z_index ?border_radius ~title ~size ~rating ~rating_elt ~transparency_elt () =
  (* We need a div generator *)
  let add_debug elt = match debug with
    | Some message ->
      elt##setAttribute (Js.string "rateflix") (Js.string message)
    | None -> ()
  in
  let add_title elt =
    elt##setAttribute (Js.string "title") (Js.string title)
  in
  let set_style ~property ~value elt =
    elt##.style##setProperty (Js.string property) (Js.string value)
      Js.Optdef.empty |> ignore
  in
  let set_z_index elt = match z_index with
    | None -> ()
    | Some index ->
      set_style ~property:"z-index" ~value:(string_of_int index) elt
  in
  let set_border_radius elt = match border_radius with
    | None -> ()
    | Some radius ->
      set_style ~property:"border-radius" ~value:(string_of_int radius ^ "px") elt
  in


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
    | None -> "∞"
  in

  let doc = Dom_html.document in

  let () =
    match rating, transparent with
    | Some rating, true ->
      let transparency = Transparency.(calculate default rating) in
      let div = Dom_html.createSpan doc in
      div##.className := (Js.string "movie overlay");
      div##setAttribute (Js.string "imdb-rating") (Js.string rating_text);
      div##.style##setProperty (Js.string "--transparency") (Js.string (Printf.sprintf "%.5f" transparency)) Js.Optdef.empty |> ignore;
      add_debug div;
      add_title div;
      set_z_index div;
      set_border_radius div;
      Dom.appendChild transparency_elt div
    | _ -> ()
  in

  let div = Dom_html.createSpan doc in
  div##.textContent := Js.some (Js.string rating_text);
  div##.className := Js.string class_name;
  add_debug div;
  add_title div;
  set_z_index div;
  let () =
    match rating_elt##.style##.position |> Js.to_string with
    | "" -> rating_elt##.style##.position := Js.string "relative";
    | _ -> ()
  in
  Dom.appendChild rating_elt div


let has_imdb_overlay =
  let query = Js.string "[class^='imdb-rating-overlay']" in
  fun elt ->
    elt##querySelector query
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


let ratelimited = ref false
let get_rating ?year title =
  let* rating = Storage.load_rating ?year title in
  match rating with
  | Some rating ->
    Log.log `Info "Loaded rating from cache: %.1f: %s" rating title;
    Lwt.return (Some rating)
  | None ->
    let* result =
      match !ratelimited with
      | true -> Lwt_result.fail ("Ratelimited", `Ratelimited)
      | false ->
        fetch_imdb_rating ?year title
    in
    match result with
    | Ok (Some rating) ->
      Log.log `Debug "Fetched rating from OMDb: %.1f: %s" rating title;
      let* () = Storage.save_rating ~title ~rating:(Some rating) in
      Lwt.return (Some rating)
    | Ok None ->
      Log.log `Debug "No rating available for: %s" title;
      (* Cache the negative result too *)
      let* () = Storage.save_rating ~title ~rating:None in
      Lwt.return None
    | Error (err, `Retry) ->
        Log.log `Warn "Error fetching rating for %s: %s" title err;
        let* () = Storage.save_rating ~title ~rating:None in
        Lwt.return None
    | Error (err, `RateLimit) ->
      Log.log `Warn "Rate limit reached while fetching rating for %s: %s" title err;
      ratelimited := true;
      Lwt.return None
    | Error (_, `Ratelimited) ->
      Log.log `Info "Request dropped due to ratelimit for %s" title;
      Lwt.return None

let get_rating = match debug with
  | false -> get_rating
  | true -> fun ?year title ->
    let year = Option.map (Printf.sprintf ": Year %d") year |> Option.value ~default:"" in
    Log.log `Info "Get rating for title: '%s'%s" title year;
    Digest.MD5.string title
    |> Digest.MD5.to_hex
    |> String.fold_left ~init:0 ~f:(fun acc c -> acc + Char.code c)
    |> (fun v -> v mod 70)
    |> (fun v -> float v /. 10.0 +. 2.)
    |> Option.some
    |> Lwt.return

let add_score_icon ?debug ?transparent ?z_index ?border_radius ~rating_elt ~transparency_elt ?year ~title ~size () =
  let* rating = get_rating ?year title in
  add_rating_badge ?debug ?transparent ?z_index ?border_radius ~title ~rating ~rating_elt ~transparency_elt ~size ();
  Lwt.return_unit


let debug_e ~debug ~msg elt =
  match debug with
  | false -> elt
  | true -> Console.console##info_2 (Js.string msg) elt; elt

let debug_l ?debug ~msg elts =
  match debug with
  | Some true ->
    List.iter ~f:(fun elt -> Console.console##info_2 (Js.string msg) elt) elts;
    elts
  | _ -> elts

(* What is the relation between a sub-selector and title selector?
   Selector: Find appropiate subtree
   Exclude: If matched, then exclude!
   Title_selector: Where to find the title
   Sub_selector: Where to place the ratings - That should be called rating selector.
   transparency_selector: Where to place transparency (if enabled).
   filter: Filter based on which element. That must be the overall tree!
   parent: How many levels up from selector
*)
let process ?(parent=0) ?transparent ?rating_selector ?title_selector ?transparency_selector ?exclude ?(filter=(fun (_, _) -> true)) ?z_index ?border_radius ?(parse_title=(fun t -> t, None)) ~selector ~title ~size =
  let get_selector = function
    | None -> fun elt -> elt
    | Some selector ->
      let selector = Js.string selector in
      fun elt -> elt##querySelector selector |> Js.Opt.to_option |> Option.value ~default:elt
  in
  let rating_selector = get_selector rating_selector in
  let transparency_selector = get_selector transparency_selector in

  let extract_title =
    match title with
    | `Attribute attr ->
      let attr = Js.string attr in
      fun elt ->
        elt##getAttribute attr
        |> Js.Opt.to_option
        |> Option.map Js.to_string
        |> Option.map String.trim
        |> (function Some "" -> None | x -> x)
    | `Function f -> fun elt ->
      f elt
      |> Option.map String.trim
      |> (function Some "" -> None | x -> x)
  in

  (* Get all elements that matches, or current element if no title selector is set *)
  let title_selector = match title_selector with
    | None -> fun elt -> [ elt ]
    | Some selector ->
      let selector = Js.string selector in
      fun elt ->
        match elt##querySelectorAll selector |> Dom.list_of_nodeList with
        | [] -> [ elt ]
        | elts -> elts
  in
  let rec exclude_f = function
    | `Function f -> fun elt -> f elt
    | `Attribute (attr, values) ->
      let attr = Js.string attr in
      fun elt ->
        elt##getAttribute attr
        |> Js.Opt.to_option
        |> Option.map (fun v -> Js.to_string v)
        |> Option.map (fun v -> List.exists ~f:(String.equal v) values)
        |> Option.value ~default:false
    | `Exists xpath ->
      let xpath = Js.string xpath in
      fun elt ->
        elt##querySelector xpath |> Js.Opt.test
    | `Closest xpath ->
      let xpath = Js.string xpath in
      fun elt ->
        elt##closest xpath |> Js.Opt.test
    | `List excludes ->
      let excludes = List.map ~f:exclude_f excludes in
      fun elt -> List.exists ~f:(fun exclude -> exclude elt) excludes
  in
  let exclude =
    let exclude = Option.value ~default:(`List []) exclude in
    exclude_f exclude
  in

  let get_title elt =
    title_selector elt
    |> List.find_map ~f:(fun elt ->
        extract_title elt
        |> Option.map parse_title
        |> Option.map (fun (title, year) -> String.trim title, year)
        |> function
        | Some (title, _) when String.length title = 0 -> None
        | v -> v
      )
  in

  fun () ->
    Dom_html.document##querySelectorAll (Js.string selector)
    |> Dom.list_of_nodeList
    |> List.map ~f:(get_parent ~level:parent)
    |> List.filter ~f:(fun elt -> exclude elt |> not)
    |> List.filter ~f:(fun elt -> has_imdb_overlay elt |> not)
    |> List.filter_map ~f:(fun elt ->
        get_title elt
        |> Option.map (fun title -> elt, title)
      )
    |> List.filter ~f:filter
    |> List.map ~f:(fun (elt, title) -> (rating_selector elt, transparency_selector elt, title))
    |> Lwt_list.iter_p (fun (rating_elt, transparency_elt, (title, year)) ->
        add_score_icon ~debug:selector ?transparent ?z_index ?border_radius ?year ~title ~rating_elt ~transparency_elt ~size ()
      )

let rec daemon ~f condition () =
  let* () = f () in
  let* () = Lwt_condition.wait condition in
  let* () = Lwt_js.sleep 0.5 in
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
  Transparency.listen ();
  Lwt.ignore_result (start ~add_ratings ())
