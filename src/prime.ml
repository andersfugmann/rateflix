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

let add_score_icon ?transparent ~title ~size elt =
  (* We want to add these to the first child element - if there is any *)

  let parsed_title, year = parse_title title in
  let* rating = Plugin.get_rating ?year parsed_title in
  Log.log `Debug "Add rating for %s: %.1f" title (Option.value ~default:0.0 rating);
  Plugin.add_rating_badge ?transparent ~size ~rating elt;
  Lwt.return_unit

let opt_js_str = function
  | Some js -> Js.to_string js
  | None -> "N/A"

let process_elements ?debug ?sub_selector ?transparent ?(title_attribute="aria-label")~selector ~size () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter_map ~f:(fun elt ->
      (* We don't need to go look for this. *)
      elt##querySelector (Js.string (Printf.sprintf "[%s]" title_attribute))
      |> Js.Opt.to_option
      |> (function None -> Some elt | Some elt -> Some elt)
      |> Option.map (fun elt -> elt##getAttribute (Js.string title_attribute) |> Js.Opt.to_option)
      |> Option.join
      |> (fun title ->  Option.iter (fun _ -> Log.log `Info "Found title: %s for %s" (opt_js_str title) selector) debug; title)
      |> Option.map (fun title -> elt, Js.to_string title)
    )
  |> List.map ~f:(fun (elt, title) ->
      let elt =
        match sub_selector with
        | Some selector ->
          elt##querySelector (Js.string selector)
          |> Js.Opt.to_option
          |> Option.value ~default:elt
        | None -> elt
      in
      (elt, title)
    )
  |> List.filter ~f:(fun (elt, _) -> Plugin.has_imdb_overlay elt |> not)
  |> Lwt_list.iter_p (fun (elt, title) ->
      let () =
        match debug with
        | Some true -> Log.log `Info "Element name: '%s'" (elt##.nodeName |> Js.to_string);
        | _ -> ()
      in
      let elt =
        match elt##.nodeName |> Js.to_string with
        | "A" ->
          elt##.parentNode
          |> Js.Opt.to_option
          |> Option.map (fun node -> Dom_html.CoerceTo.element node |> Js.Opt.to_option)
          |> Option.join
          |> Option.value ~default:elt
        | _ -> elt
      in
      add_score_icon ?transparent ~title ~size elt
    )


(* as a child to packshot with no transparency *)
(* as as a child to card with transparency *)
let process_tiles () =
  let* () =
    process_elements
      ~selector:"[data-testid='card']"
      ~size:`Regular
      ~transparent:true
      ()
  in
  let* () =
    process_elements
      ~sub_selector:"[data-testid='packshot']"
      ~selector:"[data-testid='card']"
      ~size:`Regular
      ~transparent:false
      ()
  in
  Lwt.return ()


(* The query selector '$=' means endswith *)
let process_carousel =
  process_elements
    ~selector:"[data-testid$='carousel-card']"
    ~size:`Large


let process_hero_card =
  process_elements
    ~selector:"[data-testid='title-art']"
    ~title_attribute:"alt"
    ~sub_selector:"div"
    ~size:`Regular
    ~transparent:false

(* Main processing function that runs on every iteration *)

let process () =
  let* () = process_tiles () in
  let* () = process_carousel () in
  let* () = process_hero_card () in
  Lwt.return_unit

(* Initialize plugin *)
let () = Plugin.start_plugin ~add_ratings:process ()
