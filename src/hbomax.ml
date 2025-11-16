open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib


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

let attribute_match ~attrib ~pattern =
  let pattern = Regexp.regexp pattern in
  let attrib = Js.string attrib in
  fun elt ->
    elt##getAttribute attrib
    |> Js.Opt.to_option
    |> Option.map Js.to_string
    |> Option.map (fun v -> Regexp.string_match pattern v 0)
    |> Option.join
    |> Option.is_some

let is_episode = has_parent_element ~f:(attribute_match ~attrib:"data-sonic-id" ~pattern:"episodes")
let is_continue_watching_element = has_parent_element ~f:(attribute_match ~attrib:"data-sonic-id" ~pattern:"continue-watching")

(* If the element has a parent that has
   data-sonic-id="home-page-rail-continue-watching-experiment-1063", then we may want to strip the first word
   (se | watch)
*)
let parse_title =
  let top_elements =
    Regexp.regexp "^[A-za-z]+ [0-9]?[0-9]: (.*)$"
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
    match is_continue_watching_element elt with
     | true ->
       Log.log `Info "Found continue watching title: %s" title;
       String.split_on_char ~sep:' ' title
       |> List.tl
       |> String.concat ~sep:" "
     | false -> title

let add_score_icon ~title ~size elt =
  Log.log `Info "Lookup title: %s" title;
  let* rating = Plugin.get_rating title in
  Plugin.add_rating_badge ~size ~rating elt;
  Lwt.return_unit


let _tap ~f x = f x; x

let has_schedule elt =
  elt##querySelector (Js.string "[data-testid='metadata_schedule']")
  |> Js.Opt.test

let get_title_text elt =
  elt##querySelector (Js.string "p")
  |> Js.Opt.to_option
  |> Option.map (fun elt ->
      elt##.innerText |> Js.to_string
    )

let get_title_attr elt =
  elt##getAttribute (Js.string "aria-label")
  |> Js.Opt.to_option
  |> Option.map Js.to_string
  |> Option.map (fun title -> parse_title elt title)

let get_title elt =
  let title = get_title_text elt in
  let title_attr = get_title_attr elt in
  match is_continue_watching_element elt with
  | true -> title_attr
  | false when Option.is_some title -> title
  | _ -> title_attr

let process_elements ~selector ~size () =
  Dom_html.document##querySelectorAll (Js.string selector)
  |> Dom.list_of_nodeList
  |> List.filter ~f:(fun elt -> Plugin.has_imdb_overlay elt |> not)
  |> List.filter ~f:(fun elt -> has_schedule elt |> not)
  |> List.filter ~f:(fun elt -> is_episode elt |> not)
  |> List.filter_map ~f:(fun elt ->
      let title = get_title elt in
      Option.map (fun title -> elt, title) title
    )
  |> Lwt_list.iter_p (fun (elt, title) ->
    add_score_icon ~title ~size elt
  )

let process_tiles =
  process_elements
    ~selector:"[data-sonic-type='show'], [data-sonic-type='video']"
    ~size:`Regular

(* Main processing function that runs on every iteration *)
let process () =
  let* () = process_tiles () in
  Lwt.return_unit

let () = Plugin.start_plugin ~add_ratings:process ()
