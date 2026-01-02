open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels
open Lib

let parse_title =
  let title_prefixes =
    [ "New [^ ]+ Badge";
      "[^ ]+ Finale Badge";
      "Number 1?[0-9]"
    ]
    |> List.map ~f:Regexp.regexp
  in
  let title_postfixes =
    [ "Season [0-9]+ Episode [0-9]+";
      "Select for details on this title";
      "All Episodes";
      "New Episode Every";
      "All Seasons Now Streaming";
      "Rated ";
      "Disney[+] Original";
      "STAR Original";
      "Hulu Original";
      "Mid-Season Finale";
      "Season Finale Badge";
      "Upcoming [0-9]+/[0-9]+ [|]"
    ]
    |> List.map ~f:Regexp.regexp
  in


  let parse title =
    let rec inner title unmatched = function
      | prefix :: xs -> begin
          match Regexp.search prefix title 0 with
          | Some (0, result) ->
            let len = Regexp.matched_string result |> String.length in
            let title' =
              String.sub ~pos:len ~len:(String.length title - len) title
              |> String.trim
            in
            inner title' [] (unmatched @ xs)
          | _ ->
            inner title (prefix :: unmatched) xs
        end
      | [] -> title
    in

    let title = inner title [] title_prefixes in

    let index =
      List.fold_left ~init:None ~f:(fun acc re ->
          match acc, Regexp.search re title 0 with
          | Some n, Some (m, result) when Regexp.matched_string result != "" && m < n -> Some m
          | None, Some (m, result) when Regexp.matched_string result != "" -> Some m
          | _ -> acc
        ) title_postfixes
    in
    match index with
    | None -> title
    | Some len ->
      String.sub ~pos:0 ~len title
  in
  fun title ->
    parse title |> String.trim, None

let process_carousel =
  Plugin.process
    ~selector:"[data-testid='hero-carousel-shelf-item']"
    ~title_selector:"[aria-label]"
    ~title:(`Attribute "aria-label")
    ~size:`Large
    ~parse_title
    ~z_index:2
    ~transparent:false

let process_item =
  Plugin.process
    ~selector:"[data-testid='set-item']"
    ~title_selector:"[alt]"
    ~title:(`Attribute "alt")
    ~size:`Regular
    ~exclude:[`Closest "[data-testid='hero-carousel-shelf-item']";
              `Exists "[data-testid='set-item-rating']"]
    ~z_index:2

let process_item2 =
  Plugin.process
    ~selector:"[data-testid='set-item']"
    ~title:(`Attribute "aria-label")
    ~size:`Regular
    ~exclude:[`Closest "[data-testid='hero-carousel-shelf-item']"]
    ~parse_title
    ~z_index:2

let process_details =
  Plugin.process
    ~selector:"[data-testid='details-title-treatment']"
    ~title_selector:"[alt]"
    ~title:(`Attribute "alt")
    ~size:`Regular
    ~z_index:2
    ~transparent:false


let process () =
  let* () = process_carousel () in
  let* () = process_item () in
  let* () = process_item2 () in
  let* () = process_details () in
  Lwt.return_unit

let () = Lib.Plugin.start_plugin ~add_ratings:process ()
