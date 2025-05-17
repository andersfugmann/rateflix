open Js_of_ocaml
open StdLabels
(* Removed unused opens: Js_of_ocaml_lwt, Lwt.Syntax *)

[@@@warning "-39"]

(** Cache entry type: holds a rating (which may be None) and a timestamp *)
type cache_entry = {
  rating : float option;[@warning "-39"]
  timestamp : float; [@warning "-39"]
} [@@deriving json]

let cache_entry_of_json = Deriving_Json.from_string cache_entry_json
let cache_entry_to_json = Deriving_Json.to_string cache_entry_json

let (let+) = Option.bind
let (let*) = Option.map

(** Constants for cache management *)
let cache_key_prefix = "imdb_rating_cache_"
let cache_ttl = 60. *. 60. *. 24. *. 7.
let negative_cache_ttl = 60. *. 60.

(** Debug logging function to console *)
let log msg =
  Console.console##log (Js.string msg)

(**
 * Create a storage key for a movie title
 * Normalizes the title by converting to lowercase and replacing spaces with underscores
 *)
let make_cache_key (title : string) : string =
  (* Helper to normalize a string by replacing special chars with underscores *)
  let normalize str =
    String.map ~f:(function
      | ' ' | '/' | '\\' | '?' | '&' | ':' -> '_'
      | c -> c
    ) str
  in
  let normalized = title |> String.lowercase_ascii |> normalize in
  cache_key_prefix ^ normalized

let save_rating ~title ~rating =
  let storage = Dom_html.window##.localStorage in
  let key = make_cache_key title in
  let entry = {
    rating;
    timestamp = Unix.gettimeofday ()
  } in
  let json_str = cache_entry_to_json entry in

  Js.Optdef.iter storage (fun storage ->
    storage##setItem (Js.string key) (Js.string json_str);
  )

let load_rating title =
  let key = make_cache_key title in
  let+ storage = Dom_html.window##.localStorage |> Js.Optdef.to_option in
  let+ entry = storage##getItem (Js.string key) |> Js.Opt.to_option in
  let entry = cache_entry_of_json (Js.to_string entry) in
  entry.rating

let cache_entry_expired ~now = function
  | { timestamp; rating = None } -> now -. timestamp > negative_cache_ttl
  | { timestamp; rating = Some _ } -> now -. timestamp > cache_ttl

let get_cache_entries () =
  match Dom_html.window##.localStorage |> Js.Optdef.to_option with
  | Some storage ->
    List.init ~len:(storage##.length) ~f:(fun i -> storage##key i)
    |> List.filter_map ~f:Js.Opt.to_option
    |> List.filter ~f:(fun key -> Js.to_string key |> String.starts_with ~prefix:cache_key_prefix)
    |> List.map ~f:(fun key -> key, Js.Opt.to_option (storage##getItem key))
    (* Convert into cache entries *)
    |> List.map ~f:(function
      | (k, Some v) -> k, Some (cache_entry_of_json (Js.to_string v))
      | (k, None)   -> k, None
    )
  | None -> []

let count_cache_entries () =
  get_cache_entries () |> List.length

let remove_key key =
  match Dom_html.window##.localStorage |> Js.Optdef.to_option with
  | Some storage ->
    storage##removeItem key
  | None -> ()


let clear_cache_with_predicate ~f =
  get_cache_entries ()
  |> List.filter_map ~f:(function
    | (k, Some entry) when f entry -> Some k
    | (_, Some _) -> None
    | (k, None) -> Some k
  )
  (* Remove the keys *)
  |> List.iter ~f:remove_key


let clear_expired_cache () =
  let f = cache_entry_expired ~now:(Unix.gettimeofday ()) in
  clear_cache_with_predicate ~f

let clear_cache () =
  let f _ = true in
  clear_cache_with_predicate ~f

let save_key key value =
  let storage = Dom_html.window##.localStorage in
  Js.Optdef.iter storage (fun storage -> storage##setItem (Js.string key) (Js.string value))

let load_key key =
  let+ storage = Dom_html.window##.localStorage |> Js.Optdef.to_option in
  let+ value = storage##getItem (Js.string key) |> Js.Opt.to_option in
  Some (value |> Js.to_string)

let () =
  let str = Printf.sprintf "Cache entries: %d\n" (count_cache_entries ()) in
  log str
