open Js_of_ocaml
open StdLabels
open Lwt.Syntax

(* Removed unused opens: Js_of_ocaml_lwt, Lwt.Syntax *)

[@@@warning "-39"]

(** Cache entry type: holds a rating (which may be None) and a timestamp *)
type cache_entry = {
  rating : float option;[@warning "-39"]
  timestamp : float; [@warning "-39"]
} [@@deriving yojson]

let cache_entry_of_str str = Yojson.Safe.from_string str |> cache_entry_of_yojson |> Result.get_ok
let cache_entry_to_str entry = cache_entry_to_yojson entry |> Yojson.Safe.to_string

(** Constants for cache management *)
let cache_key_prefix = "imdb_rating_cache_"
let cache_ttl = 60. *. 60. *. 24. *. 7.
let negative_cache_ttl = 60. *. 60.

let gettimeofday () =
  let date = new%js Js.date_now in
  let time = date##getTime in
  Js.to_float time

let save_key key value =
  match Chrome.Storage.local_storage () with
  | Some storage ->
    Chrome.Storage.set storage ~key ~value
  | None ->
    Lwt.return_unit

let load_key key =
  match Chrome.Storage.local_storage () with
  | Some storage ->
    Chrome.Storage.get storage key
  | None ->
    Lwt.return_none

(**
 * Create a storage key for a movie title
 * Normalizes the title by converting to lowercase and replacing spaces with underscores
*)

let make_cache_key ?year title =
  (* Helper to normalize a string by replacing special chars with underscores *)
  let normalize str =
    String.map ~f:(function
      | ' ' | '/' | '\\' | '?' | '&' | ':' -> '_'
      | c -> c
    ) str
  in
  let normalized = title |> String.lowercase_ascii |> normalize in
  let year = match year with
    | Some y -> Printf.sprintf "__%d" y
    | None -> ""
  in
  Printf.sprintf "%s%s:%s" cache_key_prefix normalized year

let save_rating ~title ~rating =
  let key = make_cache_key title in
  let entry = {
    rating;
    timestamp = gettimeofday ()
  } in
  let json_str = cache_entry_to_str entry in
  save_key key json_str

let cache_entry_expired ~now = function
  | { timestamp; rating = None } -> now -. timestamp > negative_cache_ttl
  | { timestamp; rating = Some _ } -> now -. timestamp > cache_ttl

let load_rating ?year title =
  let key = make_cache_key ?year title in
  let* key = load_key key in
  match key with
  | None -> Lwt.return_none
  | Some str ->
    let entry = cache_entry_of_str str in
    match cache_entry_expired ~now:(gettimeofday ()) entry with
    | true -> Lwt.return_none
    | false -> Lwt.return entry.rating

let get_cache_entries () =
  match Chrome.Storage.local_storage () with
  | Some storage ->
    let* keys = Chrome.Storage.get_keys_by_prefix storage ~prefix:cache_key_prefix  in
    let* values =
      Lwt_list.map_s (fun key ->
        let* value = Chrome.Storage.get storage key in
        Lwt.return (key,
                    match Option.map cache_entry_of_str value with
                    | Some value -> Some value
                    | None -> None
                    | exception _ -> None)
      ) keys
    in
    Lwt.return values
  | None -> Lwt.return []

let count_cache_entries () =
  let* cache_entries = get_cache_entries () in
  Lwt.return (List.length cache_entries)

let remove_key key =
  match Chrome.Storage.local_storage () with
  | Some storage ->
    Chrome.Storage.remove storage key
  | None -> Lwt.return_unit

let clear_cache_with_predicate ~f =
  let* cache_entries = get_cache_entries () in
  cache_entries
  |> List.filter_map ~f:(function
    | (k, Some entry) when f entry -> Some k
    | (_, Some _) -> None
    | (k, None) -> Some k
  )
  |> Lwt_list.iter_s remove_key


let clear_expired_cache () =
  let f = cache_entry_expired ~now:(gettimeofday ()) in
  clear_cache_with_predicate ~f

let clear_cache () =
  let f _ = true in
  clear_cache_with_predicate ~f

let () =
  let inner =
    let* count = count_cache_entries () in
    Log.log `Info "Cache entries: %d\n" count;
    Lwt.return_unit
  in
  Lwt.ignore_result inner
