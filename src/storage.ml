open StdLabels
open MoreLabels
open Lwt.Syntax

(** In-memory session cache for ratings. Cleared on page reload. *)

[@@@warning "-39"]

type cache_entry = {
  rating : float option;
  timestamp : float;
} [@@deriving yojson]

[@@@warning "+39"]

let cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 256

let cache_ttl = 60. *. 60. *. 24. *. 7.
let negative_cache_ttl = 60. *. 60.
let transparency_key = "transparency"

let gettimeofday () =
  let open Js_of_ocaml in
  let date = new%js Js.date_now in
  Js.to_float date##getTime

let make_cache_key ?year title =
  let normalized =
    title
    |> String.lowercase_ascii
    |> String.map ~f:(function
      | ' ' | '/' | '\\' | '?' | '&' | ':' -> '_'
      | c -> c)
  in
  match year with
  | Some y -> Printf.sprintf "%s__%d" normalized y
  | None -> normalized

let kv_store : (string, string) Hashtbl.t = Hashtbl.create 16

let save_key key value =
  Hashtbl.replace kv_store ~key ~data:value;
  Lwt.return_unit

let load_key key =
  Lwt.return (Hashtbl.find_opt kv_store key)

let save_rating ~title ?year ~rating () =
  let key = make_cache_key ?year title in
  Hashtbl.replace cache ~key ~data:{ rating; timestamp = gettimeofday () };
  Lwt.return_unit

let cache_entry_expired ~now = function
  | { timestamp; rating = None } -> now -. timestamp > negative_cache_ttl
  | { timestamp; rating = Some _ } -> now -. timestamp > cache_ttl

let load_rating ?year title =
  let key = make_cache_key ?year title in
  match Hashtbl.find_opt cache key with
  | None -> Lwt.return_none
  | Some entry ->
    match cache_entry_expired ~now:(gettimeofday ()) entry with
    | true -> Hashtbl.remove cache key; Lwt.return_none
    | false -> Lwt.return entry.rating

let count_cache_entries () =
  Lwt.return (Hashtbl.length cache)

let clear_cache () =
  Hashtbl.clear cache;
  Lwt.return_unit

let clear_expired_cache () =
  let now = gettimeofday () in
  let expired =
    Hashtbl.fold ~f:(fun ~key ~data acc ->
      match cache_entry_expired ~now data with
      | true -> key :: acc
      | false -> acc
    ) cache ~init:[]
  in
  List.iter ~f:(Hashtbl.remove cache) expired;
  Lwt.return_unit

let () =
  let inner =
    let* count = count_cache_entries () in
    Log.log `Info "Cache entries: %d\n" count;
    Lwt.return_unit
  in
  Lwt.ignore_result inner
