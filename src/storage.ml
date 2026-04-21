open StdLabels
open MoreLabels

(** In-memory session cache for ratings. Cleared on page reload. *)

let cache : (string, float option) Hashtbl.t = Hashtbl.create 256

let transparency_key = "transparency"

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
  Hashtbl.replace cache ~key ~data:rating;
  Lwt.return_unit

let load_rating ?year title =
  let key = make_cache_key ?year title in
  Lwt.return (Hashtbl.find_opt cache key |> Option.join)
