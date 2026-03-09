open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax

[@@@warning "-39"]

(** Type representing a single search result from the rateflix server *)
type search_result = {
  title: string;
  year: int option [@default None];
  imdb_rating: float;
  imdb_id: string;
  match_score: float;
} [@@deriving yojson { strict = false }]

(** Server response is a list of (query, result) pairs *)
type query_json = {
  q_title: string [@key "title"];
  q_year: int option [@key "year"] [@default None];
} [@@deriving yojson]

type response_entry = query_json * search_result [@@deriving yojson]
type response = response_entry list [@@deriving yojson]

[@@@warning "+39"]

let server_url = "http://127.0.0.1:8080"

(** Parse IMDb rating string to float option *)
let parse_rating rating = if Float.equal rating 0.0 then None else Some rating

let fetch_imdb_rating ?year title : (float option, _) Lwt_result.t =
  let query =
    let base = Printf.sprintf {|{"title": "%s"|} title in
    match year with
    | Some y -> Printf.sprintf "%s, \"year\": %d}" base y
    | None -> base ^ "}"
  in
  let payload = Printf.sprintf "[%s]" query in
  let url = Printf.sprintf "%s/lookup" server_url in
  let* frame =
    XmlHttpRequest.perform_raw
      ~response_type:Text
      ~content_type:"application/json"
      ~override_method:`POST
      ~contents:(`String payload)
      url
  in
  match frame with
  | { code = 200; content = body; _ } ->
    begin
      let json_str = Js.to_string body in
      try
        let json = Yojson.Safe.from_string json_str in
        match response_of_yojson json with
        | Ok ((_ , result) :: _) -> Lwt_result.return (parse_rating result.imdb_rating)
        | Ok [] -> Lwt_result.return None
        | Error msg -> Lwt_result.fail (Printf.sprintf "Parse error: %s" msg, `Retry)
      with e ->
        let msg = Printf.sprintf "Parse error: %s. %s" (Printexc.to_string e) json_str in
        Lwt_result.fail (msg, `Retry)
    end
  | { code; _ } ->
    Lwt_result.fail (Printf.sprintf "Server error: %d" code, `Retry)
