open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax

[@@@warning "-39"]

(* Drop native strange handling of json. Lets use ppx_deriving_json instead! *)
(** Type representing the OMDb API response *)
type omdb_response = {
  title: string option [@key "Title"] [@default None];
  year: string option [@key "Year"] [@default None];
  imdbRating: string option [@default None];
  imdbID: string option [@default None];
  response: string [@key "Response"];
  error: string option [@key "Error"] [@default None];
} [@@deriving yojson { strict = false }]


[@@@warning "+39"]

let omdb_response_of_json str = Yojson.Safe.from_string str |> omdb_response_of_yojson |> Result.get_ok
let omdb_response_to_json entry = omdb_response_to_yojson entry |> Yojson.Safe.to_string


let omdb_api_url = "https://www.omdbapi.com/"
let omdb_key = "omdbApiKey"
let omdb_rate_limit = "Request limit reached!"

(** Parse IMDb rating string to float option *)
let parse_rating rating_str = float_of_string_opt rating_str

let fetch_imdb_rating ?year title : (float option, _) Lwt_result.t =
  let encodeUri str = str |> Js.string |> Js.encodeURI |> Js.to_string in
  let* api_key = Storage.load_key omdb_key in
  let api_key = Option.get api_key in
  let url =
    let u = Printf.sprintf "%s?apikey=%s&t=%s" omdb_api_url (encodeUri api_key) (encodeUri title) in
    match year with
    | Some year -> Printf.sprintf "%s&y=%d" u year
    | None -> u
  in
  let* frame = XmlHttpRequest.perform_raw ~response_type:Text url in
  match frame with
  | { code = 200; content = body; _ } ->
    begin
      let json_str = Js.to_string body in
      try
        (* Parse the JSON response using our safe deserializing *)
        let response = omdb_response_of_json json_str in
        match response.imdbRating with
        | Some rating -> Lwt_result.return (parse_rating rating)
        | None -> Lwt_result.return None
      with e ->
        let msg = Printf.sprintf "Parse error: %s. %s" (Printexc.to_string e) json_str in
        Lwt_result.fail (msg, `Retry)
    end
  | { code; content = body; _ } ->
    let json_str = Js.to_string body in
    try
      (* Parse the JSON response using our safe deserializing *)
      let response = omdb_response_of_json json_str in
      match code, response.error with
      | 401, Some msg when String.equal msg omdb_rate_limit -> Lwt_result.fail (msg, `RateLimit)
      | _, Some msg -> Lwt_result.fail (msg, `Retry)
      | _, None -> Lwt_result.fail (Printf.sprintf "Error: %d" code, `Retry)
    with e ->
      let msg = Printf.sprintf "Parse error: %s. %s" (Printexc.to_string e) json_str in
      Lwt_result.fail (msg, `Retry)
