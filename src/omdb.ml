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
} [@@deriving yojson { strict = false }]


[@@@warning "+39"]

let omdb_response_of_json str = Yojson.Safe.from_string str |> omdb_response_of_yojson |> Result.get_ok
let omdb_response_to_json entry = omdb_response_to_yojson entry |> Yojson.Safe.to_string


let omdb_api_url = "https://www.omdbapi.com/"
let omdb_key = "omdbApiKey"

(** Parse IMDb rating string to float option *)
let parse_rating rating_str =
  match float_of_string rating_str with
  | n -> Some n
  | exception _ -> None

let fetch_imdb_rating ?year title : (float option, string) Lwt_result.t =
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
        Lwt_result.fail msg
    end
  | { code;  _ } ->
    let msg = Printf.sprintf "Request error: %d. Request: %s" code url in
    Lwt_result.fail msg
