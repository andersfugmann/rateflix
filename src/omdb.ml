open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax

[@@@warning "-39"]

(** Type representing the OMDb API response *)
type omdb_response = {
  title: string option;
  year: string option;
  imdbRating: string option;
  imdbID: string option;
  response: string option;
  error: string option;
} [@@deriving json]

[@@@warning "+39"]

let omdb_response_of_json = Deriving_Json.from_string omdb_response_json
let omdb_response_to_json = Deriving_Json.to_string omdb_response_json

let omdb_api_url = "https://www.omdbapi.com/"
let omdb_key = "omdbApiKey"

(** Parse IMDb rating string to float option *)
let parse_rating rating_str =
  match float_of_string rating_str with
  | n -> Some n
  | exception _ -> None

let fetch_imdb_rating ~title : (float option, string) Lwt_result.t =
  let encodeUri str = str |> Js.string |> Js.encodeURI |> Js.to_string in
  match Storage.load_key omdb_key with
  | None -> Lwt_result.fail "No API key"
  | Some api_key ->
    let url =
      Printf.sprintf "%s?apikey=%s&t=%s" omdb_api_url (encodeUri api_key) (encodeUri title)
    in
    let* frame = XmlHttpRequest.perform_raw ~response_type:Text url in
    match frame with
    | { code = 200; content = body; _ } ->
      begin
        try
          (* Parse the JSON response using our safe deserializer *)
          let json_str = Js.to_string body in
          let response = omdb_response_of_json json_str in
          match response.imdbRating with
          | Some rating -> Lwt_result.return (parse_rating rating)
          | None -> Lwt_result.return None
        with e ->
          let msg = Printf.sprintf "Parse error: %s" (Printexc.to_string e) in
          Lwt_result.fail msg
      end
    | { code;  _ } ->
      let msg = Printf.sprintf "Request error: %d" code in
      Lwt_result.fail msg
