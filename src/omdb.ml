open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax

let omdb_api_url = "https://www.omdbapi.com/"
let omdb_key = "omdbApiKey"

let fetch_imdb_rating ~title : (string, string) Lwt_result.t =
  match Storage.load_key omdb_key with
  | None -> Lwt_result.fail "No API key"
  | Some api_key ->
      let url =
        Printf.sprintf "%s?apikey=%s&t=%s" omdb_api_url (Js.to_string (Js.encodeURI (Js.string api_key))) (Js.to_string (Js.encodeURI (Js.string title)))
      in
      let open XmlHttpRequest in
      let* frame = perform_raw ~response_type:Text url in
      match frame.code, frame.content with
      | 200, body ->
          (try
             let json = Json.unsafe_input body in
             Lwt_result.return (json##.imdbRating |> Js.to_string)
           with _ -> Lwt_result.fail "Parse error")
      | _ -> Lwt_result.fail "Request error"
