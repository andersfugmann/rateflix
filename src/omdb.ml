open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax

let omdb_api_url = "https://www.omdbapi.com/"
let key = "omdbApiKey"

let save_key value =
  let storage = Dom_html.window##.localStorage in
  Js.Optdef.iter storage (fun storage -> storage##setItem (Js.string key) (Js.string value))

let load_key () =
  let storage = Dom_html.window##.localStorage |> Js.Optdef.to_option in
  let value =
    Option.bind storage
      (fun storage ->
         storage##getItem (Js.string key)
         |> Js.Opt.to_option
      )
  in
  value


let get_api_key () : string option =
  let storage = Dom_html.window##.localStorage in
  let key = "omdbApiKey" in
  match Js.Optdef.to_option storage with
  | Some storage ->
      let v = storage##getItem (Js.string key) in
      Js.Opt.to_option v |> Option.map Js.to_string
  | None -> None

let fetch_imdb_rating ~title : (string, string) Lwt_result.t =
  match get_api_key () with
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
