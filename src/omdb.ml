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


let json_test = {|
{
  "Title": "Manifest",
  "Year": "2018–2023",
  "Rated": "TV-14",
  "Released": "24 Sep 2018",
  "Runtime": "43 min",
  "Genre": "Drama, Mystery, Sci-Fi",
  "Director": "N/A",
  "Writer": "Jeff Rake",
  "Actors": "Melissa Roxburgh, Josh Dallas, J.R. Ramirez",
  "Plot": "When a commercial airliner suddenly reappears after being missing for five years, those aboard must reintegrate into society.",
  "Language": "English",
  "Country": "United States",
  "Awards": "1 win & 6 nominations total",
  "Poster": "https://m.media-amazon.com/images/M/MV5BMTFlNjg0YjAtYzMwOC00Zjc0LTkwYjAtZmRiYzdjNjcyYjc2XkEyXkFqcGc@._V1_SX300.jpg",
  "Ratings": [
    {
      "Source": "Internet Movie Database",
      "Value": "7.0/10"
    }
  ],
  "Metascore": "N/A",
  "imdbRating": "7.0",
  "imdbVotes": "98,085",
  "imdbID": "tt8421350",
  "Type": "series",
  "totalSeasons": "4",
  "Response": "True"
}
  |}

[@@@warning "+39"]

let omdb_response_of_json str = Yojson.Safe.from_string str |> omdb_response_of_yojson |> Result.get_ok
let omdb_response_to_json entry = omdb_response_to_yojson entry |> Yojson.Safe.to_string


let () =
  (* Print the result of a converted type! *)
  try
    let _ = omdb_response_of_json json_test in
    Console.console##info "Json parsed ok"
  with
  | e -> Console.console##info (Printf.sprintf "Json parse failed: %s" @@ Printexc.to_string e)

let () =
  try
    let _ = omdb_response_of_json {| {"Response":"False","Error":"Movie not found!"} |} in
    Console.console##info "Json2 parsed ok"
  with
  | e -> Console.console##info (Printf.sprintf "Json2 parse failed: %s" @@ Printexc.to_string e)


let () =
  let response = {
    title = Some "Title";
    year = Some "1999";
    imdbRating = Some "109.0";
    imdbID = Some "tt00001";
    response = "True";
  } in
  let str = omdb_response_to_json response in
  Console.console##info (Printf.sprintf "Json to string: %s" str)


let omdb_api_url = "https://www.omdbapi.com/"
let omdb_key = "omdbApiKey"

(** Parse IMDb rating string to float option *)
let parse_rating rating_str =
  match float_of_string rating_str with
  | n -> Some n
  | exception _ -> None

let fetch_imdb_rating ~title : (float option, string) Lwt_result.t =
  let encodeUri str = str |> Js.string |> Js.encodeURI |> Js.to_string in
  let* api_key = Storage.load_key omdb_key in
  let api_key = Option.get api_key in
  let url = Printf.sprintf "%s?apikey=%s&t=%s" omdb_api_url (encodeUri api_key) (encodeUri title) in
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
