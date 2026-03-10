(** IMDB TSV data loading and parsing *)


type title_entry = {
  tconst: string;
  title_type: Types.title_type;
  primary_title: string;
  secondary_title: string;
  year: int option;
  rating: float;
  votes: int;
}

let parse_int field =
  match int_of_string_opt field with
  | Some n -> n
  | None -> raise (Types.Parse_error (Printf.sprintf "Invalid int: %s" field))

let parse_float field =
  match float_of_string_opt field with
  | Some f -> f
  | None -> raise (Types.Parse_error (Printf.sprintf "Invalid float: %s" field))

(** Parse title.basics.tsv row *)
type title_basics = {
  b_tconst: string;
  b_title_type: Types.title_type;
  b_primary_title: string;
  b_secondary_title: string;
  b_start_year: int option;
}

let parse_basics_row = function
  | b_tconst :: entry_type :: b_primary_title :: b_secondary_title :: _ :: start_year :: _ ->
    Some { b_tconst; b_title_type = Types.title_type_of_string entry_type; b_primary_title; b_secondary_title; b_start_year = int_of_string_opt start_year }
  | l -> let s = String.concat "; " l in Stdlib.Printf.printf "Could not parse [ %s ]\n" s; None
(* raise (Types.Parse_error (Printf.sprintf "Invalid basics row with %d fields" (List.length row))) *)

(** Parse title.ratings.tsv row *)
type title_rating = {
  r_tconst: string;
  r_average_rating: float;
  r_num_votes: int;
}

let parse_rating_row = function
  | [r_tconst; rating; votes] ->
      Some { r_tconst; r_average_rating = parse_float rating; r_num_votes = parse_int votes }
  | l -> let s = String.concat "; " l in Stdlib.Printf.printf "Could not parse [ %s ]\n" s; None
(* raise (Types.Parse_error (Printf.sprintf "Invalid rating row with %d fields" (List.length row))) *)

(** Read imdb titles and ratings *)
let read ~filter dir =
  let ratings_table =
    let ratings = Eio.Path.(dir / "title.ratings.tsv") in
    let parse_ratings : string Seq.t -> 'a = fun seq ->
      seq
      |> Seq.drop 1
      |> Seq.map String.trim
      |> Seq.filter (fun s -> String.length s > 0)
      |> Seq.map (String.split_on_char '\t')
      |> Seq.filter_map parse_rating_row
      |> Seq.map (fun rating -> (rating.r_tconst, rating))
      |> Hashtbl.of_seq
    in
    Eio.Path.with_lines ratings parse_ratings
  in

  let parse_basics seq =
    let total_basics = ref 0 in
    let result =
      seq
      |> Seq.drop 1
      |> Seq.map (String.split_on_char '\t')
      |> Seq.filter_map parse_basics_row
      |> Seq.map (fun basic -> incr total_basics; basic)
      |> Seq.filter_map (fun basic ->
          match Hashtbl.find_opt ratings_table basic.b_tconst with
          | None -> None
          | Some rating -> Some (basic, rating)
        )
      |> Seq.map (fun (basic, rating) ->
          {
            tconst = basic.b_tconst;
            title_type = basic.b_title_type;
            primary_title = basic.b_primary_title;
            secondary_title = basic.b_secondary_title;
            year = basic.b_start_year;
            rating = rating.r_average_rating;
            votes = rating.r_num_votes;
          }
        )
      |> Seq.filter filter
      |> Array.of_seq
    in
    let with_ratings = Array.length result in
    Printf.printf "Skipped %d titles without ratings\n%!" (!total_basics - with_ratings);
    result
  in
  let basics = Eio.Path.(dir / "title.basics.tsv") in
  Eio.Path.with_lines basics parse_basics
