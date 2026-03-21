(** IMDB TSV data loading and parsing *)
open Base


type title_entry = {
  tconst: int;
  title_type: Types.title_type;
  primary_title: string;
  secondary_title: string;
  year: int option;
  rating: float;
  votes: int;
}

let parse_tconst s =
  if String.length s > 2 && Char.equal (String.get s 0) 't' && Char.equal (String.get s 1) 't'
  then Int.of_string_opt (String.drop_prefix s 2)
  else None

let parse_int field =
  match Int.of_string_opt field with
  | Some n -> n
  | None -> raise (Types.Parse_error (Printf.sprintf "Invalid int: %s" field))

let parse_float field =
  match Float.of_string_opt field with
  | Some f -> f
  | None -> raise (Types.Parse_error (Printf.sprintf "Invalid float: %s" field))

(** Parse title.basics.tsv row *)
type title_basics = {
  b_tconst: int;
  b_title_type: Types.title_type;
  b_primary_title: string;
  b_secondary_title: string;
  b_start_year: int option;
}

let parse_basics_row = function
  | b_tconst :: entry_type :: b_primary_title :: b_secondary_title :: _ :: start_year :: _ ->
    (match parse_tconst b_tconst with
     | Some id -> Some { b_tconst = id; b_title_type = Types.title_type_of_string entry_type; b_primary_title; b_secondary_title; b_start_year = Int.of_string_opt start_year }
     | None -> None)
  | l -> let s = String.concat ~sep:"; " l in Stdlib.Printf.printf "Could not parse [ %s ]\n" s; None
(* raise (Types.Parse_error (Printf.sprintf "Invalid basics row with %d fields" (List.length row))) *)

(** Parse title.ratings.tsv row *)
type title_rating = {
  r_tconst: int;
  r_average_rating: float;
  r_num_votes: int;
}

let parse_rating_row = function
  | [r_tconst; rating; votes] ->
      (match parse_tconst r_tconst with
       | Some id -> Some { r_tconst = id; r_average_rating = parse_float rating; r_num_votes = parse_int votes }
       | None -> None)
  | l -> let s = String.concat ~sep:"; " l in Stdlib.Printf.printf "Could not parse [ %s ]\n" s; None
(* raise (Types.Parse_error (Printf.sprintf "Invalid rating row with %d fields" (List.length row))) *)

(** Read imdb titles and ratings *)
let read ~filter dir =
  let ratings_table =
    let ratings = Eio.Path.(dir / "title.ratings.tsv") in
    let parse_ratings : string Stdlib.Seq.t -> _ = fun seq ->
      let tbl = Hashtbl.create (module Int) in
      seq
      |> Stdlib.Seq.drop 1
      |> Stdlib.Seq.map String.strip
      |> Stdlib.Seq.filter (fun s -> String.length s > 0)
      |> Stdlib.Seq.map (String.split ~on:'\t')
      |> Stdlib.Seq.filter_map parse_rating_row
      |> Stdlib.Seq.iter (fun rating ->
          Hashtbl.set tbl ~key:rating.r_tconst ~data:rating);
      tbl
    in
    Eio.Path.with_lines ratings parse_ratings
  in

  let parse_basics seq =
    let total_basics = ref 0 in
    let result =
      seq
      |> Stdlib.Seq.drop 1
      |> Stdlib.Seq.map (String.split ~on:'\t')
      |> Stdlib.Seq.filter_map parse_basics_row
      |> Stdlib.Seq.map (fun basic -> Int.incr total_basics; basic)
      |> Stdlib.Seq.filter_map (fun basic ->
          match Hashtbl.find ratings_table basic.b_tconst with
          | None -> None
          | Some rating -> Some (basic, rating)
        )
      |> Stdlib.Seq.map (fun (basic, rating) ->
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
      |> Stdlib.Seq.filter filter
      |> Stdlib.Array.of_seq
    in
    let with_ratings = Array.length result in
    Stdlib.Printf.printf "Skipped %d titles without ratings\n%!" (!total_basics - with_ratings);
    result
  in
  let basics = Eio.Path.(dir / "title.basics.tsv") in
  Eio.Path.with_lines basics parse_basics
