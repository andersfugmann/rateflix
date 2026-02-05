(** IMDB TSV data loading and parsing *)

type title_entry = {
  tconst: string;
  title_type: string;
  primary_title: string;
  year: int option;
  rating: float;
  votes: int;
}

let parse_nullable = function
  | "\\N" -> None
  | s -> Some s

let parse_year field =
  parse_nullable field
  |> Option.map int_of_string

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
  b_title_type: string;
  b_primary_title: string;
  b_start_year: int option;
}

let parse_basics_row = function
  | b_tconst :: b_title_type :: b_primary_title :: _ :: _ :: start_year :: _ ->
      Some { b_tconst; b_title_type; b_primary_title; b_start_year = parse_year start_year }
  | _ -> None
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
  | _ -> None
(* raise (Types.Parse_error (Printf.sprintf "Invalid rating row with %d fields" (List.length row))) *)

(** Decompress gzip content using gunzip command *)
let decompress_if_gz ~path content =
  (* TODO: Dont use external commands. *)
  match String.ends_with ~suffix:".gz" path with
  | false -> content
  | true ->
      (* Write content to temp file and decompress with gunzip *)
      let tmp_gz = Filename.temp_file "imdb" ".gz" in
      let tmp_out = Filename.temp_file "imdb" ".tsv" in
      let oc = open_out_bin tmp_gz in
      output_string oc content;
      close_out oc;
      let cmd = Printf.sprintf "gunzip -c %s > %s" (Filename.quote tmp_gz) (Filename.quote tmp_out) in
      (match Sys.command cmd with
       | 0 ->
           let ic = open_in_bin tmp_out in
           let len = in_channel_length ic in
           let result = really_input_string ic len in
           close_in ic;
           Sys.remove tmp_gz;
           Sys.remove tmp_out;
           result
       | n -> raise (Types.Parse_error (Printf.sprintf "gunzip failed with exit code %d" n)))

(** Read and parse a TSV file - simple tab-split, no quoting *)
let read_tsv ~path content =
  decompress_if_gz ~path content
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length (String.trim line) > 0)
  |> List.map (String.split_on_char '\t')

(** Find file with optional .gz extension *)
let find_file dir basename =
  let try_path suffix =
    let path = Eio.Path.(dir / (basename ^ suffix)) in
    match Eio.Path.is_file path with
    | true -> Some (path, basename ^ suffix)
    | false -> None
  in
  match try_path "" with
  | Some result -> result
  | None ->
      match try_path ".gz" with
      | Some result -> result
      | None -> raise (Types.Parse_error (Printf.sprintf "File not found: %s or %s.gz" basename basename))

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
    seq
    |> Seq.drop 1
    |> Seq.map String.trim
    |> Seq.filter (fun s -> String.length s > 0)
    |> Seq.map (String.split_on_char '\t')
    |> Seq.filter_map parse_basics_row
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
          year = basic.b_start_year;
          rating = rating.r_average_rating;
          votes = rating.r_num_votes;
        }
      )
    |> Seq.filter filter
    |> Array.of_seq
  in
  let basics = Eio.Path.(dir / "title.basics.tsv") in
  Eio.Path.with_lines basics parse_basics
