(** Token-based matching using inverted index, with edit distance scoring *)
open Base

(** Indexed title with precomputed tokens and normalized string *)
type indexed_title = {
  entry: Imdb_data.title_entry;
  tokens: string list;
  token_count: int;
  normalized: string;
}

type t = {
  titles: indexed_title array;
  inverted_index: (string, int list) Hashtbl.t;
  normalize: string -> string;
  tokenize: string -> string list;
}

(** Build inverted index from title entries *)
let build ~normalize ~tokenize titles =
  let inverted_index = Hashtbl.create ~size:100_000 (module String) in
  let indexed_titles = Array.map titles ~f:(fun entry ->
      let norm1 = normalize entry.Imdb_data.primary_title in
      let norm2 = normalize entry.Imdb_data.secondary_title in
      let tokens1 = tokenize norm1 in
      let tokens2 = tokenize norm2 in
      (* Combine tokens from both titles, deduplicated *)
      let tokens = List.dedup_and_sort ~compare:String.compare (tokens1 @ tokens2) in
      { entry; tokens; token_count = List.length tokens1; normalized = norm1 }
    )
  in
  Array.iteri indexed_titles ~f:(fun idx { tokens; _ } ->
      List.iter tokens ~f:(fun token ->
          Hashtbl.add_multi inverted_index ~key:token ~data:idx
        )
    );
  { titles = indexed_titles; inverted_index; normalize; tokenize }

(** Find candidate indices that share at least one token with query *)
let lookup t ~query_tokens =
  let seen = Hashtbl.create ~size:1000 (module Int) in
  List.iter query_tokens ~f:(fun token ->
      Hashtbl.find_multi t.inverted_index token
      |> List.iter ~f:(fun idx ->
          Hashtbl.set seen ~key:idx ~data:()
        )
    );
  Hashtbl.keys seen

(** Count how many query tokens appear in the title's tokens *)
let count_matches ~query_tokens ~title_tokens =
  let title_set = Set.of_list (module String) title_tokens in
  List.count query_tokens ~f:(fun token -> Set.mem title_set token)

(** Check if title type matches filter *)
let matches_title_types ~title_types entry =
  match title_types with
  | None -> true
  | Some types -> List.mem ~equal:Poly.equal types entry.Imdb_data.title_type

(** Compare candidates: more token matches, then fewer tokens, then year match *)
let compare_candidates ~query_year (a_entry, a_matches, a_token_count) (b_entry, b_matches, b_token_count) =
  match Int.compare b_matches a_matches with
  | 0 ->
      (match Int.compare a_token_count b_token_count with
       | 0 ->
           (match query_year with
            | None -> 0
            | Some qy ->
                let a_year = Option.value_map a_entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy) in
                let b_year = Option.value_map b_entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy) in
                Bool.compare b_year a_year)
       | n -> n)
  | n -> n

module Normalize = Normalize_lib.Normalize

(** Calculate normalized score: 1.0 = perfect match, 0.0 = completely different.
    Case differences cost 0.1 per char, other edits cost 1.0. *)
let calculate_score ~query ~title =
  let distance = Normalize.weighted_edit_distance query title in
  let max_len = max (String.length query) (String.length title) in
  match max_len with
  | 0 -> 1.0
  | _ -> 1.0 -. (distance /. Float.of_int max_len)

(** Take candidates with best token ranking, then pick best by edit distance *)
let select_best ~norm_query ~query_year candidates =
  match candidates with
  | [] -> None
  | (first_entry, first_matches, first_tc, first_norm) :: rest ->
      let first_year_match = match query_year with
        | None -> false
        | Some qy -> Option.value_map first_entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy)
      in
      (* Find all candidates tied with the best ranking (tokens, count, and year) *)
      let tied = (first_entry, first_matches, first_tc, first_norm) ::
        List.take_while rest ~f:(fun (entry, m, tc, _) ->
          m = first_matches && tc = first_tc &&
          (match query_year with
           | None -> true
           | Some qy ->
               let year_match = Option.value_map entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy) in
               Bool.equal year_match first_year_match))
      in
      (* Among tied candidates, pick the one with lowest weighted edit distance *)
      tied
      |> List.map ~f:(fun (entry, _, _, _normalized) ->
          let dist = Normalize.weighted_edit_distance norm_query entry.Imdb_data.primary_title in
          (entry, dist))
      |> List.min_elt ~compare:(fun (_, a_dist) (_, b_dist) ->
          Float.compare a_dist b_dist)
      |> Option.map ~f:(fun (entry, _dist) -> entry)

(** Search for best matching title *)
let search t ~query ~year ~title_types =
  let norm_query = t.normalize query in
  let query_tokens = t.tokenize norm_query in
  match List.length query_tokens with
  | 0 -> None
  | _ ->
      lookup t ~query_tokens
      |> List.map ~f:(fun idx -> t.titles.(idx))
      |> List.filter ~f:(fun { entry; _ } -> matches_title_types ~title_types entry)
      |> List.map ~f:(fun { entry; tokens; token_count; normalized } ->
          let matches = count_matches ~query_tokens ~title_tokens:tokens in
          (entry, matches, token_count, normalized)
        )
      |> List.filter ~f:(fun (_, matches, _, _) -> matches > 0)
      |> List.sort ~compare:(fun (a_e, a_m, a_tc, _) (b_e, b_m, b_tc, _) ->
          compare_candidates ~query_year:year (a_e, a_m, a_tc) (b_e, b_m, b_tc))
      |> select_best ~norm_query ~query_year:year
      |> Option.map ~f:(fun entry ->
          let score = calculate_score ~query ~title:entry.Imdb_data.primary_title in
          (entry, score))
