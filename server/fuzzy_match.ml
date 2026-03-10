(** Token-based matching using inverted index, with edit distance scoring *)
open Base

(** Indexed title with precomputed tokens and normalized strings *)
type indexed_title = {
  entry: Imdb_data.title_entry;
  tokens: string list;
  token_count: int;
  normalized_primary: string;
  normalized_secondary: string;
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
      { entry; tokens; token_count = List.length tokens1;
        normalized_primary = norm1; normalized_secondary = norm2 }
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

type search_stats = {
  candidates: int;
  tied: int;
}

(** Take candidates with best token ranking, then pick best by edit distance *)
let select_best ~norm_query ~query_year candidates =
  let num_candidates = List.length candidates in
  match candidates with
  | [] -> None
  | (first_entry, first_matches, first_tc, _, _) :: _ ->
      let first_year_match = match query_year with
        | None -> false
        | Some qy -> Option.value_map first_entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy)
      in
      let tied = List.take_while candidates ~f:(fun (entry, m, tc, _, _) ->
          m = first_matches && tc = first_tc &&
          (match query_year with
           | None -> true
           | Some qy ->
               let year_match = Option.value_map entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy) in
               Bool.equal year_match first_year_match))
      in
      let num_tied = List.length tied in
      let best =
        List.fold_left tied ~init:None ~f:(fun acc (entry, _, _, norm_primary, norm_secondary) ->
          let max_edits = Option.map acc ~f:snd in
          let dist_primary = Normalize.weighted_edit_distance ?max_edits norm_query norm_primary in
          let dist_secondary = Normalize.weighted_edit_distance ?max_edits norm_query norm_secondary in
          let dist = Float.min dist_primary dist_secondary in
          match acc with
          | Some (_, best_dist) when Float.( >= ) dist best_dist -> acc
          | _ -> Some (entry, dist))
      in
      let stats = { candidates = num_candidates; tied = num_tied } in
      Option.map best ~f:(fun (entry, _dist) -> (entry, stats))

(** Search for best matching title *)
let search t ~query ~year ~title_types =
  let norm_query = t.normalize query in
  let query_tokens = t.tokenize norm_query in
  match List.length query_tokens with
  | 0 -> None
  | _ ->
      lookup t ~query_tokens
      |> List.filter_map ~f:(fun idx ->
          let { entry; tokens; token_count;
                normalized_primary; normalized_secondary } = t.titles.(idx) in
          if not (matches_title_types ~title_types entry) then None
          else
            let matches = count_matches ~query_tokens ~title_tokens:tokens in
            if matches = 0 then None
            else Some (entry, matches, token_count, normalized_primary, normalized_secondary))
      |> List.sort ~compare:(fun (a_e, a_m, a_tc, _, _) (b_e, b_m, b_tc, _, _) ->
          compare_candidates ~query_year:year (a_e, a_m, a_tc) (b_e, b_m, b_tc))
      |> select_best ~norm_query ~query_year:year
      |> Option.map ~f:(fun (entry, stats) ->
          let score = calculate_score ~query ~title:entry.Imdb_data.primary_title in
          (entry, score, stats))
