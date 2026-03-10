(** Token-based matching using inverted index, with edit distance scoring *)
open Base

(** Indexed title with precomputed tokens and normalized strings *)
type indexed_title = {
  entry: Imdb_data.title_entry;
  tokens: string list;
  token_count: int;
  normalized_primary: string;
  normalized_secondary: string;
  uchars_primary: Uchar.t array;
  uchars_secondary: Uchar.t array;
}

type t = {
  titles: indexed_title array;
  inverted_index: (string, Set.M(Int).t) Hashtbl.t;
  normalize: string -> string;
  tokenize: string -> string list;
}

(** Build inverted index from title entries *)
let build ~normalize ~tokenize titles =
  let lists = Hashtbl.create ~size:100_000 (module String) in
  let indexed_titles = Array.map titles ~f:(fun entry ->
      let norm1 = normalize entry.Imdb_data.primary_title in
      let norm2 = normalize entry.Imdb_data.secondary_title in
      let tokens1 = tokenize norm1 in
      let tokens2 = tokenize norm2 in
      let tokens = List.dedup_and_sort ~compare:String.compare (tokens1 @ tokens2) in
      { entry; tokens; token_count = List.length tokens1;
        normalized_primary = norm1; normalized_secondary = norm2;
        uchars_primary = Normalize.to_uchars norm1;
        uchars_secondary = Normalize.to_uchars norm2 }
    )
  in
  Array.iteri indexed_titles ~f:(fun idx { tokens; _ } ->
      List.iter tokens ~f:(fun token ->
          Hashtbl.add_multi lists ~key:token ~data:idx
        )
    );
  let inverted_index = Hashtbl.map lists ~f:(Set.of_list (module Int)) in
  { titles = indexed_titles; inverted_index; normalize; tokenize }

(** Lookup using Set union of rarest n/2+1 query tokens *)
let lookup t ~query_tokens =
  let titles =
    query_tokens
    |> List.dedup_and_sort ~compare:String.compare
    |> List.map ~f:(fun token ->
        let count =
          Hashtbl.find t.inverted_index token
          |> Option.value_map ~f:Set.length ~default:0
        in
        token, count
      )
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
    |> Fn.flip List.take ((List.length query_tokens + 1)/2)
    |> List.map ~f:fst
    |> List.filter_map ~f:(fun token -> Hashtbl.find t.inverted_index token)
    |> Set.union_list (module Int)
    |> Set.to_list
  in
  List.length titles, titles

(** Lookup using imperative Bytes counting *)
let _lookup_imperative t ~query_tokens =
  let query_tokens = List.dedup_and_sort ~compare:String.compare query_tokens in
  let counts = Bytes.make (Array.length t.titles) '\000' in
  let touched = ref [] in
  List.iter query_tokens ~f:(fun token ->
      match Hashtbl.find t.inverted_index token with
      | None -> ()
      | Some set ->
        Set.iter set ~f:(fun idx ->
            let prev = Char.to_int (Bytes.get counts idx) in
            (match prev with
             | 0 -> touched := idx :: !touched
             | _ -> ());
            Bytes.set counts idx (Char.of_int_exn (prev + 1)))
    );
  let total = List.length !touched in
  let best = ref 0 in
  let tied = ref [] in
  List.iter !touched ~f:(fun idx ->
      let count = Char.to_int (Bytes.get counts idx) in
      Bytes.set counts idx '\000';
      match count with
      | c when c > !best -> best := c; tied := [idx]
      | c when c = !best -> tied := idx :: !tied
      | _ -> ());
  (total, !tied)

(** Lookup using hashtable counting *)
let _lookup_hashtbl t ~query_tokens =
  let query_tokens = List.dedup_and_sort ~compare:String.compare query_tokens in
  let counts = Hashtbl.create ~size:1000 (module Int) in
  List.iter query_tokens ~f:(fun token ->
      match Hashtbl.find t.inverted_index token with
      | None -> ()
      | Some set ->
        Set.iter set ~f:(fun idx ->
            Hashtbl.update counts idx ~f:(function
              | None -> 1
              | Some n -> n + 1))
    );
  let total = Hashtbl.length counts in
  let (_, tied) =
    Hashtbl.fold counts ~init:(0, []) ~f:(fun ~key:idx ~data:count (best, acc) ->
        match count with
        | c when c > best -> (c, [idx])
        | c when c = best -> (best, idx :: acc)
        | _ -> (best, acc))
  in
  (total, tied)

(** Check if title type matches filter *)
let matches_title_types ~title_types entry =
  match title_types with
  | None -> true
  | Some types -> List.mem ~equal:Poly.equal types entry.Imdb_data.title_type

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

(** Select best candidate by edit distance, with year preference.
    All candidates already have the maximum token match count. *)
let select_best ~query_uchars ~query_year candidates =
  let num_candidates = List.length candidates in
  match candidates with
  | [] -> None
  | _ ->
      let best =
        List.fold_left candidates ~init:None ~f:(fun acc (entry, uchars_primary, uchars_secondary) ->
          let max_edits = Option.map acc ~f:(fun (_, d, _) -> d) in
          let dist_primary = Normalize.weighted_edit_distance_uchars ?max_edits query_uchars uchars_primary in
          let dist_secondary = Normalize.weighted_edit_distance_uchars ?max_edits query_uchars uchars_secondary in
          let dist = Float.min dist_primary dist_secondary in
          let year_match = match query_year with
            | Some qy -> Option.value_map entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy)
            | None -> false
          in
          match acc with
          | Some (_, best_dist, _) when Float.( < ) dist best_dist -> Some (entry, dist, year_match)
          | Some (_, best_dist, best_year) when Float.equal dist best_dist && (not best_year) && year_match -> Some (entry, dist, year_match)
          | Some _ -> acc
          | None -> Some (entry, dist, year_match))
      in
      let stats = { candidates = num_candidates; tied = num_candidates } in
      Option.map best ~f:(fun (entry, _dist, _) -> (entry, stats))

(** Search for best matching title *)
let search t ~query ~year ~title_types =
  let norm_query = t.normalize query in
  let query_uchars = Normalize.to_uchars norm_query in
  let query_tokens = t.tokenize norm_query in
  match query_tokens with
  | [] -> None
  | _ ->
      let query_len = String.length norm_query in
      let (total_candidates, raw) = lookup t ~query_tokens in
      let candidates =
        raw
        |> List.filter_map ~f:(fun idx ->
            let { entry; normalized_primary; normalized_secondary;
                  uchars_primary; uchars_secondary; _ } = t.titles.(idx) in
            match matches_title_types ~title_types entry with
            | false -> None
            | true -> Some (entry, normalized_primary, normalized_secondary,
                           uchars_primary, uchars_secondary))
        |> List.sort ~compare:(fun (_, p1, s1, _, _) (_, p2, s2, _, _) ->
            let len a b = Int.min (Int.abs (String.length a - query_len))
                                  (Int.abs (String.length b - query_len)) in
            Int.compare (len p1 s1) (len p2 s2))
      in
      let num_tied = List.length candidates in
      candidates
      |> List.map ~f:(fun (entry, _, _, up, us) -> (entry, up, us))
      |> select_best ~query_uchars ~query_year:year
      |> Option.map ~f:(fun (entry, _) ->
          let score = calculate_score ~query ~title:entry.Imdb_data.primary_title in
          (entry, score, { candidates = total_candidates; tied = num_tied }))
