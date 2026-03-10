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
      let tokens = List.dedup_and_sort ~compare:String.compare (tokens1 @ tokens2) in
      { entry; tokens; token_count = List.length tokens1;
        normalized_primary = norm1; normalized_secondary = norm2;
        uchars_primary = Normalize.to_uchars norm1;
        uchars_secondary = Normalize.to_uchars norm2 }
    )
  in
  Array.iteri indexed_titles ~f:(fun idx { tokens; _ } ->
      List.iter tokens ~f:(fun token ->
          Hashtbl.add_multi inverted_index ~key:token ~data:idx
        )
    );
  { titles = indexed_titles; inverted_index; normalize; tokenize }

(** Print histogram of token frequency distribution *)
let print_token_histogram t =
  let freqs = Hashtbl.map t.inverted_index ~f:List.length in
  (* Bucket boundaries: 1, 2-5, 6-10, 11-50, 51-100, 101-500, 501-1K, 1K-5K, 5K-10K, 10K+ *)
  let buckets = [| 1; 5; 10; 50; 100; 500; 1_000; 5_000; 10_000; Int.max_value |] in
  let bucket_counts = Array.create ~len:(Array.length buckets) 0 in
  let top_tokens = ref [] in
  Hashtbl.iteri freqs ~f:(fun ~key:token ~data:count ->
    let idx = Array.findi_exn buckets ~f:(fun _ b -> count <= b) |> fst in
    bucket_counts.(idx) <- bucket_counts.(idx) + 1;
    if count > 1000 then top_tokens := (token, count) :: !top_tokens);
  let labels = [| "1"; "2-5"; "6-10"; "11-50"; "51-100"; "101-500";
                  "501-1K"; "1K-5K"; "5K-10K"; "10K+" |] in
  Stdlib.Printf.printf "Token frequency histogram (%d unique tokens):\n%!"
    (Hashtbl.length t.inverted_index);
  Array.iteri labels ~f:(fun i label ->
    if bucket_counts.(i) > 0 then
      Stdlib.Printf.printf "  %8s: %d tokens\n%!" label bucket_counts.(i));
  let top = List.sort !top_tokens ~compare:(fun (_, a) (_, b) -> Int.compare b a) in
  (match List.take top 20 with
   | [] -> ()
   | top ->
     Stdlib.Printf.printf "Top tokens (>1000 titles):\n%!";
     List.iter top ~f:(fun (token, count) ->
       Stdlib.Printf.printf "  %-20s %d\n%!" token count))

(** Find candidate indices with match counts.
    Counts how many query tokens each candidate shares, then returns
    only candidates with the maximum match count in a single fold. *)
let lookup t ~query_tokens =
  let counts = Hashtbl.create ~size:1000 (module Int) in
  List.iter query_tokens ~f:(fun token ->
      Hashtbl.find_multi t.inverted_index token
      |> List.iter ~f:(fun idx ->
          Hashtbl.update counts idx ~f:(function
            | None -> 1
            | Some n -> n + 1)
        )
    );
  Hashtbl.fold counts ~init:(0, []) ~f:(fun ~key:idx ~data:count (best, acc) ->
    if count > best then (count, [(idx, count)])
    else if count = best then (best, (idx, count) :: acc)
    else (best, acc))
  |> snd

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
  match List.length query_tokens with
  | 0 -> None
  | _ ->
      let query_len = String.length norm_query in
      lookup t ~query_tokens
      |> List.filter_map ~f:(fun (idx, _matches) ->
          let { entry; tokens = _; token_count = _;
                normalized_primary; normalized_secondary;
                uchars_primary; uchars_secondary } = t.titles.(idx) in
          if not (matches_title_types ~title_types entry) then None
          else Some (entry, normalized_primary, normalized_secondary,
                     uchars_primary, uchars_secondary))
      |> List.sort ~compare:(fun (_, p1, s1, _, _) (_, p2, s2, _, _) ->
          let len a b = Int.abs (String.length a - query_len) |> Int.min (Int.abs (String.length b - query_len)) in
          Int.compare (len p1 s1) (len p2 s2))
      |> List.map ~f:(fun (entry, _, _, up, us) -> (entry, up, us))
      |> select_best ~query_uchars ~query_year:year
      |> Option.map ~f:(fun (entry, stats) ->
          let score = calculate_score ~query ~title:entry.Imdb_data.primary_title in
          (entry, score, stats))
