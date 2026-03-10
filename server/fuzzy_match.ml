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
  inverted_index: (string, int array) Hashtbl.t;
  normalize: string -> string;
  tokenize: string -> string list;
  counts: Bytes.t;
}

(** Minimum token length exempt from frequency filtering *)
let min_stopword_length = 4

(** Maximum fraction of titles a short token may appear in before being filtered *)
let max_stopword_frequency = 0.02

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
  let num_titles = Array.length indexed_titles in
  let threshold = Float.to_int (Float.of_int num_titles *. max_stopword_frequency) in
  let filtered = ref 0 in
  let inverted_index = Hashtbl.filter_mapi lists ~f:(fun ~key:token ~data:entries ->
      if String.length token < min_stopword_length && List.length entries > threshold then
        (Int.incr filtered; None)
      else
        Some (Array.of_list entries))
  in
  Stdlib.Printf.printf "Filtered %d stopword tokens (length < %d, frequency > %.0f%% = %d titles)\n%!"
    !filtered min_stopword_length (max_stopword_frequency *. 100.0) threshold;
  let counts = Bytes.make num_titles '\000' in
  { titles = indexed_titles; inverted_index; normalize; tokenize; counts }

(** Print histogram of token frequency distribution *)
let print_token_histogram t =
  let freqs = Hashtbl.map t.inverted_index ~f:Array.length in
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
  let counts = Bytes.create (Array.length t.titles) in
  let touched =
    List.fold ~init:[] query_tokens ~f:(fun acc token ->
        match Hashtbl.find t.inverted_index token with
        | None -> acc
        | Some arr ->
          Array.fold ~init:acc arr ~f:(fun acc idx ->
              let prev = Char.to_int (Bytes.get counts idx) in
              Bytes.set counts idx (Char.of_int_exn (prev + 1));
              match prev = 0 with
              | true -> idx :: acc
              | false -> acc
            );
      )
  in
  let total = List.length touched in
  let (_best, tied) =
    List.fold ~init:(0, []) touched ~f:(fun (best, acc) idx ->
        match Char.to_int (Bytes.get counts idx) with
        | count when count > best -> (count, [idx])
        | count when count < best -> (best, acc)
        | _ -> (best, idx :: acc)
      )
  in
  (total, tied)

let _lookup t ~query_tokens =
  let counts = t.counts in
  let touched = ref [] in
  List.iter query_tokens ~f:(fun token ->
      match Hashtbl.find t.inverted_index token with
      | None -> ()
      | Some arr ->
        Array.iter arr ~f:(fun idx ->
            let prev = Char.to_int (Bytes.get counts idx) in
            if prev = 0 then touched := idx :: !touched;
            Bytes.set counts idx (Char.of_int_exn (prev + 1))
          )
    );
  let total = List.length !touched in
  let best = ref 0 in
  let tied = ref [] in
  List.iter !touched ~f:(fun idx ->
      let count = Char.to_int (Bytes.get counts idx) in
      Bytes.set counts idx '\000';
      if count > !best then (best := count; tied := [idx])
      else if count = !best then tied := idx :: !tied
    );
  (total, !tied)

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
  lookup_ms: float;
  filter_ms: float;
  sort_ms: float;
  spelll_ms: float;
  ours_ms: float;
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
      let stats = { candidates = num_candidates; tied = num_candidates;
                    lookup_ms = 0.0; filter_ms = 0.0; sort_ms = 0.0;
                    spelll_ms = 0.0; ours_ms = 0.0 } in
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
      let t0 = Unix.gettimeofday () in
      let (total_candidates, raw) = lookup t ~query_tokens in
      let t1 = Unix.gettimeofday () in
      let filtered =
        raw
        |> List.filter_map ~f:(fun idx ->
            let { entry; tokens = _; token_count = _;
                  normalized_primary; normalized_secondary;
                  uchars_primary; uchars_secondary } = t.titles.(idx) in
            if not (matches_title_types ~title_types entry) then None
            else Some (entry, normalized_primary, normalized_secondary,
                       uchars_primary, uchars_secondary))
      in
      let t2 = Unix.gettimeofday () in
      let candidates =
        filtered
        |> List.sort ~compare:(fun (_, p1, s1, _, _) (_, p2, s2, _, _) ->
            let len a b = Int.abs (String.length a - query_len) |> Int.min (Int.abs (String.length b - query_len)) in
            Int.compare (len p1 s1) (len p2 s2))
      in
      let t3 = Unix.gettimeofday () in
      (* Benchmark: Spelll.edit_distance with upper bound *)
      let _spelll_best =
        List.fold_left candidates ~init:None ~f:(fun acc (entry, norm_primary, norm_secondary, _, _) ->
          let max_dist = Option.map acc ~f:(fun (_, d, _) -> d) in
          let skip p = match max_dist with Some m -> String.length p - query_len > m || query_len - String.length p > m | None -> false in
          let dist_primary = if skip norm_primary then Int.max_value else Spelll.edit_distance norm_query norm_primary in
          let dist_secondary = if skip norm_secondary then Int.max_value else Spelll.edit_distance norm_query norm_secondary in
          let dist = Int.min dist_primary dist_secondary in
          let year_match = match year with
            | Some qy -> Option.value_map entry.Imdb_data.year ~default:false ~f:(fun y -> y = qy)
            | None -> false
          in
          match acc with
          | Some (_, best_dist, _) when dist < best_dist -> Some (entry, dist, year_match)
          | Some (_, best_dist, best_year) when dist = best_dist && (not best_year) && year_match -> Some (entry, dist, year_match)
          | Some _ -> acc
          | None -> Some (entry, dist, year_match))
      in
      let t4 = Unix.gettimeofday () in
      (* Our implementation *)
      let result =
        candidates
        |> List.map ~f:(fun (entry, _, _, up, us) -> (entry, up, us))
        |> select_best ~query_uchars ~query_year:year
      in
      let t5 = Unix.gettimeofday () in
      let to_ms a b = (b -. a) *. 1000.0 in
      let num_tied = List.length candidates in
      result
      |> Option.map ~f:(fun (entry, _stats) ->
          let score = calculate_score ~query ~title:entry.Imdb_data.primary_title in
          (entry, score, { candidates = total_candidates;
                           tied = num_tied;
                           lookup_ms = to_ms t0 t1;
                           filter_ms = to_ms t1 t2;
                           sort_ms = to_ms t2 t3;
                           spelll_ms = to_ms t3 t4;
                           ours_ms = to_ms t4 t5 }))
