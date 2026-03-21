(** Token-based matching using inverted index, with edit distance scoring *)
open Base

(** Indexed title with precomputed merged token list *)
type indexed_title = {
  entry: Imdb_data.title_entry;
  tokens: string list;
}

type t = {
  titles: indexed_title array;
  inverted_index: (string, Set.M(Int).t) Hashtbl.t;
  normalize: string -> string;
  tokenize: string -> string list;
}

(** Marshallable subset of t (no function values).
    Base's Set and Hashtbl contain comparator functions, so we convert
    to plain association lists for marshalling. *)

let cache_version = 2

type cache = {
  version: int;
  cached_titles: indexed_title array;
  cached_inverted_index: (string * int list) list;
}

let to_cache t =
  let index_list =
    Hashtbl.fold t.inverted_index ~init:[] ~f:(fun ~key ~data acc ->
      (key, Set.to_list data) :: acc)
  in
  { version = cache_version;
    cached_titles = t.titles;
    cached_inverted_index = index_list }

let of_cache ~normalize ~tokenize c =
  if c.version <> cache_version then
    failwith (Printf.sprintf "Cache version mismatch: expected %d, got %d"
                cache_version c.version);
  let inverted_index = Hashtbl.create ~size:(List.length c.cached_inverted_index) (module String) in
  List.iter c.cached_inverted_index ~f:(fun (key, indices) ->
    Hashtbl.set inverted_index ~key ~data:(Set.of_list (module Int) indices));
  { titles = c.cached_titles; inverted_index; normalize; tokenize }

let hashcons () =
  let table = Hashtbl.create ~size:200_000 (module String) in
  fun s ->
    Hashtbl.find_or_add table s ~default:(fun () -> s)

(** Build inverted index from title entries *)
let build ~normalize ~tokenize titles =
  let hashcons = hashcons () in
  let lists = Hashtbl.create ~size:100_000 (module String) in
  (* Hash consing: identical token strings share a single heap allocation *)
  let indexed_titles = Array.map titles ~f:(fun entry ->
      let norm1 = normalize entry.Imdb_data.primary_title in
      let norm2 = normalize entry.Imdb_data.secondary_title in
      let tokens_primary = tokenize norm1 in
      let tokens_secondary = tokenize norm2 in
      let tokens =
        List.dedup_and_sort ~compare:String.compare (tokens_primary @ tokens_secondary)
        |> List.map ~f:hashcons
      in
      { entry; tokens }
    )
  in
  Array.iteri indexed_titles ~f:(fun idx { tokens; _ } ->
      List.iter tokens ~f:(fun token ->
          Hashtbl.add_multi lists ~key:token ~data:idx
        )
    );
  let inverted_index = Hashtbl.map lists ~f:(Set.of_list (module Int)) in
  { titles = indexed_titles; inverted_index; normalize; tokenize }

let lookup t ~query_tokens =
  let titles =
    query_tokens
    |> List.dedup_and_sort ~compare:String.compare
    |> List.filter_map ~f:(fun token ->
        Hashtbl.find t.inverted_index token
        |> Option.map ~f:Set.length
        |> Option.map ~f:(fun count -> token, count)
      )
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
    |> List.filter_mapi ~f:(fun i (t, c) -> match i > 0 && c > 10000 with true -> None | false -> Some t)
    |> List.filter_map ~f:(fun token -> Hashtbl.find t.inverted_index token)
    |> Set.union_list (module Int)
  in
  Set.length titles, Set.to_list titles

(** Check if title type matches filter *)
let matches_title_types ~title_types entry =
  match title_types with
  | None -> true
  | Some types -> List.mem ~equal:Poly.equal types entry.Imdb_data.title_type

(** Jaccard similarity between two token sets: |intersection| / |union|.
    Returns 0.0 when both sets are empty, 1.0 for identical sets. *)
let jaccard_similarity xs =
  let set_x = Set.of_list (module String) xs in
  fun ys ->
  let set_y = Set.of_list (module String) ys in
  match Set.union set_x set_y |> Set.length with
  | 0 -> 0.0
  | union ->
    let shared = Set.inter set_x set_y |> Set.length in
    Float.(of_int shared / of_int union)

(** Calculate normalized score: 1.0 = perfect match, 0.0 = completely different.
    Case differences cost 0.1 per char, other edits cost 1.0. *)
let calculate_score ~query title =
  let distance = Normalize.weighted_edit_distance query title in
  let max_len = max (String.length query) (String.length title) in
  match max_len with
  | 0 -> 1.0
  | _ -> 1.0 -. (distance /. Float.of_int max_len)

let calculate_item_score ~query { Imdb_data.primary_title; secondary_title; _ } =
  let primary_score = calculate_score ~query primary_title in
  let secondary_score = calculate_score ~query secondary_title in
  Float.max primary_score secondary_score

type search_stats = {
  candidates: int;
  filtered: int;
}

let select_best ~normalize ~tokenize ~query ~query_tokens candidates =
  let score = jaccard_similarity query_tokens in
  List.map candidates ~f:(fun { entry; _ } ->
      let tp = tokenize (normalize entry.Imdb_data.primary_title) in
      let ts = tokenize (normalize entry.Imdb_data.secondary_title) in
      let jaccard = Float.max (score tp) (score ts) in
      entry, jaccard
    )
  |> List.sort_and_group ~compare:(fun (_, m1) (_, m2) -> Float.compare m2 m1)
  |> List.hd
  |> Option.value ~default:[]
  |> List.map ~f:(fun (elt, _) -> elt, calculate_item_score ~query elt)
  |> List.max_elt ~compare:(fun (e1, s1) (e2, s2) -> match Float.compare s1 s2, e1.Imdb_data.year, e2.year with
      | 0, Some y1, Some y2 -> Int.compare y1 y2
      | n, _, _ -> n
    )


(** Search for best matching title *)
let search t ?year ~title_types query =
  let norm_query = t.normalize query in
  let query_tokens = t.tokenize norm_query in
  let query_len = String.length query in
  let (total_candidates, candidates) = lookup t ~query_tokens in
  let candidates =
    candidates
    |> List.filter_map ~f:(fun idx ->
        let { entry; _ } as item = t.titles.(idx) in
        match matches_title_types ~title_types entry with
        | false -> None
        | true -> Some item
      )
    |> (fun v -> Option.value_map year ~default:v ~f:(fun year' ->
        List.filter v ~f:(function
            | { entry = { year = Some year; _ }; _ } -> year = year'
            | _ -> false
          )
      ))
    |> List.sort ~compare:(fun { entry = e1; _ } { entry = e2; _ } ->
        let len e = Int.min
            (Int.abs (String.length e.Imdb_data.primary_title - query_len))
            (Int.abs (String.length e.Imdb_data.secondary_title - query_len)) in
        Int.compare (len e1) (len e2))
  in
  let num_filtered = List.length candidates in
  select_best ~normalize:t.normalize ~tokenize:t.tokenize ~query ~query_tokens candidates
  |> Option.map ~f:(fun (entry, score) ->
      (entry, score, { candidates = total_candidates; filtered = num_filtered }))
