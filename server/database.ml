(** Token-based matching using inverted index, with edit distance scoring *)
open Base
open Bin_prot.Std

type t = {
  titles: Imdb_data.title_entry array;
  inverted_index: (string * int array) array;
} [@@deriving bin_io]

type search_stats = {
  candidates: int;
  filtered: int;
}

(** Binary search for a key in the sorted inverted index *)
let find_token t token =
  Array.binary_search t.inverted_index ~compare:(fun (k, _) token -> String.compare k token)
    `First_equal_to token
  |> Option.map ~f:(fun i -> snd t.inverted_index.(i))

(** Build inverted index from title entries *)
let build titles =
  let lists = Hashtbl.create ~size:100_000 (module String) in
  Array.iteri titles ~f:(fun idx entry ->

      let tokens =
        let t1 = Normalize.normalize entry.Imdb_data.primary_title in
        let t2 = Normalize.normalize entry.Imdb_data.secondary_title in
        List.dedup_and_sort ~compare:String.compare (t1 @ t2)
      in
      List.iter tokens ~f:(fun token ->
          Hashtbl.add_multi lists ~key:token ~data:idx)
    );
  let inverted_index =
    Hashtbl.fold lists ~init:[] ~f:(fun ~key ~data acc ->
      (key, Array.of_list (List.dedup_and_sort ~compare:Int.compare data)) :: acc)
    |> Array.of_list
    |> Array.sorted_copy ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
  in
  { titles; inverted_index }

let lookup t ~query_tokens =
  query_tokens
  |> List.dedup_and_sort ~compare:String.compare
  |> List.filter_map ~f:(fun token ->
      find_token t token
      |> Option.map ~f:(fun arr -> token, Array.length arr)
    )
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> List.drop_while ~f:(function | (_, 0) -> true | _ -> false)
  |> List.filteri ~f:(fun i (_, c) -> i < 1 || c < 10000)
  |> List.map ~f:fst
  |> List.filter_map ~f:(fun token -> find_token t token)
  |> List.reduce ~f:(Array.merge ~compare:Int.compare)
  |> Option.value_map ~default:[] ~f:Array.to_list
  |> List.remove_consecutive_duplicates ~equal:Int.equal


(** Check if title type matches filter *)
let matches_title_types ~title_types entry =
  match title_types with
  | None -> true
  | Some types -> List.mem ~equal:Poly.equal types entry.Imdb_data.title_type

let matches_year ~year entry =
  match year with
  | None -> true
  | Some year ->
    match entry.Imdb_data.year with
    | Some year' -> year = year'
    | None -> true

(** IDF weight for a token: log(N / df). Returns 1.0 for unknown tokens. *)
let idf_weight t token =
  match find_token t token with
  | None -> 1.0
  | Some posting_list ->
    let n = Float.of_int (Array.length t.titles) in
    let df = Float.of_int (Array.length posting_list) in
    Float.log (n /. df)

(** IDF-weighted recall: sum of IDF weights for matching tokens / sum for query tokens *)
let weighted_recall t xs =
  let set_x = Set.of_list (module String) xs in
  let idf = idf_weight t in
  let sum_x = Set.sum (module Float) set_x ~f:idf in
  fun ys ->
    let set_y = Set.of_list (module String) ys in
    let inter_set = Set.inter set_x set_y in
    let sum_inter = Set.sum (module Float) inter_set ~f:idf in
    sum_inter /. sum_x

let select_best t ~query ~query_tokens candidates =
  let recall = weighted_recall t query_tokens in
  let weighted_edit_distance =
    let query_uchars = Normalize.to_uchars query in
    fun ?max_edits s ->
      Normalize.weighted_edit_distance_uchars ?max_edits query_uchars (Normalize.to_uchars s)
  in
  candidates
  |> List.concat_map ~f:(fun ({ Imdb_data.primary_title; secondary_title; _ } as entry) ->
      let res = [primary_title, entry] in
      match String.equal primary_title secondary_title with
      | true -> res
      | false ->
        (secondary_title, entry) :: res
    )
  |> List.map ~f:(fun (title, entry) ->
      let tokens = Normalize.normalize title in
      (recall tokens, title, entry)
    )
  |> List.sort ~compare:(fun (s1, _, _) (s2, _, _) -> Float.compare s2 s1)
  |> (fun candidates ->
      match candidates with
      | [] -> []
      | (best_score, _, _) :: _ ->
        List.take_while candidates ~f:(fun (s, _, _) -> Float.equal s best_score)
    )
  |> List.dedup_and_sort ~compare:(fun (_, _, e1) (_, _, e2) -> Int.compare e1.Imdb_data.tconst e2.Imdb_data.tconst)
  |> List.map ~f:(fun (_score, title, entry) -> (title, entry))
  |> List.fold ~init:(Int.max_value, None) ~f:(fun (max_edits, best) (title, elt) ->
      let distance = weighted_edit_distance ~max_edits title in
      match Int.compare max_edits distance with
      | -1 -> (max_edits, best)
      | 1 -> (distance, Some (elt, title))
      | _ (* 0 *) ->
        let best =
          match best, elt with
          | Some ({ Imdb_data.year = Some year; _}, _), { Imdb_data.year = Some year'; _} when year' > year -> Some (elt, title)
          | _ -> best
        in
        (max_edits, best)
    )
  |> function
  | (_, None) -> None
  | (distance, Some (elt, title)) ->
    let max_length = Int.max (String.length query) (String.length title) |> Float.of_int in
    let score = 1.0 -. (Float.of_int distance /. Normalize.edit_distance_scale /. max_length) in
    Some (elt, Float.max 0.0 score)


(** Search for best matching title *)
let search t ?year ~title_types query =
  let query_tokens =
    Normalize.normalize query
    |> List.dedup_and_sort ~compare:String.compare
  in
  let candidates = lookup t ~query_tokens in
  let total_candidates = List.length candidates in
  let candidates =
    candidates
    |> List.filter_map ~f:(fun idx ->
        let entry = t.titles.(idx) in
        match matches_title_types ~title_types entry && matches_year ~year entry with
        | false -> None
        | true -> Some entry
      )
  in
  let num_filtered = List.length candidates in
  select_best t ~query ~query_tokens candidates
  |> Option.map ~f:(fun (entry, score) ->
      (entry, score, { candidates = total_candidates; filtered = num_filtered }))
