(** Token-based matching using inverted index, with edit distance scoring *)
open Base
open Bin_prot.Std

type t = {
  titles: Imdb_data.title_entry array;
  inverted_index: (string * int array) array;
} [@@deriving bin_io]

(** Binary search for a key in the sorted inverted index *)
let find_token t token =
  Array.binary_search t.inverted_index ~compare:(fun (k, _) token -> String.compare k token)
    `First_equal_to token
  |> Option.map ~f:(fun i -> snd t.inverted_index.(i))

(** Build inverted index from title entries *)
let build titles =
  let lists = Hashtbl.create ~size:100_000 (module String) in
  Array.iteri titles ~f:(fun idx entry ->
      let norm1 = Normalize.normalize entry.Imdb_data.primary_title in
      let norm2 = Normalize.normalize entry.Imdb_data.secondary_title in
      let tokens =
        List.dedup_and_sort ~compare:String.compare
          (Normalize.tokenize norm1 @ Normalize.tokenize norm2)
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
  |> List.filter_mapi ~f:(fun i (t, c) -> match i > 0 && c > 10000 with true -> None | false -> Some t)
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
let calculate_score ?max_edits ~query title =
  let distance = Normalize.weighted_edit_distance ~cost:Normalize.substitution_cost ?max_edits query title in
  distance

let calculate_item_score ~query ?max_edits { Imdb_data.primary_title; secondary_title; _ } =
  let primary_score = calculate_score ?max_edits ~query primary_title in
  let secondary_score = calculate_score ?max_edits ~query secondary_title in
  Float.max primary_score secondary_score

type search_stats = {
  candidates: int;
  filtered: int;
}

let select_best ~query ~query_tokens candidates =
  let score = jaccard_similarity query_tokens in
  let weighted_edit_distance =
    let query_uchars = Normalize.to_uchars query in
    let cost = Normalize.substitution_cost in
    fun ?max_edits s ->
      Normalize.weighted_edit_distance_uchars ~cost ?max_edits query_uchars (Normalize.to_uchars s)
  in
  (* We need to split up into primary and secondary *)
  candidates
  |> List.concat_map ~f:(fun ({ Imdb_data.primary_title; secondary_title; _ } as entry) ->
      (* And drop secondary if they are equal *)
      let res = [primary_title, entry] in
      match String.equal primary_title secondary_title with
      | true -> res
      | false ->
        (secondary_title, entry) :: res
    )
  |> List.map ~f:(fun (title, entry) ->
      let tokens = Normalize.tokenize (Normalize.normalize title) in
      (score tokens, title, entry)
    )
  |> List.sort_and_group ~compare:(fun (m1, _, _) (m2, _, _) -> Float.compare m2 m1)
  |> List.hd
  |> Option.value ~default:[]
  |> List.map ~f:(fun (_score, title, entry) -> (title, entry))
  |> List.fold ~init:(1.0, None) ~f:(fun (score, best) (title, elt) ->
      let max_length = Int.max (String.length query) (String.length title) |> Float.of_int in
      let max_edits = match score with
        | 0.0 -> Float.max_value
        | n -> n *. max_length
      in
      let distance = weighted_edit_distance ~max_edits title in
      (* Stdlib.Printf.printf "%.3f: tt%07d %s\n" distance elt.Imdb_data.tconst title; *)
      let score' = distance /. max_length in
      match Float.compare score score' with
      | -1 -> (score, best)
      | 1 -> (score', Some elt)
      | _ (* 0 *) ->
        match best, elt with
        | Some { Imdb_data.year = Some year; _}, { Imdb_data.year = Some year'; _} when year' > year ->
          (score, Some elt)
        | _ -> (score, best)
    )
  |> function
  | (_, None) -> None
  | (score, Some elt) ->
    Some (elt, (1.0 -. score))


(** Search for best matching title *)
let search t ?year ~title_types query =
  let norm_query = Normalize.normalize query in
  let query_tokens = Normalize.tokenize norm_query in
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
  select_best ~query ~query_tokens candidates
  |> Option.map ~f:(fun (entry, score) ->
      (entry, score, { candidates = total_candidates; filtered = num_filtered }))
