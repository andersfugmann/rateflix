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
  |> (fun t ->
      match List.exists query_tokens ~f:(String.Caseless.equal "m") || true  with
      | true -> Stdlib.Printf.printf "Query: [%s]: [%s]\n" (String.concat ~sep:"; " query_tokens)(String.concat ~sep:"; " t); t
      | false -> t
    )
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

let jaccard_similarities xs =
  let set_x = Set.of_list (module String) xs in
  let length_x = Set.length set_x in
  fun ys ->
    let set_y = Set.of_list (module String) ys in
    let length_y = Set.length set_y in
    let max_length = Int.max length_x length_y in
    let shared = Set.inter set_x set_y |> Set.length in
    let union = Set.union set_x set_y |> Set.length in
    let j1 = Float.(of_int shared / of_int union) in
    let j2 = Float.(of_int shared / of_int max_length) in
    let j3 = Float.(of_int shared / of_int length_x) in
    let j4 = Float.(of_int shared / of_int length_y) in
    [| j1; j2; j3; j4 |]


(** Calculate normalized score: 1.0 = perfect match, 0.0 = completely different.
    Case differences cost 0.1 per char, other edits cost 1.0. *)

type search_stats = {
  candidates: int;
  filtered: int;
}

let jaccard_similarities_idx = [0; 2; 3]

let select_best ~query ~query_tokens candidates =
  let jaccard_similarities = jaccard_similarities query_tokens in
  let weighted_edit_distance =
    let query_uchars = Normalize.to_uchars query in
    fun ?max_edits s ->
      Normalize.weighted_edit_distance_uchars ?max_edits query_uchars (Normalize.to_uchars s)
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
      let tokens = Normalize.normalize title in
      (jaccard_similarities tokens, title, entry)
    )
  |> (fun candidates ->
      jaccard_similarities_idx
      |> List.concat_map ~f:(fun idx ->
          List.sort_and_group candidates ~compare:(fun (s1, _, _) (s2, _, _) -> Float.compare (Array.get s2 idx) (Array.get s1 idx))
          |> List.hd
          |> Option.value ~default:[]
        )
    )
  |> List.dedup_and_sort ~compare:(fun (_, _, e1) (_, _, e2) -> Int.compare e1.Imdb_data.tconst e2.Imdb_data.tconst)
  |> List.map ~f:(fun (_score, title, entry) -> (title, entry))
  |> List.fold ~init:(1.0, None) ~f:(fun (score, best) (title, elt) ->
      let max_length = Int.max (String.length query) (String.length title) |> Float.of_int in
      let max_edits = match score with
        | 0.0 -> Int.max_value
        | n -> Float.to_int (Float.round_up (n *. max_length *. Normalize.edit_distance_scale))
      in
      let distance = Float.of_int (weighted_edit_distance ~max_edits title) /. Normalize.edit_distance_scale in
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
  select_best ~query ~query_tokens candidates
  |> Option.map ~f:(fun (entry, score) ->
      (entry, score, { candidates = total_candidates; filtered = num_filtered }))
