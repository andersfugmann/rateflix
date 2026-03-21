(** Token-based matching using inverted index, with edit distance scoring *)
open Base
open Bin_prot.Std

module Inverted_index = struct
  type t = (string, int array) Hashtbl.t

  let to_stdlib tbl =
    let h = Stdlib.Hashtbl.create (Hashtbl.length tbl) in
    Hashtbl.iteri tbl ~f:(fun ~key ~data -> Stdlib.Hashtbl.replace h key data);
    h

  let of_stdlib h =
    let tbl = Hashtbl.create ~size:(Stdlib.Hashtbl.length h) (module String) in
    Stdlib.Hashtbl.iter (fun key data -> Hashtbl.set tbl ~key ~data) h;
    tbl

  let bin_size_t tbl =
    bin_size_hashtbl bin_size_string (bin_size_array bin_size_int) (to_stdlib tbl)

  let bin_write_t buf ~pos tbl =
    bin_write_hashtbl bin_write_string (bin_write_array bin_write_int) buf ~pos (to_stdlib tbl)

  let bin_read_t buf ~pos_ref =
    of_stdlib (bin_read_hashtbl bin_read_string (bin_read_array bin_read_int) buf ~pos_ref)

  let __bin_read_t__ _buf ~pos_ref _n =
    Bin_prot.Common.raise_read_error (Bin_prot.Common.ReadError.Silly_type "Inverted_index.t") !pos_ref

  let bin_shape_t =
    bin_shape_hashtbl bin_shape_string (bin_shape_array bin_shape_int)

  let bin_writer_t = { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t }
  let bin_reader_t = { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
  let bin_t = { Bin_prot.Type_class.shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
end

type t = {
  titles: Imdb_data.title_entry array;
  inverted_index: Inverted_index.t;
} [@@deriving bin_io]

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
  let inverted_index = Hashtbl.map lists ~f:(fun indices ->
      Array.of_list (List.dedup_and_sort ~compare:Int.compare indices))
  in
  { titles; inverted_index }

let lookup t ~query_tokens =
  query_tokens
  |> List.dedup_and_sort ~compare:String.compare
  |> List.filter_map ~f:(fun token ->
      Hashtbl.find t.inverted_index token
      |> Option.map ~f:(fun arr -> token, Array.length arr)
    )
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> List.filter_mapi ~f:(fun i (t, c) -> match i > 0 && c > 10000 with true -> None | false -> Some t)
  |> List.filter_map ~f:(fun token -> Hashtbl.find t.inverted_index token)
  |> List.reduce ~f:(Array.merge ~compare:Int.compare)
  |> Option.value ~default:[||]
  |> fun v -> Array.length v, Array.to_list v

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

let select_best ~query ~query_tokens candidates =
  let score = jaccard_similarity query_tokens in
  List.map candidates ~f:(fun entry ->
      let tp = Normalize.tokenize (Normalize.normalize entry.Imdb_data.primary_title) in
      let ts = Normalize.tokenize (Normalize.normalize entry.Imdb_data.secondary_title) in
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
  let norm_query = Normalize.normalize query in
  let query_tokens = Normalize.tokenize norm_query in
  let query_len = String.length query in
  let (total_candidates, candidates) = lookup t ~query_tokens in
  let candidates =
    candidates
    |> List.filter_map ~f:(fun idx ->
        let entry = t.titles.(idx) in
        match matches_title_types ~title_types entry with
        | false -> None
        | true -> Some entry
      )
    |> (fun v -> Option.value_map year ~default:v ~f:(fun year' ->
        List.filter v ~f:(fun entry -> match entry.Imdb_data.year with
            | Some year -> year = year'
            | None -> false
          )
      ))
    |> List.sort ~compare:(fun e1 e2 ->
        let len e = Int.min
            (Int.abs (String.length e.Imdb_data.primary_title - query_len))
            (Int.abs (String.length e.Imdb_data.secondary_title - query_len)) in
        Int.compare (len e1) (len e2))
  in
  let num_filtered = List.length candidates in
  select_best ~query ~query_tokens candidates
  |> Option.map ~f:(fun (entry, score) ->
      (entry, score, { candidates = total_candidates; filtered = num_filtered }))
