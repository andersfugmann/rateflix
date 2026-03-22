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
  let distance = Normalize.weighted_edit_distance ?max_edits query title in
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
