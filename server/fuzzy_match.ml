(** Fuzzy matching using trigram indexing and Jaro-Winkler scoring *)
open Base
type t = {
  titles: Imdb_data.title_entry array;
  trigram_map: (string, int list) Hashtbl.t;
  normalize: string -> string;
}

(** Generate trigrams from a string *)
let trigrams s =
  let module M = Set.M(Int) in
  let len = String.length s in
  match len < 3 with
  | true -> [s]
  | false -> List.init (len - 2) ~f:(fun pos -> String.sub ~pos ~len:3 s)

(** Build trigram index from title entries *)
let build ~normalize titles =
  let trigram_map = Hashtbl.create ~size:100_000 (module String) in
  Array.iteri
    ~f:(fun idx { Imdb_data.primary_title; _ } ->
        normalize primary_title
        |> trigrams
        |> List.dedup_and_sort ~compare:String.compare
        |> List.iter ~f:(fun tri -> Hashtbl.add_multi trigram_map ~key:tri ~data:idx)
      ) titles;
  { titles; trigram_map; normalize }

(** Find candidate indices that share trigrams with query *)
let lookup t s =
  let counts = Hashtbl.create ~size:1000 (module Int) in
  let tris = trigrams s in
  List.iter ~f:(fun tri ->
      Hashtbl.find_multi t.trigram_map tri
      |> List.iter ~f:(fun idx -> Hashtbl.incr counts idx)
    ) tris;
  let threshold = max 1 (List.length tris / 2) in
  Hashtbl.fold ~init:[] ~f:(fun ~key:idx ~data:count acc ->
    match count >= threshold with
    | true -> idx :: acc
    | false -> acc
  ) counts

(** Jaro similarity between two strings *)
let jaro s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  (* Yet another special case???? *)
  match len1 = 0 || len2 = 0 with
  | true -> 0.0
  | false ->
      let match_distance = max 0 ((max len1 len2 / 2) - 1) in
      let s1_matches = Array.create ~len:len1 false in
      let s2_matches = Array.create ~len:len2 false in

      let rec loop ~matches = function
        | i when i >= len1 -> matches
        | i ->
          let start = max 0 (i - match_distance) in
          let end_pos = min (i + match_distance + 1) len2 in
          let rec find_match ~matches j =
            match j >= end_pos with
            | true -> matches
            | false ->
              match s2_matches.(j) || Char.equal s1.[i] s2.[j] |> not with
              | true -> find_match ~matches (j + 1)
              | false ->
                s1_matches.(i) <- true;
                s2_matches.(j) <- true;
                matches + 1
          in
          let matches = find_match ~matches start in
          loop ~matches (i + 1)
      in
      let matches = loop ~matches:0 0 in
      let transpositions =
        match matches = 0 with
        | true -> 0
        | false ->
          (* Count transpositions *)
          let rec loop ~k ~transpositions = function
            | i when i >= len1 -> transpositions
            | i ->
              let (k, transpositions) =
                match s1_matches.(i) with
                | false -> k, transpositions
                | true ->
                  let rec find_next j =
                    match s2_matches.(j) with
                    | true -> j
                    | false -> find_next (j + 1)
                  in
                  let j = find_next k in
                  let transpositions = match Char.equal s1.[i] s2.[j] with
                    | true -> transpositions
                    | false -> transpositions + 1
                  in
                  j + 1, transpositions
              in
              loop ~k ~transpositions (i - 1)
          in
          loop ~k:0 ~transpositions:0 0
      in
      let m = Float.of_int matches in
      let t = Float.of_int transpositions /. 2.0 in
      (m /. Float.of_int len1 +. m /. Float.of_int len2 +. (m -. t) /. m) /. 3.0

(** Jaro-Winkler similarity with prefix boost *)
let jaro_winkler s1 s2 =
  let jaro_sim = jaro s1 s2 in
  let prefix_len = ref 0 in
  let max_prefix = min 4 (min (String.length s1) (String.length s2)) in
  let rec count_prefix i =
    match i >= max_prefix with
    | true -> ()
    | false ->
        match Char.equal s1.[i] s2.[i] with
        | true -> prefix_len := i + 1; count_prefix (i + 1)
        | false -> ()
  in
  count_prefix 0;
  jaro_sim +. (Float.of_int !prefix_len *. 0.1 *. (1.0 -. jaro_sim))

(** Apply year scoring - penalize mismatches when year is specified *)
let score_with_year ~query_year ~title_year base_score =
  match query_year, title_year with
  | Some qy, Some ty ->
      (match qy = ty with
       | true -> base_score +. 0.1  (* Exact year match bonus *)
       | false ->
           let diff = abs (qy - ty) in
           match diff <= 1 with
           | true -> base_score +. 0.05  (* Off by one year - small bonus *)
           | false -> base_score -. (Float.of_int (min diff 20) *. 0.01))  (* Penalty for year mismatch *)
  | Some _, None -> base_score -. 0.1  (* Slight penalty if query has year but title doesn't *)
  | _ -> base_score

(** Check if title type matches filter *)
let matches_title_types ~title_types entry =
  match title_types with
  | None -> true
  | Some types -> List.mem ~equal:String.equal types entry.Imdb_data.title_type

(** Search for best matching title *)
let search t ~query ~year ~title_types =
  let norm_query = t.normalize query in
  lookup t norm_query
  |> List.map ~f:(fun idx -> t.titles.(idx))
  |> List.filter ~f:(matches_title_types ~title_types)
  |> List.map ~f:(fun entry -> entry, t.normalize entry.Imdb_data.primary_title)
  |> List.map ~f:(fun (entry, norm_title) ->
      let base_score = jaro_winkler norm_query norm_title in
      let score = score_with_year ~query_year:year ~title_year:entry.Imdb_data.year base_score in
      (entry, score)
    )
  |> List.fold_left ~init:None ~f:(fun best (entry, score) ->
      match best with
      | Some (_, best_score) when Float.(best_score > score) -> best
      | _ -> Some (entry, score)
    )
