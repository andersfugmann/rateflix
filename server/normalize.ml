(** Unicode title normalization and tokenization for matching *)

(** Case fold a UTF-8 string using Unicode case folding *)
let case_fold title =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String title) in
  let buf = Buffer.create (String.length title) in
  let rec inner () =
    match Uutf.decode decoder with
    | `Uchar u ->
        (match Uucp.Case.Fold.fold u with
         | `Self -> Uutf.Buffer.add_utf_8 buf u
         | `Uchars cs -> List.iter (Uutf.Buffer.add_utf_8 buf) cs);
        inner ()
    | `End -> Buffer.contents buf
    | `Malformed _ -> inner ()
    | `Await -> Buffer.contents buf
  in
  inner ()

(** Normalize title to lowercase ASCII, keeping only letters, digits and spaces *)
let normalize title =
  (* NFD decomposition separates base chars from combining diacritics *)
  let decomposed = Uunf_string.normalize_utf_8 `NFD title in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String decomposed) in
  let rec inner () =
    match Uutf.decode decoder with
    | `Uchar u ->
      begin
        match Uucp.Gc.general_category u with
        | `Zs -> Seq.Cons (' ', inner)
        | `Ll | `Lu | `Nd ->
          (* Lowercase and keep only ASCII alphanumeric *)
          let chars =
            match Uucp.Case.Map.to_lower u with
            | `Self -> Seq.singleton u
            | `Uchars cs -> List.to_seq cs
          in
          let seq =
            Seq.filter_map (function
                | c when Uchar.is_char c ->
                  begin
                    match Uchar.to_char c with
                    | ('a' .. 'z' | '0' .. '9') as ch -> Some ch
                    | _ -> None
                  end
                | _ -> None
              ) chars
          in
          (Seq.append seq inner) ()
        | _ -> inner ()
      end
    | `End -> Seq.Nil
    | `Malformed _ -> inner ()
    | `Await -> Seq.Nil
  in
  String.of_seq inner
  |> String.trim

(** Tokenize a normalized string into words *)
let tokenize s =
  String.split_on_char ' ' s
  |> List.filter (fun w -> String.length w > 0)

(** Decode a UTF-8 string into an array of unicode codepoints *)
let to_uchars s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec collect acc =
    match Uutf.decode decoder with
    | `Uchar u -> collect (u :: acc)
    | `End -> Array.of_list (List.rev acc)
    | `Malformed _ -> collect (Uchar.rep :: acc)
    | `Await -> Array.of_list (List.rev acc)
  in
  collect []

(** Case fold a single uchar for comparison *)
let case_fold_uchar u =
  match Uucp.Case.Fold.fold u with
  | `Self -> [u]
  | `Uchars cs -> cs

(** Check if two uchars are case-equivalent *)
let is_case_equivalent a b =
  case_fold_uchar a = case_fold_uchar b

(** Substitution cost: 0.0 for identical, 0.1 for case-only difference, 1.0 otherwise *)
let substitution_cost a b =
  match a,b with
  | a,b when Uchar.equal a b -> 0.0
  | a,b when is_case_equivalent a b -> 0.1
  | _ -> 1.0

(** Weighted Levenshtein distance using two mutable arrays.
    If ~max_edits is provided, returns infinity when distance is known to exceed it. *)
let edit_distance ~cost ?max_edits s1 s2 =
  let a = to_uchars s1 in
  let b = to_uchars s2 in
  let m = Array.length a in
  let n = Array.length b in
  let prev = Array.init (n + 1) Float.of_int in
  let curr = Array.make (n + 1) 0.0 in
  let exceeds_max row = match max_edits with
    | None -> false
    | Some max -> Array.for_all (fun v -> v > max) row
  in
  let exception Early_exit in
  try
    for i = 0 to m - 1 do
      curr.(0) <- Float.of_int (i + 1);
      for j = 0 to n - 1 do
        let sub_cost = cost a.(i) b.(j) in
        curr.(j + 1) <- Float.min
          (Float.min (prev.(j + 1) +. 1.0) (curr.(j) +. 1.0))
          (prev.(j) +. sub_cost)
      done;
      if exceeds_max curr then raise Early_exit;
      Array.blit curr 0 prev 0 (n + 1)
    done;
    prev.(n)
  with Early_exit -> infinity

let weighted_edit_distance ?max_edits s1 s2 =
  edit_distance ~cost:substitution_cost ?max_edits s1 s2

(* Expect tests *)
let%expect_test "normalize: Extended chars" =
  print_endline (normalize "æøå€$");
  [%expect {| a |}]

let%expect_test "normalize: uppercase" =
  print_endline (normalize "THE GODFATHER");
  [%expect {| the godfather |}]

let%expect_test "normalize: accented characters" =
  print_endline (normalize "Amélie");
  [%expect {| amelie |}]

let%expect_test "normalize: multiple accents" =
  print_endline (normalize "Léon: The Professional");
  [%expect {| leon the professional |}]

let%expect_test "normalize: diaeresis" =
  print_endline (normalize "Nausicaä");
  [%expect {| nausicaa |}]

let%expect_test "normalize: circumflex" =
  print_endline (normalize "Hôtel Rwanda");
  [%expect {| hotel rwanda |}]

let%expect_test "normalize: punctuation removed" =
  print_endline (normalize "Who's Afraid of Virginia Woolf?");
  [%expect {| whos afraid of virginia woolf |}]

let%expect_test "normalize: apostrophe in name" =
  print_endline (normalize "Schindler's List");
  [%expect {| schindlers list |}]

let%expect_test "normalize: numbers kept" =
  print_endline (normalize "2001: A Space Odyssey");
  [%expect {| 2001 a space odyssey |}]

let%expect_test "tokenize: basic" =
  tokenize "the matrix" |> String.concat ", " |> print_endline;
  [%expect {| the, matrix |}]

let%expect_test "tokenize: multiple spaces" =
  tokenize "the   matrix" |> String.concat ", " |> print_endline;
  [%expect {| the, matrix |}]

let%expect_test "tokenize: single word" =
  tokenize "amelie" |> String.concat ", " |> print_endline;
  [%expect {| amelie |}]

let%expect_test "tokenize: empty string" =
  tokenize "" |> String.concat ", " |> print_endline;
  [%expect {| |}]

let%expect_test "case_fold: uppercase" =
  print_endline (case_fold "THE GODFATHER");
  [%expect {| the godfather |}]

let%expect_test "case_fold: preserves accents" =
  print_endline (case_fold "Amélie");
  [%expect {| amélie |}]

let%expect_test "case_fold: preserves punctuation" =
  print_endline (case_fold "Schindler's List");
  [%expect {| schindler's list |}]

let%expect_test "case_fold: german sharp s" =
  print_endline (case_fold "STRASSE");
  [%expect {| strasse |}]

let%expect_test "normalize: Greek letters (non-ASCII)" =
  (* Greek 'Ω' (U+03A9) lowercases to 'ω' (U+03C9) which is > 255 *)
  print_endline (normalize "Ωmega");
  [%expect {| mega |}]

let%expect_test "edit_distance: identical strings" =
  Printf.printf "%.1f" (weighted_edit_distance "hello" "hello");
  [%expect {| 0.0 |}]

let%expect_test "edit_distance: case only" =
  Printf.printf "%.1f" (weighted_edit_distance "Hello" "hello");
  [%expect {| 0.1 |}]

let%expect_test "edit_distance: all caps" =
  Printf.printf "%.1f" (weighted_edit_distance "THE" "the");
  [%expect {| 0.3 |}]

let%expect_test "edit_distance: one char substitution" =
  Printf.printf "%.1f" (weighted_edit_distance "cat" "bat");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: insertion" =
  Printf.printf "%.1f" (weighted_edit_distance "helo" "hello");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: accent difference" =
  Printf.printf "%.1f" (weighted_edit_distance "Amelie" "Amélie");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: case + accent" =
  Printf.printf "%.1f" (weighted_edit_distance "amelie" "Amélie");
  [%expect {| 1.1 |}]

let%expect_test "edit_distance: max_edits short-circuit" =
  Printf.printf "%.1f" (weighted_edit_distance ~max_edits:0.5 "cat" "bat");
  [%expect {| inf |}]

let%expect_test "edit_distance: max_edits allows case change" =
  Printf.printf "%.1f" (weighted_edit_distance ~max_edits:0.5 "Cat" "cat");
  [%expect {| 0.1 |}]

let%expect_test "edit_distance: max_edits exact match" =
  Printf.printf "%.1f" (weighted_edit_distance ~max_edits:0.0 "hello" "hello");
  [%expect {| 0.0 |}]
