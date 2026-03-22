open Base
open Stdio

(** Roman numeral to arabic conversion (uppercase only, I-X) *)
let roman_to_arabic = function
  | "I" -> Some "1" | "II" -> Some "2" | "III" -> Some "3"
  | "IV" -> Some "4" | "V" -> Some "5" | "VI" -> Some "6"
  | "VII" -> Some "7" | "VIII" -> Some "8" | "IX" -> Some "9"
  | "X" -> Some "10" | _ -> None

(** Split a NFKD-decomposed string into words on Unicode space separators (Zs). *)
let split_words s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create 64 in
  let rec collect acc =
    match Uutf.decode decoder with
    | `Uchar u ->
      begin match Uucp.Gc.general_category u with
      | `Zs ->
        let word = Buffer.contents buf in
        Buffer.clear buf;
        collect (match word with "" -> acc | _ -> word :: acc)
      | _ ->
        Uutf.Buffer.add_utf_8 buf u;
        collect acc
      end
    | `End ->
      let word = Buffer.contents buf in
      List.rev (match word with "" -> acc | _ -> word :: acc)
    | `Malformed _ -> collect acc
    | `Await -> List.rev acc
  in
  collect []

(** Reduce a word to lowercase ASCII letters and digits only *)
let to_ascii word =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String word) in
  let char_sequence =
    let open Sequence.Generator in
    let (let*) = (>>=) in
    let rec inner () =
      match Uutf.decode decoder with
      | `Uchar u ->
        begin match Uucp.Gc.general_category u with
        | `Ll | `Lu | `Nd ->
          let code = Uchar.to_scalar u in
          let* () = match code with
            | c when c >= 0x41 && c <= 0x5A -> yield (Char.lowercase (Char.of_int_exn c))
            | c when c >= 0x61 && c <= 0x7A -> yield (Char.of_int_exn c)
            | c when c >= 0x30 && c <= 0x39 -> yield (Char.of_int_exn c)
            | _ -> return ()
          in
          inner ()
        | _ -> inner ()
        end
      | `End -> return ()
      | `Malformed _ -> inner ()
      | `Await -> return ()
    in
    run (inner ())
  in
  String.of_sequence char_sequence

(** Full normalization pipeline: NFKD → split on Zs → replace roman numerals → ASCII lowercase *)
let normalize s =
  let decomposed = Uunf_string.normalize_utf_8 `NFKD s in
  split_words decomposed
  |> List.filter_map ~f:(fun word ->
      let word = match roman_to_arabic word with
        | Some arabic -> arabic
        | None -> word
      in
      let ascii = to_ascii word in
      match ascii with "" -> None | _ -> Some ascii)
  |> String.concat ~sep:" "

(** Tokenize a normalized string into words *)
let tokenize s =
  String.split ~on:' ' s
  |> List.filter ~f:(fun w -> String.length w > 0)

(** Decode a UTF-8 string into an array of unicode codepoints *)
let to_uchars s =
  let decomposed = Uunf_string.normalize_utf_8 `NFKD s in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String decomposed) in
  let rec collect acc =
    match Uutf.decode decoder with
    | `Uchar u -> collect (u :: acc)
    | `End -> Array.of_list (List.rev acc)
    | `Malformed _ -> collect (Uchar.replacement_char :: acc) (* Bit unsure *)
    | `Await -> Array.of_list (List.rev acc)
  in
  collect []

let equal_caseless a b =
  match Uchar.to_char a with
  | None -> false
  | Some a ->
    match Uchar.to_char b with
    | None -> false
    | Some b -> Char.Caseless.equal a b

(** Substitution cost: 0.0 for identical, 0.1 for case-only difference, 1.0 otherwise *)
let substitution_cost a b =
  match Uchar.equal a b with
  | true -> 0.0
  | false when equal_caseless a b -> 0.1
  | false -> 1.0

(** Weighted Levenshtein distance using two mutable arrays.
    If ~max_edits is provided, returns infinity when distance is known to exceed it.
    Accepts pre-decoded uchar arrays to avoid repeated UTF-8 decoding. *)
let weighted_edit_distance_uchars ~cost ?max_edits a b =
  let m = Array.length a in
  let n = Array.length b in
  match max_edits with
  | Some max_edits when Float.(of_int Int.(abs (m - n)) > max_edits) -> Float.infinity
  | _ ->
    let prev = Array.init (n + 1) ~f:Float.of_int in
    let curr = Array.create ~len:(n + 1) 0.0 in
    let exception Early_exit in
    try
      for i = 0 to m - 1 do
        curr.(0) <- Float.of_int (i + 1);
        let row_min = ref curr.(0) in
        for j = 0 to n - 1 do
          let sub_cost = cost a.(i) b.(j) in
          let v = Float.min
            (Float.min (prev.(j + 1) +. 1.0) (curr.(j) +. 1.0))
            (prev.(j) +. sub_cost) in
          curr.(j + 1) <- v;
          if Float.(v < !row_min) then row_min := v
        done;
        (match max_edits with
         | Some max_edits when Float.(!row_min > max_edits) -> raise Early_exit
         | _ -> ());
        Array.blit ~src:curr ~src_pos:0 ~dst:prev ~dst_pos:0 ~len:(n + 1)
      done;
      prev.(n)
    with Early_exit -> Float.infinity

let weighted_edit_distance ~cost ?max_edits s1 s2 =
  weighted_edit_distance_uchars ~cost ?max_edits (to_uchars s1) (to_uchars s2)

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
  tokenize "the matrix" |> String.concat ~sep:", " |> print_endline;
  [%expect {| the, matrix |}]

let%expect_test "tokenize: multiple spaces" =
  tokenize "the   matrix" |> String.concat ~sep:", " |> print_endline;
  [%expect {| the, matrix |}]

let%expect_test "tokenize: single word" =
  tokenize "amelie" |> String.concat ~sep:", " |> print_endline;
  [%expect {| amelie |}]

let%expect_test "tokenize: empty string" =
  tokenize "" |> String.concat ~sep:", " |> print_endline;
  [%expect {| |}]

let%expect_test "normalize: Greek letters (non-ASCII)" =
  (* Greek 'Ω' (U+03A9) lowercases to 'ω' (U+03C9) which is > 255 *)
  print_endline (normalize "Ωmega");
  [%expect {| mega |}]

let%expect_test "edit_distance: identical strings" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "hello" "hello");
  [%expect {| 0.0 |}]

let%expect_test "edit_distance: case only" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "Hello" "hello");
  [%expect {| 0.1 |}]

let%expect_test "edit_distance: all caps" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "THE" "the");
  [%expect {| 0.3 |}]

let%expect_test "edit_distance: one char substitution" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "cat" "bat");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: insertion" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "helo" "hello");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: accent difference" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "Amelie" "Amélie");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: case + accent" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost "amelie" "Amélie");
  [%expect {| 1.1 |}]

let%expect_test "edit_distance: max_edits short-circuit" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost ~max_edits:0.5 "cat" "bat");
  [%expect {| inf |}]

let%expect_test "edit_distance: max_edits allows case change" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost ~max_edits:0.5 "Cat" "cat");
  [%expect {| 0.1 |}]

let%expect_test "edit_distance: max_edits exact match" =
  printf "%.1f" (weighted_edit_distance ~cost:substitution_cost ~max_edits:0.0 "hello" "hello");
  [%expect {| 0.0 |}]

let%expect_test "normalize: roman numeral Ⅲ (unicode)" =
  print_endline (normalize "Movie Ⅲ");
  [%expect {| movie 3 |}]

let%expect_test "normalize: uppercase roman III" =
  print_endline (normalize "Frozen III");
  [%expect {| frozen 3 |}]

let%expect_test "normalize: lowercase roman iii not replaced" =
  print_endline (normalize "frozen iii");
  [%expect {| frozen iii |}]

let%expect_test "normalize: roman IV in context" =
  print_endline (normalize "Star Wars Episode IV");
  [%expect {| star wars episode 4 |}]
