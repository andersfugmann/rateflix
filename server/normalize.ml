open Base
open Stdio

(** Roman numeral to arabic conversion (uppercase only, I-X) *)
let roman_to_arabic = function
  | "I" -> Some "1" | "II" -> Some "2" | "III" -> Some "3"
  | "IV" -> Some "4" | "V" -> Some "5" | "VI" -> Some "6"
  | "VII" -> Some "7" | "VIII" -> Some "8" | "IX" -> Some "9"
  | "X" -> Some "10" | _ -> None

(** Split a NFKD-decomposed string into a sequence of words,
    where each word is a Uchar.t list. Only letters and digits
    are kept; spaces act as word separators; all other characters are dropped. *)
let split_words s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let space = Uchar.of_char ' ' in
  let chars =
    let open Sequence.Generator in
    let (let*) = (>>=) in
    let rec inner () =
      match Uutf.decode decoder with
      | `Uchar u ->
        begin match Uucp.Gc.general_category u with
        | `Lu | `Ll | `Lt | `Lm | `Lo | `Nd ->
          let* () = yield u in inner ()
        | `Zs ->
          let* () = yield space in inner ()
        | _ -> inner ()
        end
      | `End -> return ()
      | `Malformed _ -> inner ()
      | `Await -> return ()
    in
    run (inner ())
  in
  chars
  |> Sequence.group ~break:(fun a b -> Uchar.equal a space || Uchar.equal b space)
  |> Sequence.filter ~f:(function | [ a ] -> not (Uchar.equal a space) | _ -> true)


let uchar_to_ascii c =
  match Uchar.to_scalar c with
  | c when (c >= 0x41 && c <= 0x5A) || (c >= 0x61 && c <= 0x7A) || (c >= 0x30 && c <= 0x39) -> Some (Char.of_int_exn c)
  | _ -> None

(** Full normalization pipeline: NFKD → split on Zs → replace roman numerals → ASCII lowercase *)
let normalize s =
  let decomposed = Uunf_string.normalize_utf_8 `NFKD s in
  split_words decomposed
  |> Sequence.filter_map ~f:(fun word_uchars ->
      word_uchars
      |> List.filter_map ~f:uchar_to_ascii
      |> String.of_char_list
      |> (fun word ->
          match roman_to_arabic word with
          | Some arabic -> arabic
          | None -> word
        )
      |> function
      | "" -> None
      | word -> Some (String.lowercase word)
    )
  |> Sequence.to_list


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


(* Expect tests *)
let%expect_test "normalize: Extended chars" =
  print_endline (normalize "æøå€$" |> String.concat ~sep:" ");
  [%expect {| a |}]

let%expect_test "normalize: uppercase" =
  print_endline (normalize "THE GODFATHER" |> String.concat ~sep:" ");
  [%expect {| the godfather |}]

let%expect_test "normalize: accented characters" =
  print_endline (normalize "Amélie" |> String.concat ~sep:" ");
  [%expect {| amelie |}]

let%expect_test "normalize: multiple accents" =
  print_endline (normalize "Léon: The Professional" |> String.concat ~sep:" ");
  [%expect {| leon the professional |}]

let%expect_test "normalize: diaeresis" =
  print_endline (normalize "Nausicaä" |> String.concat ~sep:" ");
  [%expect {| nausicaa |}]

let%expect_test "normalize: circumflex" =
  print_endline (normalize "Hôtel Rwanda" |> String.concat ~sep:" ");
  [%expect {| hotel rwanda |}]

let%expect_test "normalize: punctuation removed" =
  print_endline (normalize "Who's Afraid of Virginia Woolf?" |> String.concat ~sep:" ");
  [%expect {| whos afraid of virginia woolf |}]

let%expect_test "normalize: apostrophe in name" =
  print_endline (normalize "Schindler's List" |> String.concat ~sep:" ");
  [%expect {| schindlers list |}]

let%expect_test "normalize: numbers kept" =
  print_endline (normalize "2001: A Space Odyssey" |> String.concat ~sep:" ");
  [%expect {| 2001 a space odyssey |}]


let%expect_test "normalize: Greek letters (non-ASCII)" =
  (* Greek 'Ω' (U+03A9) lowercases to 'ω' (U+03C9) which is > 255 *)
  print_endline (normalize "Ωmega" |> String.concat ~sep:" ");
  [%expect {| mega |}]

let ed s1 s2 = weighted_edit_distance_uchars ~cost:substitution_cost (to_uchars s1) (to_uchars s2)

let%expect_test "edit_distance: identical strings" =
  printf "%.1f" (ed "hello" "hello");
  [%expect {| 0.0 |}]

let%expect_test "edit_distance: case only" =
  printf "%.1f" (ed "Hello" "hello");
  [%expect {| 0.1 |}]

let%expect_test "edit_distance: all caps" =
  printf "%.1f" (ed "THE" "the");
  [%expect {| 0.3 |}]

let%expect_test "edit_distance: one char substitution" =
  printf "%.1f" (ed "cat" "bat");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: insertion" =
  printf "%.1f" (ed "helo" "hello");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: accent difference" =
  printf "%.1f" (ed "Amelie" "Amélie");
  [%expect {| 1.0 |}]

let%expect_test "edit_distance: case + accent" =
  printf "%.1f" (ed "amelie" "Amélie");
  [%expect {| 1.1 |}]

let%expect_test "edit_distance: max_edits short-circuit" =
  printf "%.1f" (weighted_edit_distance_uchars ~cost:substitution_cost ~max_edits:0.5 (to_uchars "cat") (to_uchars "bat"));
  [%expect {| inf |}]

let%expect_test "edit_distance: max_edits allows case change" =
  printf "%.1f" (weighted_edit_distance_uchars ~cost:substitution_cost ~max_edits:0.5 (to_uchars "Cat") (to_uchars "cat"));
  [%expect {| 0.1 |}]

let%expect_test "edit_distance: max_edits exact match" =
  printf "%.1f" (weighted_edit_distance_uchars ~cost:substitution_cost ~max_edits:0.0 (to_uchars "hello") (to_uchars "hello"));
  [%expect {| 0.0 |}]

let%expect_test "normalize: roman numeral Ⅲ (unicode)" =
  print_endline (normalize "Movie Ⅲ" |> String.concat ~sep:" ");
  [%expect {| movie 3 |}]

let%expect_test "normalize: uppercase roman III" =
  print_endline (normalize "Frozen III" |> String.concat ~sep:" ");
  [%expect {| frozen 3 |}]

let%expect_test "normalize: lowercase roman iii not replaced" =
  print_endline (normalize "frozen iii" |> String.concat ~sep:" ");
  [%expect {| frozen iii |}]

let%expect_test "normalize: roman IV in context" =
  print_endline (normalize "Star Wars Episode IV" |> String.concat ~sep:" ");
  [%expect {| star wars episode 4 |}]
