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


let edit_distance_scale = 10.0

(** Weighted Levenshtein distance (×10 scaled integer) with early exit.
    When ~max_edits is provided, returns Int.max_value if minimum row cost exceeds it.
    Accepts pre-decoded uchar arrays to avoid repeated UTF-8 decoding. *)
let weighted_edit_distance_uchars ?(max_edits=Int.max_value) a b =
  let exception Early_exit in
  let insert_cost _ = 10 in
  let delete_cost _ = 10 in
  let replace_cost a b =
    match Uchar.equal a b with
    | true -> 0
    | false when equal_caseless a b -> 1
    | false -> 20
  in
  let m = Array.length a in
  let n = Array.length b in
  let prev = Array.init (n + 1) ~f:(fun j ->
    let v = ref 0 in
    for jj = 0 to j - 1 do v := !v + insert_cost b.(jj) done;
    !v)
  in
  let curr = Array.create ~len:(n + 1) 0 in
  try
    for i = 0 to m - 1 do
      curr.(0) <- prev.(0) + delete_cost a.(i);
      let row_min = ref curr.(0) in
      for j = 0 to n - 1 do
        let del = prev.(j + 1) + delete_cost a.(i) in
        let ins = curr.(j) + insert_cost b.(j) in
        let sub = prev.(j) + replace_cost a.(i) b.(j) in
        let v = Int.min (Int.min del ins) sub in
        curr.(j + 1) <- v;
        if v < !row_min then row_min := v
      done;
      (match !row_min > max_edits with
       | true -> raise Early_exit
       | false -> ());
      Array.blit ~src:curr ~src_pos:0 ~dst:prev ~dst_pos:0 ~len:(n + 1)
    done;
    prev.(n)
  with Early_exit -> Int.max_value


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

let ed s1 s2 = weighted_edit_distance_uchars (to_uchars s1) (to_uchars s2)

let%expect_test "edit_distance: identical strings" =
  printf "%d" (ed "hello" "hello");
  [%expect {| 0 |}]

let%expect_test "edit_distance: case only" =
  printf "%d" (ed "Hello" "hello");
  [%expect {| 1 |}]

let%expect_test "edit_distance: all caps" =
  printf "%d" (ed "THE" "the");
  [%expect {| 3 |}]

let%expect_test "edit_distance: one char substitution" =
  printf "%d" (ed "cat" "bat");
  [%expect {| 20 |}]

let%expect_test "edit_distance: insertion" =
  printf "%d" (ed "helo" "hello");
  [%expect {| 10 |}]

let%expect_test "edit_distance: accent difference" =
  printf "%d" (ed "Amelie" "Amélie");
  [%expect {| 10 |}]

let%expect_test "edit_distance: case + accent" =
  printf "%d" (ed "amelie" "Amélie");
  [%expect {| 11 |}]

let%expect_test "edit_distance: Star Wars: Skeleton Crew" =
  printf "%d" (ed "Star Wars: Skeleton Crew" "Star Wars: Detours");
  [%expect {| 120 |}]

let%expect_test "edit_distance: Star Wars: Skeleton Crew" =
  printf "%d" (ed "Star Wars: Skeleton Crew" "Skeleton Crew");
  [%expect {| 110 |}]

let%expect_test "edit_distance: max_edits short-circuit" =
  printf "%d" (weighted_edit_distance_uchars ~max_edits:5 (to_uchars "cat") (to_uchars "bat"));
  [%expect {| 4611686018427387903 |}]

let%expect_test "edit_distance: max_edits allows case change" =
  printf "%d" (weighted_edit_distance_uchars ~max_edits:5 (to_uchars "Cat") (to_uchars "cat"));
  [%expect {| 1 |}]

let%expect_test "edit_distance: max_edits exact match" =
  printf "%d" (weighted_edit_distance_uchars ~max_edits:0 (to_uchars "hello") (to_uchars "hello"));
  [%expect {| 0 |}]

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
