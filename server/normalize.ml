(** Unicode title normalization for fuzzy matching *)

let normalize title =
  (* First decompose to NFD to separate base chars from diacritics *)
  let decomposed = Uunf_string.normalize_utf_8 `NFD title in
  let buf = Buffer.create (String.length title) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String decomposed) in
  let rec decode_loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        (* Skip combining marks (diacritics) *)
        let dominated = Uucp.Alpha.is_alphabetic u || Uchar.equal u (Uchar.of_char ' ') in
        let is_mark = Uucp.Gc.general_category u = `Mn in
        (match dominated && not is_mark with
         | true ->
             (match Uucp.Case.Map.to_lower u with
              | `Self -> Uutf.Buffer.add_utf_8 buf u
              | `Uchars chars -> List.iter (fun c -> Uutf.Buffer.add_utf_8 buf c) chars)
         | false -> ());
        decode_loop ()
    | `End -> Buffer.contents buf
    | `Malformed _ -> decode_loop ()
    | `Await -> Buffer.contents buf
  in
  decode_loop ()
  |> String.trim
