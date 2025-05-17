open Js_of_ocaml

let show_status status_div msg =
  status_div##.textContent := Js.some (Js.string msg);
  let _ = Dom_html.window##setTimeout
      (Js.wrap_callback (fun () -> status_div##.textContent := Js.null))
      (Js.number_of_float 2000.) in
  ()

let update_cache_count count_elem =
  let count = Lib.Storage.count_cache_entries () in
  let text = Js.string (string_of_int count) in
  count_elem##.innerHTML := text

let () =
  let open Dom_html in
  match getElementById_coerce "apikey" CoerceTo.input,
        getElementById_coerce "status" CoerceTo.div,
        getElementById_coerce "apikey-form" CoerceTo.form,
        getElementById "cache-count",
        getElementById_coerce "clear-cache" CoerceTo.button with
  | Some apikey_input, Some status_div, Some form, cache_count, Some clear_btn ->

      Dom_html.addEventListener form Dom_html.Event.submit
        (Dom_html.full_handler
           (fun ev ->
              let (_ : Dom_html.submitEvent Js.t) = Js.Unsafe.coerce ev in
              let key = Js.to_string apikey_input##.value |> String.trim in
              Lib.Storage.save_key Lib.Omdb.omdb_key key;
              show_status status_div "Api key saved";
              (fun _ -> Js._false)
           )) Js._false
      |> ignore;

      Dom_html.addEventListener clear_btn Dom_html.Event.click
        (Dom_html.handler
           (fun _ ->
              Lib.Storage.clear_cache ();  (* Use the no-argument version that clears all cache *)
              update_cache_count cache_count;
              show_status status_div "Cache cleared";
              Js._false
           )) Js._false
      |> ignore;

      (* Initialize cache count display *)
      update_cache_count cache_count;

      let key = Lib.Storage.load_key Lib.Omdb.omdb_key in
      Option.iter (fun key ->
        apikey_input##.value := (Js.string key);
        show_status status_div "Api key loaded") key
  | _ -> ()
