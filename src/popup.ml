open Js_of_ocaml
open! Lwt.Syntax

let show_status status_div msg =
  status_div##.textContent := Js.some (Js.string msg);
  let _ = Dom_html.window##setTimeout
      (Js.wrap_callback (fun () -> status_div##.textContent := Js.null))
      (Js.number_of_float 2000.) in
  ()

let update_cache_count count_elem =
  let* count = Lib.Storage.count_cache_entries () in
  let text = Js.string (string_of_int count) in
  count_elem##.innerHTML := text;
  Lwt.return_unit

let run () =
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
              Lwt.ignore_result @@ Lib.Storage.save_key Lib.Omdb.omdb_key key;
              show_status status_div "Api key saved";
              (fun _ -> Js._false)
           )) Js._false
      |> ignore;

      Dom_html.addEventListener clear_btn Dom_html.Event.click
        (Dom_html.handler
           (fun _ ->
              let inner () =
                let* () = Lib.Storage.clear_cache () in
                let* () = update_cache_count cache_count in
                Lwt.return_none
              in
              Lwt.ignore_result (inner ());
              show_status status_div "Cache cleared";
              Js._false
           )) Js._false
      |> ignore;

      (* Initialize cache count display *)
      let* () = update_cache_count cache_count in

      let* key = Lib.Storage.load_key Lib.Omdb.omdb_key in
      begin
        match key with
        | Some key ->
          apikey_input##.value := (Js.string key);
          show_status status_div "Api key loaded"
        | None -> ()
      end;
      Lwt.return_unit
  | _ -> Lwt.return_unit

let () =
  Lwt.ignore_result (run ())
