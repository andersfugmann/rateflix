open Js_of_ocaml

let show_status status_div msg =
  status_div##.textContent := Js.some (Js.string msg);
  let _ = Dom_html.window##setTimeout
      (Js.wrap_callback (fun () -> status_div##.textContent := Js.null))
      (Js.number_of_float 2000.) in
  ()

let () =
  let open Dom_html in
  match getElementById_coerce "apikey" CoerceTo.input,
        getElementById_coerce "status" CoerceTo.div,
        getElementById_coerce "apikey-form" CoerceTo.form with
  | Some apikey_input, Some status_div, Some form ->

      Dom_html.addEventListener form Dom_html.Event.submit
        (Dom_html.full_handler
           (fun ev ->
              let (_ : Dom_html.submitEvent Js.t) = Js.Unsafe.coerce ev in
              let key = Js.to_string apikey_input##.value |> String.trim in
              Omdb.save_key key;
              show_status status_div "Api key saved";
              (fun _ -> Js._false)
           )) Js._false
      |> ignore;

      let key = Omdb.load_key () in
      Option.iter (fun key ->
        apikey_input##.value := key;
        show_status status_div "Api key loaded") key
  | _ -> ()
