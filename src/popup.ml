open Js_of_ocaml

let () =
  let open Dom_html in
  match getElementById_coerce "apikey" CoerceTo.input,
        getElementById_coerce "status" CoerceTo.div,
        getElementById_coerce "apikey-form" CoerceTo.form with
  | Some apikey_input, Some status_div, Some form ->
      let show_status msg =
        status_div##.textContent := Js.some (Js.string msg);
        let _ = Dom_html.window##setTimeout
          (Js.wrap_callback (fun () -> status_div##.textContent := Js.null))
          (Js.number_of_float 2000.) in
        ()
      in

      let save_key (_ : Dom_html.event Js.t) =
        let key = Js.to_string apikey_input##.value |> String.trim in
        let _ =
          Js.Unsafe.global##chrome##storage##sync##set
            (Js.Unsafe.obj [| ("omdbApiKey", Js.Unsafe.inject (Js.string key)) |])
            (Js.wrap_callback (fun _ -> show_status "API key saved!"))
        in
        Js._false
      in

      let load_key () =
        let _ =
          Js.Unsafe.global##chrome##storage##sync##get
            (Js.array [| Js.string "omdbApiKey" |])
            (Js.wrap_callback (fun result ->
              let key = Js.Optdef.to_option (Js.Unsafe.get result "omdbApiKey") in
              match key with
              | Some k -> apikey_input##.value := Js.to_string k |> Js.string
              | None -> ()
            ))
        in
        ()
      in

      ignore (Dom_html.addEventListener form Dom_html.Event.submit
        (Dom_html.full_handler (fun ev ->
          let (_ : Dom_html.submitEvent Js.t) = Js.Unsafe.coerce ev in
          ignore (save_key (Js.Unsafe.coerce ev));
          (fun _ -> Js._false)
        )) Js._false);
      load_key ()
  | _ -> ()
