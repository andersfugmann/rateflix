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
        let storage = Dom_html.window##.localStorage in
        let key = "omdbApiKey" in
        let value = Js.to_string apikey_input##.value |> String.trim in
        Js.Optdef.iter storage (fun storage -> storage##setItem (Js.string key) (Js.string value));
        show_status "API key saved!";
        Js._false
      in

      let load_key () =
        let key = "omdbApiKey" in
        let storage = Dom_html.window##.localStorage |> Js.Optdef.to_option in
        let value = Option.bind storage
            (fun storage ->
               let x = storage##getItem (Js.string key) in
               let y = Js.Opt.to_option x in
               y
            )
        in
        Option.iter (fun value -> apikey_input##.value := value; show_status "Api key loaded") value;
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
