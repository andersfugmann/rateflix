open Js_of_ocaml
open! Lwt.Syntax
open Lib

(* Constants for slider storage keys *)
let slider_key_prefix = "transparency-"
let slider_beta_key = slider_key_prefix ^ "beta"
let slider_low_key = slider_key_prefix ^ "low"
let slider_high_key = slider_key_prefix ^ "high"
let slider_max_key = slider_key_prefix ^ "max"

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

(* Update a slider's display value *)
let set_transparency_display_value id value =
  let value_span_id = id ^ "-value" in
  match Dom_html.document##getElementById (Js.string value_span_id) |> Js.Opt.to_option with
  | Some span -> span##.textContent := Js.some (Js.string (Printf.sprintf "%.1f" value))
  | None -> Log.log `Info "%s Could not find %s" __FUNCTION__ value_span_id

let set_transparency_value id value =
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.input with
  | None -> Log.log `Info "%s Could not find %s" __FUNCTION__ id
  | Some s ->
    s##.value := Js.string (string_of_float value);
    set_transparency_display_value id value

let get_transparency_value id =
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.input with
  | None -> 0.0
  | Some s ->
    s##.value
    |> Js.to_string
    |> float_of_string

(* Read all settings from the controls *)
let get_transparency_settings () =
  Transparency.{
    beta = get_transparency_value slider_beta_key;
    low = get_transparency_value slider_low_key;
    high = get_transparency_value slider_high_key;
    max = get_transparency_value slider_max_key;
  }

let set_transparency_settings t =
  set_transparency_value slider_beta_key t.Transparency.beta;
  set_transparency_value slider_low_key t.Transparency.low;
  set_transparency_value slider_high_key t.Transparency.high;
  set_transparency_value slider_max_key t.Transparency.max;
  ()

(* Set up a slider with its stored value and event listener *)
let setup_transparency_slider ~callback:_ slider_id =
  (* We should find the value element. No need to continiously look it up. *)
  let open Dom_html in
  match getElementById_coerce slider_id CoerceTo.input with
  | Some slider ->
      (* Add event listener for input changes *)
      addEventListener slider Event.input
        (handler (fun _ ->
          let value = float_of_string (Js.to_string slider##.value) in
          set_transparency_display_value slider_id value;
          Transparency.send_event (get_transparency_settings ());
          Js._true
        ))
        Js._true
      |> ignore
  | None -> Log.log `Info "%s Could not find %s" __FUNCTION__ slider_id


(* Function to set up save button *)
let setup_save_button status_div =
  let open Dom_html in
  match getElementById_coerce "save-transparency" CoerceTo.button with
  | None -> Lwt.return_unit
  | Some button ->
      addEventListener button Event.click
        (handler (fun _ ->
           let transparency = get_transparency_settings () in
           Lwt.ignore_result (
             let* () = Transparency.save transparency in
             show_status status_div "Transparency settings saved";
             Lwt.return_unit);
           Js._false
         ))
        Js._false
      |> ignore;

      Lwt.return_unit

(* Function to reset sliders to default values *)
let setup_reset_button status_div =
  let open Dom_html in
  match getElementById_coerce "reset-transparency" CoerceTo.button with
  | None -> Lwt.return_unit
  | Some button ->
      addEventListener button Event.click
        (handler (fun _ ->
           let transparency = Transparency.default in
           Lwt.ignore_result (Transparency.save transparency);
           set_transparency_settings transparency;
           show_status status_div "Transparency Settings Reset";
          Js._false
        ))
        Js._false
      |> ignore;

      Lwt.return_unit


let run () =
  let open Dom_html in

  let* transparency = Transparency.load () in
  set_transparency_settings transparency;

  (* Initialize cache count display *)
  let callback () = () in

  (* Set up transparency sliders with immediate values *)
  List.iter (setup_transparency_slider ~callback)
    [slider_beta_key; slider_low_key; slider_high_key; slider_max_key];

  let apikey_el = getElementById_coerce "apikey" CoerceTo.input |> Option.get in
  let status_el = getElementById "status" in
  let apikey_save_el = getElementById "apikey-save" in
  let cache_count = getElementById "cache-count" in
  let clear_cache_el = getElementById "clear-cache" in

  Lwt.ignore_result(update_cache_count cache_count);

  Dom_html.addEventListener apikey_save_el Dom_html.Event.click
    (Dom_html.handler
       (fun _ ->
          let key = Js.to_string apikey_el##.value |> String.trim in
          Lwt.ignore_result @@ Lib.Storage.save_key Lib.Omdb.omdb_key key;
          show_status status_el "Api key saved";
          Js._false
       )) Js._false
  |> ignore;

  Dom_html.addEventListener clear_cache_el Dom_html.Event.click
    (Dom_html.handler
       (fun _ ->
          let inner () =
            let* () = Lib.Storage.clear_cache () in
            let* () = update_cache_count cache_count in
            Lwt.return_none
          in
          Lwt.ignore_result (inner ());
          show_status status_el "Cache cleared";
          Js._false
       )) Js._false
  |> ignore;

  (* Set up save and reset buttons *)
  let* () = setup_save_button status_el in
  let* () = setup_reset_button status_el in

  let* key = Lib.Storage.load_key Lib.Omdb.omdb_key in
  begin
    match key with
    | Some key ->
      apikey_el##.value := (Js.string key);
      show_status status_el "Api key loaded"
    | None -> ()
  end;
  Lwt.return_unit


let () =
  Log.log `Info "Open popup";
  (* Start the preload immediately when the script loads, before the DOM is ready *)

  (* Wait for DOM to be loaded *)
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Lwt.ignore_result (run ());
    Js._false
  )
