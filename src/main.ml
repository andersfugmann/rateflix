open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix
open Lwt.Syntax

(* This function simulates an asynchronous operation to fetch a random score.
   In a real-world scenario, this could be an API call or some other async operation. *)

(* Simulate fetching a random score asynchronously *)
let get_random_score () =
  let* () = Lwt_js.sleep (Random.float 0.5) in
  Lwt.return (Random.float 10.0)

let add_score_icon elt =
  let doc = Dom_html.document in
  let span = Dom_html.createSpan doc in
  let span = (span :> Dom_html.element Js.t) in
  get_random_score () >>= fun score ->
  span##.textContent := Js.some (Js.string (Printf.sprintf "%.1f" score));
  (* Style the span as a small round yellow circle overlay *)
  span##.style##.position := Js.string "absolute";
  span##.style##.top := Js.string "8px";
  span##.style##.right := Js.string "8px";
  span##.style##.width := Js.string "16px";
  span##.style##.height := Js.string "16px";
  span##.style##.backgroundColor := Js.string "#FFD600";
  span##.style##.color := Js.string "#222";
  span##.style##.borderRadius := Js.string "50%";
  span##.style##.display := Js.string "flex";
  let _ = span##.style##setProperty (Js.string "align-items") (Js.string "center") Js.Optdef.empty in
  let _ = span##.style##setProperty (Js.string "justify-content") (Js.string "center") Js.Optdef.empty in
  span##.style##.fontWeight := Js.string "bold";
  span##.style##.fontSize := Js.string "8px";
  span##.style##.zIndex := Js.string "1000";
  let _ = span##.style##setProperty (Js.string "box-shadow") (Js.string "0 2px 8px rgba(0,0,0,0.15)") Js.Optdef.empty in
  (* Ensure the parent is relatively positioned for absolute overlay *)
  let parent =
    match Js.Opt.to_option elt##.parentNode with
    | Some p -> (Js.Unsafe.coerce p : Dom_html.element Js.t)
    | None -> elt
  in
  if parent##.style##.position = Js.string "" then
    parent##.style##.position := Js.string "relative";
  Dom.appendChild elt span;
  Lwt.return_unit

let () =
  let () = Random.self_init () in
  let _ =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->
      let titles = Dom_html.document##querySelectorAll (Js.string ".title-card") in
      let tasks = ref [] in
      for i = 0 to titles##.length - 1 do
        match Js.Opt.to_option (titles##item i) with
        | Some elt ->
            let elt = Js.Unsafe.coerce elt in
            tasks := (add_score_icon elt) :: !tasks
        | None -> ()
      done;
      let _ = Lwt.ignore_result (Lwt.join !tasks) in
      Js._false
    )
  in
  ()
