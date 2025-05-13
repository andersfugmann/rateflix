open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

(* Simulate fetching a random score asynchronously *)
let get_random_score () =
  Lwt_js.sleep (Random.float 0.5) >>= fun () ->
  Lwt.return (Random.float 10.0)

let add_score_icon elt =
  let doc = Dom_html.document in
  let span = Dom_html.createSpan doc in
  let span = (span :> Dom_html.element Js.t) in
  get_random_score () >>= fun score ->
  span##.textContent := Js.some (Js.string (Printf.sprintf "IMDb: %.1f" score));
  span##.style##.marginLeft := Js.string "8px";
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
