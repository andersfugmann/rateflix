open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
open! Lwt.Infix
open! StdLabels
open! ListLabels
open! MoreLabels

let rec daemon ~f condition () =
  let* () = f () in
  let* () = Lwt_condition.wait condition in
  let* () = Lwt_js.sleep 1.0 in
  daemon ~f condition ()

let observe_dom_changes condition =
  (* This should kick the daemon process *)
  let target = Dom_html.document##.body in
  let node = (target :> Dom.node Js.t) in
  let f _records _observer =
    Lwt_condition.signal condition ()
  in
  MutationObserver.observe ~node ~f
    ~attributes:true ~child_list:true ~subtree:true ~character_data:true
    ()
  |> ignore

let start ~init ~add_ratings () =
  let condition = Lwt_condition.create () in
  let* () = init () in
  Lwt.async (daemon ~f:add_ratings condition);
  observe_dom_changes condition;
  Lwt.return_unit

let start_plugin ~init ~add_ratings () =
  Lwt.ignore_result (start ~init ~add_ratings ())
