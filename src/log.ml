open! Js_of_ocaml
let log = false

(* Enable different levels *)

let log level =
  let logger level =
    match level with
    | `Debug -> fun s -> Console.console##debug (Js.string s)
    | `Info -> fun s -> Console.console##info (Js.string s)
    | `Warn -> fun s -> Console.console##warn (Js.string s)
    | `Error -> fun s -> Console.console##error (Js.string s)
  in
  let logger = logger level in

  match log, level with
  | false, `Debug -> fun fmt -> Printf.ifprintf () fmt
  | _ -> fun fmt -> Printf.ksprintf (fun s -> logger s) fmt
