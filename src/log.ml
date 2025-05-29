open! Js_of_ocaml
let log = true

let log level =
  let logger level =
    match level with
    | `Debug -> fun s -> Console.console##debug s
    | `Info -> fun s -> Console.console##info s
    | `Warn -> fun s -> Console.console##warn s
    | `Error -> fun s -> Console.console##error s
  in
  let logger = logger level in

  match log with
  | true -> fun fmt -> Printf.ksprintf (fun s -> logger s) fmt
  | false -> fun fmt -> Printf.ifprintf () fmt
