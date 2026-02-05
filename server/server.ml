(** HTTP server using Cohttp-eio *)

type work_queue = (Types.query * Types.search_result Eio.Promise.u) Eio.Stream.t

(** Post queries to worker queue and await results *)
let lookup ~queue (request : Types.request) : Types.response =
  let promises =
    request
    |> List.map (fun query ->
         let promise, resolver = Eio.Promise.create () in
         Eio.Stream.add queue (query, resolver);
         (query, promise))
  in
  promises
  |> List.map (fun (query, promise) ->
       (query, Eio.Promise.await promise))

(** Handle a single HTTP request *)
let handle_request ~queue body =
  body
  |> Yojson.Safe.from_string
  |> Types.request_of_yojson
  |> function
     | Ok request ->
         lookup ~queue request
         |> Types.response_to_yojson
         |> Yojson.Safe.to_string
         |> Option.some
     | Error _ -> None

(** Create the callback function *)
let make_callback ~queue =
  fun _conn request body ->
    let meth = Http.Request.meth request in
    let path = Http.Request.resource request in
    match meth, path with
    | `POST, "/lookup" ->
        let body_str = Eio.Buf_read.(of_flow ~max_size:10_000_000 body |> take_all) in
        (match handle_request ~queue body_str with
         | Some response_body ->
             let headers = Http.Header.of_list [("Content-Type", "application/json")] in
             Cohttp_eio.Server.respond ~headers ~status:`OK ~body:(Cohttp_eio.Body.of_string response_body) ()
         | None ->
             Cohttp_eio.Server.respond ~status:`Bad_request ~body:(Cohttp_eio.Body.of_string "Invalid JSON request") ())
    | _ ->
        Cohttp_eio.Server.respond ~status:`Not_found ~body:(Cohttp_eio.Body.of_string "Not found") ()

(** Start the HTTP server *)
let start ~sw ~env ~port ~queue =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  let callback = make_callback ~queue in
  let server = Cohttp_eio.Server.make ~callback () in
  Printf.printf "Server listening on port %d\n%!" port;
  Cohttp_eio.Server.run socket server ~on_error:(fun ex ->
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string ex))
