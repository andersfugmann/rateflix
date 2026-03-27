(** HTTP server using Cohttp-eio *)

type work_queue = (Types.query * (Types.search_result * Database.search_stats option * float) Eio.Promise.u) Eio.Stream.t

(** Post queries to worker queue and await results *)
let lookup ~queue (request : Types.request) =
  let promises =
    request
    |> List.map (fun query ->
         let promise, resolver = Eio.Promise.create () in
         Eio.Stream.add queue (query, resolver);
         (query, promise))
  in
  promises
  |> List.map (fun (query, promise) ->
       let (result, stats, elapsed_ms) = Eio.Promise.await promise in
       (query, result, stats, elapsed_ms))

(** Format client address *)
let client_addr conn =
  let ((_sw, peer_addr), _conn_id) = conn in
  Format.asprintf "%a" Eio.Net.Sockaddr.pp peer_addr

(** Log a response line for each query/result pair *)
let log_response addr responses =
  List.iter (fun ((query : Types.query), (result : Types.search_result), stats, elapsed_ms) ->
    let year_str = match query.year with
      | Some y -> Printf.sprintf " (%d)" y
      | None -> ""
    in
    let result_year_str = match result.year with
      | Some y -> Printf.sprintf " (%d)" y
      | None -> ""
    in
    let stats_str = match stats with
      | Some (s : Database.search_stats) ->
        Printf.sprintf " [candidates: %4d, filtered: %3d]" s.candidates s.filtered
      | None -> " [no match]"
    in
    Printf.printf "[%s] %.2f%s [%4.1fms] \"%s\"%s -> \"%s\"%s\n%!"
      addr result.match_score stats_str elapsed_ms query.title year_str result.title result_year_str
  ) responses

(** Handle a single HTTP request *)
let handle_request ~queue ~addr body =
  body
  |> Yojson.Safe.from_string
  |> Types.request_of_yojson
  |> function
     | Ok request ->
         let results = lookup ~queue request in
         log_response addr results;
         let response = List.map (fun (q, r, _, _) -> (q, r)) results in
         response
         |> Types.response_to_yojson
         |> Yojson.Safe.to_string
         |> Option.some
     | Error _ -> None

let cors_headers = Http.Header.of_list [
  ("Content-Type", "application/json");
  ("Access-Control-Allow-Origin", "*");
  ("Access-Control-Allow-Methods", "POST, OPTIONS");
  ("Access-Control-Allow-Headers", "Content-Type");
]

(** Create the callback function *)
let make_callback ~queue =
  fun conn request body ->
    let meth = Http.Request.meth request in
    let path = Http.Request.resource request in
    let addr = client_addr conn in
    match meth, path with
    | `OPTIONS, "/lookup" ->
        Cohttp_eio.Server.respond ~headers:cors_headers ~status:`No_content ~body:(Cohttp_eio.Body.of_string "") ()
    | `POST, "/lookup" ->
        let body_str = Eio.Buf_read.(of_flow ~max_size:1_000_000 body |> take_all) in
        (match handle_request ~queue ~addr body_str with
         | Some response_body ->
             Cohttp_eio.Server.respond ~headers:cors_headers ~status:`OK ~body:(Cohttp_eio.Body.of_string response_body) ()
         | None ->
             Cohttp_eio.Server.respond ~headers:cors_headers ~status:`Bad_request ~body:(Cohttp_eio.Body.of_string "Invalid JSON request") ())
    | _ ->
        Cohttp_eio.Server.respond ~status:`Not_found ~body:(Cohttp_eio.Body.of_string "Not found") ()

(** Start the HTTP server *)
let start ~sw ~env ~port ~queue =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V6.any, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  let callback = make_callback ~queue in
  let server = Cohttp_eio.Server.make ~callback () in
  Printf.printf "Server listening on port %d\n%!" port;
  Cohttp_eio.Server.run socket server ~on_error:(fun ex ->
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string ex))
