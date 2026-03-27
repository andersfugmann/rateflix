(** Test server: fuzzy search integration tests against a running rateflix server.
    Reads test cases from test_cases.txt (pipe-delimited), sends batched HTTP
    requests, and compares results against expected IMDB IDs. *)

let server_url =
  match Sys.getenv_opt "SERVER_URL" with
  | Some url -> url
  | None ->
    let server = Option.value ~default:"localhost" (Sys.getenv_opt "SERVER") in
    let port = Option.value ~default:"1913" (Sys.getenv_opt "PORT") in
    Printf.sprintf "http://%s:%s" server port

type test_case = {
  query: Rateflix_types.query;
  expected_id: string;
  description: string;
  xfail: bool;
}

type section = {
  name: string;
  cases: test_case list;
}

(* Colors *)
let green s = Printf.sprintf "\027[0;32m%s\027[0m" s
let red s = Printf.sprintf "\027[0;31m%s\027[0m" s
let yellow s = Printf.sprintf "\027[0;33m%s\027[0m" s

let trim s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
  let j = ref (len - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\r' || s.[!j] = '\n') do decr j done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)

let split_on_pipe line =
  String.split_on_char '|' line |> List.map trim

let parse_test_cases filename =
  let ic = open_in filename in
  let sections = ref [] in
  let current_name = ref "" in
  let current_cases = ref [] in
  let flush () =
    match !current_cases with
    | [] -> ()
    | cases -> sections := { name = !current_name; cases = List.rev cases } :: !sections
  in
  (try while true do
    let line = input_line ic in
    let line = trim line in
    if line = "" then ()
    else if String.length line > 6 && String.sub line 0 5 = "# ---" then begin
      flush ();
      current_cases := [];
      let s = String.sub line 5 (String.length line - 5) in
      let s = trim s in
      let s = if String.length s >= 3 && String.sub s (String.length s - 3) 3 = "---" then
        trim (String.sub s 0 (String.length s - 3))
      else s in
      current_name := s
    end
    else if String.length line > 0 && line.[0] = '#' then ()
    else begin
      match split_on_pipe line with
      | title :: expected_id :: rest ->
        let year = match rest with
          | y :: _ when y <> "" -> int_of_string_opt y
          | _ -> None
        in
        let title_types = match rest with
          | _ :: t :: _ when t <> "" ->
            let types = String.split_on_char ',' t |> List.map trim |> List.filter (fun s -> s <> "") in
            Some (List.map Rateflix_types.title_type_of_string types)
          | _ -> None
        in
        let description = match rest with
          | _ :: _ :: d :: _ when d <> "" -> d
          | _ -> title
        in
        let xfail = match rest with
          | _ :: _ :: _ :: x :: _ -> trim x = "XFAIL"
          | _ -> false
        in
        let query = { Rateflix_types.title; year; title_types } in
        current_cases := { query; expected_id; description; xfail } :: !current_cases
      | _ -> ()
    end
  done with End_of_file -> ());
  close_in ic;
  flush ();
  List.rev !sections

let build_payload cases =
  let request = List.map (fun tc -> tc.query) cases in
  Rateflix_types.request_to_yojson request |> Yojson.Safe.to_string

type counters = {
  mutable passed: int;
  mutable failed: int;
  mutable known_fail: int;
  mutable known_fixed: int;
  mutable failed_cases: test_case list;
  mutable xfail_cases: test_case list;
}

let post_lookup ~env ~sw url payload =
  let uri = Uri.of_string (url ^ "/lookup") in
  let headers = Http.Header.of_list [ "Content-Type", "application/json" ] in
  let body = Cohttp_eio.Body.of_string payload in
  let resp, resp_body = Cohttp_eio.Client.post ~sw
    (Cohttp_eio.Client.make ~https:None env#net) ~headers ~body uri in
  let status = Http.Response.status resp in
  if status <> `OK then
    failwith (Printf.sprintf "HTTP %d" (Http.Status.to_int status));
  let body_str = Eio.Buf_read.(of_flow ~max_size:10_000_000 resp_body |> take_all) in
  match Rateflix_types.response_of_yojson (Yojson.Safe.from_string body_str) with
  | Ok response -> response
  | Error msg -> failwith (Printf.sprintf "Failed to parse response: %s" msg)

let format_year = function
  | Some y -> Printf.sprintf " (%d)" y
  | None -> ""

let check_section counters section response =
  let cases = section.cases in
  if cases <> [] then begin
    Printf.printf "\n--- %s ---\n%!" section.name;
    List.iteri (fun i tc ->
      let (_query, (result : Rateflix_types.search_result)) = List.nth response i in
      let year_info = format_year tc.query.year in
      if result.imdb_id = tc.expected_id then begin
        if tc.xfail then begin
          Printf.printf "%s %s: '%s'%s → \"%s\"%s score: %g\n%!"
            (green "✓ FIXED") tc.description tc.query.title year_info result.title (format_year result.year) result.match_score;
          counters.known_fixed <- counters.known_fixed + 1
        end else begin
          Printf.printf "%s %s: '%s'%s → \"%s\"%s score: %g\n%!"
            (green "✓") tc.description tc.query.title year_info result.title (format_year result.year) result.match_score;
          counters.passed <- counters.passed + 1
        end
      end else begin
        if tc.xfail then begin
          Printf.printf "%s %s: '%s'%s → \"%s\"%s score: %g [wanted: %s, got: %s]\n%!"
            (yellow "✗") tc.description tc.query.title year_info result.title (format_year result.year) result.match_score
            tc.expected_id result.imdb_id;
          counters.known_fail <- counters.known_fail + 1;
          counters.xfail_cases <- tc :: counters.xfail_cases
        end else begin
          Printf.printf "%s %s: '%s'%s → \"%s\"%s score: %g [expected: %s, got: %s]\n%!"
            (red "✗") tc.description tc.query.title year_info result.title (format_year result.year) result.match_score
            tc.expected_id result.imdb_id;
          counters.failed <- counters.failed + 1;
          counters.failed_cases <- tc :: counters.failed_cases
        end
      end
    ) cases
  end

let () =
  let test_file =
    match Sys.getenv_opt "TEST_FILE" with
    | Some f -> f
    | None ->
      let exe_dir = Filename.dirname Sys.executable_name in
      let candidates = [
        Filename.concat exe_dir "test_cases.txt";
        Filename.concat exe_dir "../test/test_cases.txt";
        "test/test_cases.txt";
        "test_cases.txt";
      ] in
      match List.find_opt Sys.file_exists candidates with
      | Some f -> f
      | None -> failwith "Cannot find test_cases.txt"
  in
  let sections = parse_test_cases test_file in
  Printf.printf "=== Fuzzy Search Tests ===\n%!";
  Printf.printf "Server: %s\n%!" server_url;
  Printf.printf "Test file: %s\n%!" test_file;
  let counters = {
    passed = 0; failed = 0; known_fail = 0; known_fixed = 0;
    failed_cases = []; xfail_cases = [];
  } in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Fire all section requests in parallel *)
  let promises = List.map (fun section ->
    let p, r = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      let payload = build_payload section.cases in
      let response = post_lookup ~env ~sw server_url payload in
      Eio.Promise.resolve r response
    );
    (section, p)
  ) sections in
  (* Process responses sequentially *)
  List.iter (fun (section, p) ->
    let response = Eio.Promise.await p in
    check_section counters section response
  ) promises;
  (* Summary *)
  Printf.printf "\n=== Results ===\n%!";
  Printf.printf "Passed: %s\n%!" (green (string_of_int counters.passed));
  Printf.printf "Failed: %s\n%!" (red (string_of_int counters.failed));
  if counters.known_fail > 0 then
    Printf.printf "Known wrong: %s\n%!" (yellow (string_of_int counters.known_fail));
  if counters.known_fixed > 0 then
    Printf.printf "Known fixed: %s\n%!" (green (string_of_int counters.known_fixed));
  if counters.known_fail > 0 then begin
    Printf.printf "\n=== Expected Failures ===\n%!";
    let payload = build_payload (List.rev counters.xfail_cases) in
    ignore (post_lookup ~env ~sw server_url payload)
  end;
  if counters.failed > 0 then begin
    Printf.printf "\n=== Re-running failed tests ===\n%!";
    let payload = build_payload (List.rev counters.failed_cases) in
    ignore (post_lookup ~env ~sw server_url payload);
    exit 1
  end
