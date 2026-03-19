(** Main entry point for the IMDB rating lookup server *)

let normalize = Normalize.normalize
let tokenize = Normalize.tokenize

let cache_path data_dir = Filename.concat data_dir "index.cache"

let tsv_paths data_dir =
  [ Filename.concat data_dir "title.basics.tsv";
    Filename.concat data_dir "title.ratings.tsv" ]

(** Check if cache is newer than all TSV source files *)
let cache_is_fresh data_dir =
  let cache = cache_path data_dir in
  match Unix.stat cache with
  | exception Unix.Unix_error _ -> false
  | cache_stat ->
    List.for_all (fun tsv ->
      match Unix.stat tsv with
      | exception Unix.Unix_error _ -> false
      | tsv_stat -> cache_stat.Unix.st_mtime >= tsv_stat.Unix.st_mtime
    ) (tsv_paths data_dir)

(** Load index from marshal cache (unsafe) *)
let load_cache data_dir =
  let path = cache_path data_dir in
  let ic = open_in_bin path in
  let cache : Fuzzy_match.cache = Marshal.from_channel ic in
  close_in ic;
  let index = Fuzzy_match.of_cache ~normalize ~tokenize cache in
  { Handlers.index }

(** Save index to marshal cache *)
let save_cache data_dir (state : Handlers.state) =
  let path = cache_path data_dir in
  let oc = open_out_bin path in
  Marshal.to_channel oc (Fuzzy_match.to_cache state.index) [Marshal.No_sharing];
  close_out oc

(** Load IMDB data from TSV files and build search index *)
let load_from_tsv ~fs ~data_dir =
  let titles =
    let filter = function
      | { Imdb_data.title_type = Types.(Short | VideoGame | TvEpisode); _ } -> false
      | _ -> true
    in
    Imdb_data.read ~filter Eio.Path.(fs / data_dir)
  in
  let index = Fuzzy_match.build ~normalize ~tokenize titles in
  { Handlers.index }

(** Load IMDB data: try cache first, fall back to TSV *)
let load_data ~fs ~data_dir =
  if cache_is_fresh data_dir then begin
    Printf.printf "Loading from cache %s...\n%!" (cache_path data_dir);
    let state = load_cache data_dir in
    Printf.printf "Cache loaded\n%!";
    state
  end else begin
    let state = load_from_tsv ~fs ~data_dir in
    Printf.printf "Saving cache to %s...\n%!" (cache_path data_dir);
    save_cache data_dir state;
    Printf.printf "Cache saved\n%!";
    state
  end

(** Reload data on separate domain to avoid blocking requests.
    Always reloads from TSV and rebuilds cache. *)
let reload_data_async ~dm ~fs ~data_dir ~state_ref =
  Eio.Domain_manager.run dm (fun () ->
    Printf.printf "Reloading data from %s...\n%!" data_dir;
    let new_state = load_from_tsv ~fs ~data_dir in
    save_cache data_dir new_state;
    Atomic.set state_ref new_state;
    Printf.printf "Data reload complete\n%!")

(** Start worker domains that process queries from the queue *)
let start_workers ~sw ~dm ~state_ref ~queue =
  let num_domains = Domain.recommended_domain_count () in
  Printf.printf "Starting %d worker domains\n%!" num_domains;
  List.init num_domains (fun _ ->
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Domain_manager.run dm (fun () ->
        let rec loop () =
          let (query, resolver) = Eio.Stream.take queue in
          let state = Atomic.get state_ref in
          let result = Handlers.lookup_one state query in
          Eio.Promise.resolve resolver result;
          loop ()
        in
        loop ()
      )
    )
  )
  |> ignore

(** SIGHUP reload flag *)
let reload_requested = Atomic.make false

(** Setup SIGHUP signal handler *)
let setup_sighup_handler () =
  Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ ->
    Atomic.set reload_requested true;
    Printf.printf "SIGHUP received, reload scheduled\n%!"))

(** Monitor for reload requests *)
let start_reload_monitor ~sw ~dm ~fs ~data_dir ~state_ref ~clock =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      Eio.Time.sleep clock 1.0;
      (match Atomic.get reload_requested with
       | true ->
           Atomic.set reload_requested false;
           reload_data_async ~dm ~fs ~data_dir ~state_ref
       | false -> ());
      loop ()
    in
    loop ()
  )

(** Main entry point *)
let main () =
  let config = Config.parse () in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let fs = Eio.Stdenv.fs env in
  let dm = Eio.Stdenv.domain_mgr env in
  let clock = Eio.Stdenv.clock env in

  Printf.printf "Loading IMDB data from %s...\n%!" config.data_dir;
  let initial_state = load_data ~fs ~data_dir:config.data_dir in
  let state_ref = Atomic.make initial_state in
  Printf.printf "Data loaded, %d titles indexed\n%!" (Array.length initial_state.index.Fuzzy_match.titles);

  let queue : Server.work_queue = Eio.Stream.create 0 in

  setup_sighup_handler ();
  start_reload_monitor ~sw ~dm ~fs ~data_dir:config.data_dir ~state_ref ~clock;
  start_workers ~sw ~dm ~state_ref ~queue;

  Server.start ~sw ~env ~port:config.port ~queue

let () = main ()
