(** Main entry point for the IMDB rating lookup server *)


let tsv_filenames = ["title.basics.tsv"; "title.ratings.tsv"]

let cache_path data_dir = Filename.concat data_dir "index.cache"

let tsv_paths data_dir = List.map (Filename.concat data_dir) tsv_filenames

(** Check if cache is newer than all TSV source files *)
let cache_is_fresh ~fs data_dir =
  let stat path = Eio.Path.stat ~follow:true Eio.Path.(fs / path) in
  try
    let cache_mtime = (stat (cache_path data_dir)).mtime in
    List.for_all (fun tsv ->
      try (stat tsv).mtime <= cache_mtime
      with _ -> false
    ) (tsv_paths data_dir)
  with _ -> false

(** Load index from cache *)
let load_cache ~fs data_dir =
  let data = Eio.Path.load Eio.Path.(fs / cache_path data_dir) in
  let buf = Bin_prot.Common.create_buf (String.length data) in
  Bin_prot.Common.blit_string_buf data buf ~len:(String.length data);
  let index = Database.bin_read_t buf ~pos_ref:(ref 0) in
  { Handlers.index }

(** Save index to cache *)
let save_cache ~fs data_dir (state : Handlers.state) =
  let size = Database.bin_size_t state.index in
  let buf = Bin_prot.Common.create_buf size in
  let _end_pos = Database.bin_write_t buf ~pos:0 state.index in
  let data = Bytes.create size in
  Bin_prot.Common.blit_buf_bytes buf data ~len:size;
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / cache_path data_dir)
    (Bytes.unsafe_to_string data)

(** Load IMDB data from TSV files and build search index *)
let load_from_tsv ~fs ~data_dir =
  let titles =
    let filter = function
      | { Imdb_data.title_type = Types.(Short | VideoGame | TvEpisode | TvSpecial | Video); _ } -> false
      | _ -> true
    in
    Imdb_data.read ~filter Eio.Path.(fs / data_dir)
  in
  let index = Database.build titles in
  { Handlers.index }

(** Load IMDB data: try cache first, fall back to TSV *)
let load_data ~fs ~data_dir =
  let from_tsv () =
    let state = load_from_tsv ~fs ~data_dir in
    Printf.printf "Saving cache to %s...\n%!" (cache_path data_dir);
    save_cache ~fs data_dir state;
    Printf.printf "Cache saved\n%!";
    state
  in
  match cache_is_fresh ~fs data_dir with
  | true ->
    Printf.printf "Loading from cache %s...\n%!" (cache_path data_dir);
    (match load_cache ~fs data_dir with
     | state ->
       Printf.printf "Cache loaded\n%!";
       state
     | exception exn ->
       Printf.printf "Cache load failed (%s), rebuilding from TSV...\n%!"
         (Printexc.to_string exn);
       from_tsv ())
  | false ->
    from_tsv ()

(** Download missing TSV files via HTTPS + gzip decompression.
    Returns true if all files are present afterwards. *)
let download_missing ~env ~sw ~data_dir =
  let missing = List.filter (fun f ->
    not (Sys.file_exists (Filename.concat data_dir f))
  ) tsv_filenames in
  match missing with
  | [] -> true
  | to_download ->
    Printf.printf "Missing data files: %s\n%!"
      (String.concat ", " to_download);
    let client, sw = Download.make_client ~sw ~env in
    (try
       List.iter (fun filename ->
         let out_path = Filename.concat data_dir filename in
         Download.fetch_tsv ~env ~sw ~client ~out_path filename
       ) to_download;
       Printf.printf "Download complete\n%!";
       true
     with exn ->
       Printf.printf "Download failed: %s\n%!" (Printexc.to_string exn);
       List.iter (fun filename ->
         let tmp = Filename.concat data_dir (filename ^ ".tmp") in
         try Sys.remove tmp with _ -> ()
       ) to_download;
       false)

(** Download fresh IMDB data via HTTPS, replace existing files, and reload. *)
let download_and_reload ~sw ~env ~fs ~data_dir ~state_ref =
  Printf.printf "Downloading fresh IMDB data...\n%!";
  let client, sw = Download.make_client ~sw ~env in
  (try
     List.iter (fun filename ->
       let out_path = Filename.concat data_dir filename in
       Download.fetch_tsv ~env ~sw ~client ~out_path filename
     ) tsv_filenames;
     (try Sys.remove (cache_path data_dir) with _ -> ());
     Printf.printf "Download complete, reloading...\n%!";
     let new_state = load_from_tsv ~fs ~data_dir in
     save_cache ~fs data_dir new_state;
     Atomic.set state_ref new_state;
     Gc.compact ();
     Printf.printf "Reload complete\n%!"
   with exn ->
     Printf.printf "Download failed: %s, keeping existing data\n%!"
       (Printexc.to_string exn))

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
          let t0 = Unix.gettimeofday () in
          let (result, stats) = Handlers.lookup_one state query in
          let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
          Eio.Promise.resolve resolver (result, stats, elapsed_ms);
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
let start_reload_monitor ~sw ~env ~fs ~data_dir ~state_ref ~clock =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      Eio.Time.sleep clock 1.0;
      (match Atomic.get reload_requested with
       | true ->
           Atomic.set reload_requested false;
           download_and_reload ~sw ~env ~fs ~data_dir ~state_ref
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

  let queue : Server.work_queue = Eio.Stream.create 0 in

  setup_sighup_handler ();

  Eio.Fiber.fork ~sw (fun () ->
    Server.start ~sw ~env ~port:config.port ~queue);

  Printf.printf "Loading IMDB data from %s...\n%!" config.data_dir;
  (if not (Sys.file_exists config.data_dir)
   then Sys.mkdir config.data_dir 0o755);
  if not (download_missing ~env ~sw ~data_dir:config.data_dir)
  then (Printf.eprintf "Cannot proceed without data files\n%!"; exit 1);
  let initial_state = load_data ~fs ~data_dir:config.data_dir in
  let state_ref = Atomic.make initial_state in
  Gc.compact ();
  Printf.printf "Data loaded, %d titles indexed\n%!" (Array.length initial_state.index.Database.titles);

  start_reload_monitor ~sw ~env ~fs ~data_dir:config.data_dir ~state_ref ~clock;
  start_workers ~sw ~dm ~state_ref ~queue

let () = main ()
