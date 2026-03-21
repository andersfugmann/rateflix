(** Download and decompress gzipped files over HTTPS using
    cohttp-eio + tls-eio + decompress. *)

let imdb_base_url = "https://datasets.imdbws.com"

let make_client ~sw ~env =
  let net = Eio.Stdenv.net env in
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg msg) -> failwith ("CA certs: " ^ msg)
  in
  Mirage_crypto_rng_unix.use_default ();
  let https _uri socket =
    let host =
      _uri |> Uri.host
      |> Option.map (fun h -> Domain_name.(of_string_exn h |> host_exn))
    in
    match Tls.Config.client ~authenticator () with
    | Ok c -> Tls_eio.client_of_flow c ?host socket
    | Error (`Msg msg) -> failwith ("TLS config: " ^ msg)
  in
  Cohttp_eio.Client.make ~https:(Some https) net, sw

let get ~client ~sw uri =
  let resp, body = Cohttp_eio.Client.get ~sw client uri in
  let code = Http.Status.to_int (Http.Response.status resp) in
  if code >= 300 && code < 400 then
    match Http.Header.get (Http.Response.headers resp) "location" with
    | Some loc -> Cohttp_eio.Client.get ~sw client (Uri.of_string loc)
    | None -> failwith "redirect with no Location header"
  else (resp, body)

(** Download a .tsv.gz from IMDB and decompress to [out_path].
    Streams data: HTTP body → gzip decompressor → file, with no
    accumulation in memory beyond the two IO-sized buffers. *)
let fetch_tsv ~env:_ ~sw ~client ~out_path filename =
  let url = Printf.sprintf "%s/%s.gz" imdb_base_url filename in
  Printf.printf "Downloading %s...\n%!" filename;
  let _resp, body = get ~client ~sw (Uri.of_string url) in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let cstruct = Cstruct.create De.io_buffer_size in
  let eof = ref false in
  let tmp_path = out_path ^ ".tmp" in
  let oc = open_out_bin tmp_path in
  let refill buf =
    if !eof then 0
    else
      let len =
        try Eio.Flow.single_read body cstruct
        with End_of_file -> eof := true; 0
      in
      for j = 0 to len - 1 do
        Bigarray.Array1.set buf j (Char.chr (Cstruct.get_uint8 cstruct j))
      done;
      len
  in
  let flush buf len =
    let bytes = Bytes.create len in
    for j = 0 to len - 1 do
      Bytes.set bytes j (Bigarray.Array1.get buf j)
    done;
    output_bytes oc bytes
  in
  let result = Gz.Higher.uncompress ~refill ~flush i o in
  close_out oc;
  (match result with
   | Ok _metadata -> ()
   | Error (`Msg msg) ->
     (try Sys.remove tmp_path with _ -> ());
     failwith ("Decompression failed: " ^ msg));
  Sys.rename tmp_path out_path;
  Printf.printf "Saved %s\n%!" out_path
