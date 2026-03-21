(** CLI configuration parsing *)

type t = {
  port: int;
  data_dir: string;
}

let parse () =
  let port_term =
    let doc = "Port to listen on" in
    Cmdliner.Arg.(value & opt int 1913 & info ["p"; "port"] ~docv:"PORT" ~doc)
  in
  
  let data_dir_term =
    let doc = "Directory containing IMDB data files" in
    Cmdliner.Arg.(value & opt string "." & info ["d"; "data-dir"] ~docv:"DIR" ~doc)
  in
  
  let run p d = { port = p; data_dir = d } in
  let term = Cmdliner.Term.(const run $ port_term $ data_dir_term) in
  let info = Cmdliner.Cmd.info "rateflix-server" ~doc:"IMDB rating lookup server" in
  let cmd = Cmdliner.Cmd.v info term in
  
  match Cmdliner.Cmd.eval_value cmd with
  | Ok (`Ok config) -> config
  | _ -> exit 1
