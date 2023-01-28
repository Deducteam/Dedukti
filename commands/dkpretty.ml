open Api
open Kernel
open Cmdliner

let beautify config files =
  Config.init config;
  let open Processor in
  let after input _ exn =
    Option.iter (fun (_env, loc, e) -> Errors.fail_exn input loc e) exn
  in
  let hook = {before = (fun _input _env -> ()); after} in
  (* Load path is not needed since no importation is done by the
      [PrettyPrinter] processor. *)
  let load_path = Files.empty in
  Processor.(handle_files ~hook load_path ~files print);
  let f m =
    let input = Parsers.Parser.from_stdin (Basic.mk_mident m) in
    Processor.(handle_input load_path ~input print)
  in
  Option.iter f config.Config.run_on_stdin

let files =
  let doc = "Pretty print Dedukti file FILE" in
  Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let cmd_t = Term.(const beautify $ Config.t $ files)

let cmd =
  let doc = "Pretty print Dedukti files" in
  let man = [] in
  Cmd.(v (info "beautify" ~version:"%%VERSION%%" ~doc ~man) cmd_t)
