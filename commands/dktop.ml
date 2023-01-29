open Kernel
open Parsers
open Api

let top config =
  Config.init config;
  let input = Parser.from_stdin (Basic.mk_mident "<top level>") in
  Format.printf "\tDedukti (%s)@\n@." Version.version;
  let load_path = Config.load_path config in
  Processor.handle_input load_path input Processor.top_level

let cmd_t = Cmdliner.Term.(const top $ Config.t)

let cmd =
  let doc = "Run a read-eval-print-loop using dk-check." in
  Cmdliner.Cmd.(v (info "top" ~doc ~version:"%%VERSION%%") cmd_t)
