open Kernel
open Parsers
open Api

let top config =
  Config.init config;
  let input = Parser.input_from_stdin (Basic.mk_mident "<top level>") in
  Format.printf "\tDedukti (%s)@\n@." Version.version;
  let load_path = Config.load_path config in
  Processor.handle_input ~load_path ~input Processor.TopLevel

let cmd_t = Cmdliner.Term.(const top $ Config.t)

let cmd =
  let doc = "Run a read-eval-print-loop using dk-check." in
  Cmdliner.Cmd.(v (info "top" ~doc) cmd_t)
