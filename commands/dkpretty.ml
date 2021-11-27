open Api
open Kernel
open Cmdliner

let beautify config files =
  Config.init config;
  let open Processor in
  let after _ exn =
    Option.iter (fun (env, lc, e) -> Env.fail_env_error env lc e) exn
  in
  let hook = {before = ignore; after} in
  Processor.handle_files files ~hook PrettyPrinter;
  let f m =
    let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
    Processor.handle_input input PrettyPrinter
  in
  Option.iter f config.Config.run_on_stdin

let files =
  let doc = "Pretty print Dedukti file FILE" in
  Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Pretty print Dedukti files" in
  let exits = Term.default_exits in
  let man = [] in
  ( Term.(const beautify $ Config.t $ files),
    Term.(info "dkpretty" ~version:"%%VERSION%%" ~doc ~exits ~man) )

let () = Term.(exit @@ eval cmd)
