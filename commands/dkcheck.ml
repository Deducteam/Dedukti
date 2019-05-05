open Term
open Basic
open Parser
open Entry

module E = Env.Default

let mk_entry beautify md =
  if beautify then Pp.print_entry Format.std_formatter
  else E.mk_entry md


let run_on_file beautify export file =
  let input = open_in file in
  Debug.(debug Signature.D_module "Processing file '%s'..." file);
  let md = E.init file in
  Confluence.initialize ();
  Pp.set_module md;
  Parse_channel.handle md (mk_entry beautify md) input;
  if not beautify then
    Errors.success "File '%s' was successfully checked." file;
  if export then
    E.export ();
  Confluence.finalize ();
  close_in input


let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
  let options = Arg.align
    [ ( "-d"
      , Arg.String Env.set_debug_mode
      , "FLAGS enables debugging for all given flags:
      q : (quiet)    disables all warnings
      n : (notice)   notifies about which symbol or rule is currently treated
      o : (module)   notifies about loading of an external module (associated
                     to the command #REQUIRE)
      c : (confluence) notifies about information provided to the confluence
                     checker (when option -cc used)
      u : (rule)     provides information about type checking of rules
      t : (typing)   provides information about type-checking of terms
      r : (reduce)   provides information about reduction performed in terms
      m : (matching) provides information about pattern matching" )
    ; ( "-v"
      , Arg.Unit (fun () -> Env.set_debug_mode "montru")
      , " Verbose mode (equivalent to -d 'montru')" )
    ; ( "-q"
      , Arg.Unit (fun () -> Env.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q')" )
    ; ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )
    ; ( "-nc"
      , Arg.Clear Errors.color
      , " Disables colors in the output" )
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , "MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version)
      , " Prints the version number" )
    ; ( "-coc"
      , Arg.Set Typing.coc
      , " Allows to declare a symbol whose type contains Type in the
          left-hand side of a product (useful for the Calculus of Construction)" )
    ; ( "-eta"
      , Arg.Tuple [Arg.Set Reduction.eta; Arg.Clear Env.check_arity]
      , " Allows the conversion test to use eta." )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Adds the directory DIR to the load path" )
    ; ( "-ccs"
      , Arg.Set Typing.fail_on_unsatisfiable_constraints
      , " Forbids rules with unsatisfiable constraints" )
    ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalizes all terms printed in error messages" )
    ; ( "-unsafe"
      , Arg.Set Signature.unsafe
      , " Deactive sanity checks" )
    ; ( "-cc"
      , Arg.String Confluence.set_cmd
      , "CMD Set the external confluence checker command to CMD" )
    ; ( "-nl"
      , Arg.Unit (fun _ -> ())
      , " [DEPRECATED] Allow non left-linear rewriting rules (default behavior now)" )
    ; ( "--beautify"
      , Arg.Set beautify
      , " Pretty printer. Print on the standard output" )]
  in
  let usage = Format.sprintf "Usage: %s [OPTION]... [FILE]...
Type checks the given Dedukti FILE(s).
For more information see https://github.com/Deducteam/Dedukti.
Available options:" Sys.argv.(0) in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  if !beautify && !export then
    begin
      Format.eprintf "Beautify and export cannot be set at the same time@.";
      exit 2
    end;
  try
    List.iter (run_on_file !beautify !export) files;
    match !run_on_stdin with
    | None   -> ()
    | Some m ->
      let md = E.init m in
      Parse_channel.handle md (mk_entry !beautify md) stdin;
      if not !beautify
      then Errors.success "Standard input was successfully checked.\n"
  with
  | Env.EnvError (l,e) -> Errors.fail_env_error l e
  | Sys_error err  -> Errors.fail_sys_error err
