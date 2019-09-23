open Term
open Basic
open Parser

module E = Env.Make(Reduction.Default)
module TypeChecker = Processor.TypeChecker(E)
module Printer = E.Printer
module ErrorHandler = Errors.Make(E)
module Beautifier = Processor.EntryPrinter(E)

let mk_entry beautify =
  if beautify
  then Beautifier.handle_entry
  else TypeChecker.handle_entry

let run_on_file beautify export file =
  let input = open_in file in
  Debug.(debug Signature.D_module "Processing file '%s'..." file);
  let sg = E.init file in
  Confluence.initialize ();
  let _ =
    try Parse_channel.handle (E.get_name sg) (mk_entry beautify sg) input;
    with Env.EnvError (md,lc,e) ->
      close_in input;
      ErrorHandler.fail_env_error sg (md,lc,e)
  in
  if not beautify then
    ErrorHandler.success "File '%s' was successfully checked." file;
  if export then E.export sg;
  Confluence.finalize ();
  close_in input

let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
  let deprecated old_flag new_flag spec =
    let warning () =
      Debug.(debug D_warn)
        "[DEPRECATED] Flag %s is deprecated ! Use %s instead.@." old_flag new_flag in
    (old_flag,Arg.Tuple [Arg.Unit warning; spec], "")
  in
  let options = Arg.align
    [ ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Adds the directory DIR to the load path" )
    ; ( "-d"
      , Arg.String Env.set_debug_mode
      , "FLAGS Enables debugging for the given flags.
    Available flags:
      q : (quiet)    disables all warnings
      n : (notice)   notifies about which symbol or rule is currently treated
      o : (module)   notifies about loading of an external module (associated
                     to the command #REQUIRE)
      c : (confluence) notifies about information provided to the confluence
                     checker (when option --confluence used)
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
    ; ( "--no-color"
      , Arg.Clear Errors.color
      , " Disables colors in the output" )
    ; ( "-nc"
      , Arg.Clear Errors.color
      , "" )
    ; ( "--stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , "MOD Parses standard input using module name MOD" )
    ; ( "--coc"
      , Arg.Set Typing.coc
      , " [EXPERIMENTAL] Allows the declaration of symbols whose type
                   contains Type in the left-hand side of a product
                   (Similar to the logic of the Calculus of Constructions)" )
    ; ( "--ll"
      , Arg.Set Env.check_ll
      , " Checks left linearity of rules." )
    ; ( "--eta"
      , Arg.Tuple [Arg.Set Reduction.eta; Arg.Clear Env.check_arity]
      , " Allows the conversion test to use eta." )
    ; ( "--type-lhs"
      , Arg.Set Typing.fail_on_unsatisfiable_constraints
      , " Forbids rules with untypable left-hand side" )
    ; ( "--snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalizes all terms printed in error messages" )
    ; ( "--db"
      , Arg.Set Pp.print_db_enabled
      , " Prints De Bruijn indices in error messages" )
    ; ( "--confluence"
      , Arg.String Confluence.set_cmd
      , "CMD Set the external confluence checker command to CMD" )
    ; ( "--beautify"
      , Arg.Set beautify
      , " Pretty printer. Print on the standard output" )
    ; ( "--version"
      , Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version)
      , " Prints the version number" )
    (* Deprecated flags. TODO: Remove them from the argument parsing. *)
    ; deprecated "-errors-in-snf" "--snf" (Arg.Set Errors.errors_in_snf)
    ; deprecated "-cc" "--confluence" (Arg.String Confluence.set_cmd)
    ; deprecated "-eta" "--eta" (Arg.Tuple [Arg.Set Reduction.eta; Arg.Clear Env.check_arity])
    ; deprecated "-coc" "--coc" (Arg.Set Typing.coc)
    ; deprecated "-nl" "no flag" (Arg.Unit ignore)
    ; deprecated "-version" "--version" (Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version))
    ]
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
      let sg = E.init m in
      let _ =
        try Parse_channel.handle (E.get_name sg) (mk_entry !beautify sg) stdin
        with Env.EnvError (md,lc,e) -> ErrorHandler.fail_env_error sg (md,lc,e)
      in
      if not !beautify
      then ErrorHandler.success "Standard input was successfully checked.\n"
  with Sys_error err -> ErrorHandler.fail_sys_error err
