open Kernel
open Api
open Basic

let _ =
  let run_on_stdin = ref None in
  let export = ref false in
  let beautify = ref false in
  let deprecated old_flag new_flag spec =
    let warning () =
      Debug.(debug d_warn)
        "[DEPRECATED] Flag %s is deprecated ! Use %s instead.@." old_flag
        new_flag
    in
    (old_flag, Arg.Tuple [Arg.Unit warning; spec], "")
  in
  let options =
    Arg.align
      [
        ("-e", Arg.Set export, " Generates an object file (\".dko\")");
        ( "-I",
          Arg.String Files.add_path,
          "DIR Adds the directory DIR to the load path" );
        ( "-d",
          Arg.String Env.set_debug_mode,
          "FLAGS Enables debugging for the given flags.\n\
          \    Available flags:\n\
          \      q : (quiet)    disables all warnings\n\
          \      n : (notice)   notifies about which symbol or rule is \
           currently treated\n\
          \      o : (module)   notifies about loading of an external module \
           (associated\n\
          \                     to the command #REQUIRE)\n\
          \      c : (confluence) notifies about information provided to the \
           confluence\n\
          \                     checker (when option --confluence used)\n\
          \      u : (rule)     provides information about type checking of \
           rules\n\
          \      t : (typing)   provides information about type checking of \
           terms\n\
          \      s : (SR)       provides information about subject reduction \
           checking of terms\n\
          \      r : (reduce)   provides information about reduction performed \
           in terms\n\
          \      m : (matching) provides information about pattern matching" );
        ( "-v",
          Arg.Unit (fun () -> Env.set_debug_mode "montru"),
          " Verbose mode (equivalent to -d 'montru')" );
        ( "-q",
          Arg.Unit (fun () -> Env.set_debug_mode "q"),
          " Quiet mode (equivalent to -d 'q')" );
        ("--no-color", Arg.Clear Errors.color, " Disables colors in the output");
        ("-nc", Arg.Clear Errors.color, "");
        ( "--stdin",
          Arg.String (fun n -> run_on_stdin := Some n),
          "MOD Parses standard input using module name MOD" );
        ( "--coc",
          Arg.Set Typing.coc,
          " [EXPERIMENTAL] Allows the declaration of symbols whose type\n\
          \                   contains Type in the left-hand side of a product\n\
          \                   (Similar to the logic of the Calculus of \
           Constructions)" );
        ("--ll", Arg.Set Env.check_ll, " Checks left linearity of rules.");
        ( "--eta",
          Arg.Tuple [Arg.Set Reduction.eta; Arg.Clear Env.check_arity],
          " Allows the conversion test to use eta." );
        ( "--type-lhs",
          Arg.Set Typing.fail_on_unsatisfiable_constraints,
          " Forbids rules with untypable left-hand side" );
        ( "--sr-check",
          Arg.Int (fun i -> Srcheck.srfuel := i),
          "LVL Sets the level of subject reduction checking to LVL.\n\
          \                   Default value is 1. Values < 0 may not terminate \
           on\n\
          \                   rules that do not preserve typing. " );
        ( "--snf",
          Arg.Set Env.errors_in_snf,
          " Normalizes all terms printed in error messages" );
        ( "--db",
          Arg.Set Pp.print_db_enabled,
          " Prints De Bruijn indices in error messages" );
        ( "--confluence",
          Arg.String Confluence.set_cmd,
          "CMD Set the external confluence checker command to CMD" );
        ( "--beautify",
          Arg.Set beautify,
          " Pretty printer. Print on the standard output" );
        ( "--version",
          Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version),
          " Prints the version number" )
        (* Deprecated flags. TODO: Remove them from the argument parsing. *);
        deprecated "-errors-in-snf" "--snf" (Arg.Set Env.errors_in_snf);
        deprecated "-cc" "--confluence" (Arg.String Confluence.set_cmd);
        deprecated "-eta" "--eta"
          (Arg.Tuple [Arg.Set Reduction.eta; Arg.Clear Env.check_arity]);
        deprecated "-coc" "--coc" (Arg.Set Typing.coc);
        deprecated "-nl" "no flag" (Arg.Unit ignore);
        deprecated "-version" "--version"
          (Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version));
      ]
  in

  let usage =
    Format.sprintf
      "Usage: %s [OPTION]... [FILE]...\n\
       Type checks the given Dedukti FILE(s).\n\
       For more information see https://github.com/Deducteam/Dedukti.\n\
       Available options:" Sys.argv.(0)
  in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  if !beautify && !export then (
    Format.eprintf "Beautify and export cannot be set at the same time@.";
    exit 2);
  let open Processor in
  let processor = if !beautify then PrettyPrinter else TypeChecker in
  let hook_after env exn =
    match exn with
    | None              ->
        if not !beautify then Errors.success (Env.get_filename env);
        if !export then Env.export env;
        Confluence.finalize ()
    | Some (env, lc, e) -> Env.fail_env_error env lc e
  in
  let hook =
    {before = (fun _ -> Confluence.initialize ()); after = hook_after}
  in
  Processor.handle_files files ~hook processor;
  match !run_on_stdin with
  | None   -> ()
  | Some m ->
      let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
      Processor.handle_input input processor
