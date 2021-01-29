open Kernel
open Api

open Basic

let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
  let parser       = ref Parsers.Parser.Legacy in
  let deprecated old_flag new_flag spec =
    let warning () =
      Debug.(debug d_warn)
        "[DEPRECATED] Flag %s is deprecated ! Use %s instead.@." old_flag new_flag in
    (old_flag,Arg.Tuple [Arg.Unit warning; spec], "")
  in
  let options = Arg.align
    [ ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )
    ; ( "-I"
      , Arg.String Files.add_path
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
      t : (typing)   provides information about type checking of terms
      s : (SR)       provides information about subject reduction checking of terms
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
    ; ( "--sr-check"
      , Arg.Int (fun i -> Srcheck.srfuel := i)
      , "LVL Sets the level of subject reduction checking to LVL.
                   Default value is 1. Values < 0 may not terminate on
                   rules that do not preserve typing. " )
    ; ( "--snf"
      , Arg.Set Env.errors_in_snf
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
    ; ( "--sukerujo"
      , Arg.Unit (fun () -> parser := Parsers.Parser.Sukerujo)
      , " Use sukerujo syntax" )
    ; ( "--version"
      , Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version)
      , " Prints the version number" )
    (* Deprecated flags. TODO: Remove them from the argument parsing. *)
    ; deprecated "-errors-in-snf" "--snf" (Arg.Set Env.errors_in_snf)
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
  let open Processor in
  let processor = if !beautify then PrettyPrinter else TypeChecker in
  let hook_after env exn =
    match exn with
    | None ->
      if not !beautify then
        begin
          Errors.success (Env.get_filename env)
        end;
      if !export then Env.export env;
      Confluence.finalize ()
    | Some (env, lc, e) -> Env.fail_env_error env lc e
  in
  let hook =
    {
      before = (fun _ -> Confluence.initialize ());
      after = hook_after
    }
  in
  let parser = !parser in
  Processor.handle_files ~parser files ~hook processor;
  match !run_on_stdin with
  | None   -> ()
  | Some m ->
    let input = Parsers.Parser.input_from_stdin ~parser (Basic.mk_mident m) in
    Processor.handle_input input processor
