open Kernel
open Api

open Basic

module P         = Processor
module TC        = P.TypeChecker
module Printer   = P.EntryPrinter

let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
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
  let (module P:P.S with type t = unit) =
    if !beautify then (module Printer)
    else
      (module struct
        include TC
        let hook_before _ =
          Confluence.initialize ()
        let hook_success env _ =
          Errors.success (Env.get_filename env);
          if !export then Env.export env;
          Confluence.finalize ()
      end) in
  Processor.handle_files files (module P);
  match !run_on_stdin with
  | None   -> ()
  | Some m ->
    let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
    Processor.handle_input input (module P);
