open Basic
open Term
open Rule
open Parser
open Entry

let handle_file : string -> Dep.mdep_data = fun file ->
    (* Initialisation. *)
    let md = mk_mident file in
    (* Actully parsing and gathering data. *)
    let input = open_in file in
    let dep_data = Dep.handle (md,file) (fun f -> Parser.Parse_channel.handle md f input) in
    close_in input;
    dep_data

(** Output main program. *)

let output_deps : Format.formatter -> Dep.mdep_data list -> unit = fun oc data ->
  let objfile src = Filename.chop_extension src ^ ".dko" in
  let output_line : Dep.mdep_data -> unit = fun ((name, file), deps) ->
    let deps = List.map (fun (_,src) -> objfile src) deps in
    let deps = String.concat " " deps in
    Format.fprintf oc "%s : %s %s@." (objfile file) file deps
  in
  List.iter output_line data

let output_sorted : Format.formatter -> Dep.mdep_data list -> unit = fun oc data ->
  let deps = Dep.topological_sort data in
  Format.printf "%s@." (String.concat " " deps)

let _ =
  (* Parsing of command line arguments. *)
  let output  = ref stdout in
  let sorted  = ref false  in
  let args = Arg.align
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
    ; ( "-o"
      , Arg.String (fun n -> output := open_out n)
      , "FILE Outputs to file FILE" )
    ; ( "-s"
      , Arg.Set sorted
      , " Sort the source files according to their dependencies" )
    ; ( "--ignore"
      , Arg.Set Dep.ignore
      , " If some dependencies are not found, ignore them" )
    ; ( "-I"
      , Arg.String add_path
      , "DIR Add the directory DIR to the load path" ) ]
  in
  let usage = Format.sprintf "Usage: %s [OPTION]... [FILE]...
Compute the dependencies of the given Dedukti FILE(s).
For more information see https://github.com/Deducteam/Dedukti.
Available options:" Sys.argv.(0) in
  let files =
    let files = ref [] in
    Arg.parse args (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  (* Actual work. *)
  try
    let dep_data = List.map handle_file files in
    let formatter = Format.formatter_of_out_channel !output in
    let output_fun = if !sorted then output_sorted else output_deps in
    output_fun formatter dep_data;
    Format.pp_print_flush formatter ();
    close_out !output
  with
  | Dep.Dep_error dep -> Errors.fail_env_error dloc (Env.EnvErrorDep dep)
  | Sys_error err     -> Errors.fail_sys_error err
