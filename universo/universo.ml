open Basic
open Pp
open Rule
open Parser

let output_dir = ref "/tmp"

let set_output_dir s = output_dir := s

type ast = entry list

let entries : (entry list) ref  = ref []

module C = Constraints

let _ = Env.init (Basic.mk_mident "noname")

let export = ref false

let file = ref None

let open_module mid =
  let name = Format.sprintf "%s/%s.dk" !output_dir (string_of_mident mid) in
  file := Some (open_out name)

let close_module mid =
  match !file with
  | None -> assert false
  | Some f ->
    close_out f;
    file := None

let find_fmt () =
  match !file with
  | None -> Format.std_formatter
  | Some f ->
    if  !output_dir = "/tmp" then
        Format.std_formatter
    else  Format.formatter_of_out_channel f

open Term

let run_on_stdin = ref false

let args = [
  ("-d"    , Arg.Int Basic.set_debug_mode,   "Debug mode" ) ;
  ("-stdin", Arg.Set run_on_stdin,              "Use standart input" ) ;
  ("-errors-in-snf", Arg.Set    Errors.errors_in_snf   , "Normalize the types in error messages");
  ("-nl", Arg.Set Rule.allow_non_linear, "Allow non left-linear rewrite rules");
  ("-I"      , Arg.String Basic.add_path         , "Add a directory to load path");
  ("-log", Arg.String Constraints.Log.set_log_file, "Put log informations in a file");
  ("-output-dir", Arg.String set_output_dir, "Specify an output directory")]

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  let fmt =
    if output = "" then Format.std_formatter
    else
      let oc = open_out (Filename.concat output file) in
      Format.formatter_of_out_channel oc
  in
  Env.init md;
  let entries = Parser.parse_channel md input in
  let entries = List.map (Elaboration.elaboration (Env.get_signature ())) entries in
  Errors.success "File '%s' was successfully parsed." file;
  List.iter (Dkcheck.mk_entry md) entries;
  Errors.success "File '%s' was successfully checked." file;
  let model = Export.Z3.solve (Constraints.Naive.export ()) in
  Errors.success "Constraints solved.";
  let entries' = List.map (Reconstruction.reconstruction model) entries in
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ());
  close_in input

let _ =
  let export = ref false in
  let output_dir = ref "" in
  let set_output_dir s = output_dir := s in
  let options = Arg.align
      [ ( "-d"
        , Arg.Int Basic.set_debug_mode
        , "N sets the debuging level to N" )
      ; ( "-e"
      , Arg.Set export
        , " Generates an object file (\".dko\")" )
      ; ( "--output-dir"
      , Arg.String set_output_dir
      , " Directory to print the files" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  Rule.allow_non_linear := true;
  try
    List.iter (run_on_file !output_dir !export) files;
  with
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
