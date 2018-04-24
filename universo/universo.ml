open Basic

module Universo =
struct
  let mk_entry e = failwith "todo"

  let solve () =
    let cs = Constraints.export () in
    let i, model = Export.Z3.solve cs in
    (i, model)
end

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file ;
  let md = Env.init file in
  let entries = Parser.parse_channel md input in
  Errors.success "File '%s' was successfully parsed." file ;
  List.iter (Universo.mk_entry md) entries ;
  Errors.success "File '%s' was successfully checked by universo." file ;
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ()) ;
  close_in input ;
  let file = Filename.concat output (string_of_mident md ^ ".dk") in
  (md, Format.formatter_of_out_channel (open_out file), entries)

let print_file model (md, fmt, entries) =
  List.iter
    (fun x -> Pp.print_entry fmt (Reconstruction.reconstruction model x))
    entries ;
  Errors.success "File '%a.dk' was fully reconstructed." pp_mident md

let print_files model = List.iter (print_file model)

let mk_cfg elabonly checkonly debug =
  if elabonly && checkonly then
    begin
      Printf.eprintf "-elaboration-only and -checking-only are mutually exlucisves";
      exit 4
    end;
  if elabonly then
    Cfg.set_checking false
  else if checkonly || elabonly then
    begin
    Cfg.set_solving false;
    Cfg.set_debug debug
  end

let _ =
  let export = ref false in
  let output_dir = ref "/tmp" in
  let elaboration_only = ref false in
  let checking_only = ref false in
  let debug_mode = ref 0 in
  let set_output_dir s = output_dir := s in
  let set_universo_debug_mode i = debug_mode := i in
  let options =
    Arg.align
      [ ("-d", Arg.Int Basic.set_debug_mode, "N sets the debuging level to N")
      ; ("-e", Arg.Set export, " Generates an object file (\".dko\")")
      ; ( "--output-dir"
        , Arg.String set_output_dir
        , " Directory to print the files by default /tmp is used" )
      ; ("--elaboration-only", Arg.Set elaboration_only, "(debug) option")
      ; ("--checking-only", Arg.Set checking_only, "(debug) option")
      ; ("-dd", Arg.Int set_universo_debug_mode, "Print debug informations in md.universo")
      ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage ;
    List.rev !files
  in
  Rule.allow_non_linear := true ;
  mk_cfg !elaboration_only !checking_only !debug_mode;
  try
    let fmtentries' = List.map (run_on_file !output_dir !export) files in
    let _, model = Universo.solve () in
    Errors.success "Constraints were successfully solved with Z3." ;
    print_files model fmtentries'
  with
  | Sys_error err ->
      Printf.eprintf "ERROR %s.\n" err ;
      exit 1
  | Exit -> exit 3
