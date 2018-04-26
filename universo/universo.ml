open Basic

module Universo =
struct

  let get_rule_name (r:'a Rule.rule) =
    let open Rule in
    match r.name with
    | Gamma(_,name) -> name
    | _ -> assert false

  let elab_oc = ref stdout

  let init file =
    let md = Env.init file in
    Cfg.set_signature (Env.get_signature());
    elab_oc := open_out (Filename.concat "/tmp" (string_of_mident md ^ ".dk"));
    md

  let debug_mode = ref false

  let print_vars_decl md fmt e =
    let open Entry in
    let open Cic in
    let get_vars name = Cfg.get_uvars name in
    let vars =
      match e with
      | Decl(_,id,_,_) ->
        get_vars (mk_name md id)
      | Def(_,id,_,_,_) ->
        get_vars (mk_name md id)
      | Rules(rs) ->
        List.fold_left (fun vars r ->
            ISet.union (get_vars (get_rule_name r)) vars) ISet.empty rs
      | _ -> ISet.empty
    in
    let vars_entries = ISet.fold
        (fun id l -> (Decl(dloc, id, Signature.Definable, mk_sort))::l) vars [] in
    List.iter (Format.fprintf fmt "%a" Pp.print_entry) vars_entries

  let print_entry md fmt e =
    let open Entry in
    Format.fprintf fmt "%a@." (print_vars_decl md) e;
    Format.fprintf fmt "%a" Pp.print_entry e

  let mk_entry md e =
    let open Elaboration in
    let open Format in
    let e' = elaboration md e in
    if Cfg.get_checking () then
      Checker.check md e';

    if !debug_mode then
      fprintf (formatter_of_out_channel !elab_oc) "%a@." (print_entry md) e'

  let ending export =
    close_out !elab_oc;
    if export && not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ())


  let solve () =
    let cs = Constraints.export () in
    let i, model = Export.Z3.solve cs in
    (i, model)
end

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file ;
  let md = Universo.init file in
  let outfile = Filename.concat output (string_of_mident md ^ ".dk") in
  Cfg.add_fmt md outfile;
  let entries = Parser.parse_channel md input in
  Errors.success "File '%s' was successfully parsed." file ;
  List.iter (Universo.mk_entry md) entries ;
  Errors.success "File '%s' was successfully checked by universo." file ;
  Universo.ending export;
  close_in input ;
  (md, Format.formatter_of_out_channel (open_out outfile), entries)

let print_file model (md, fmt, entries) =
  List.iter
    (fun x -> Pp.print_entry fmt (Reconstruction.reconstruction model x))
    entries ;
  Errors.success "File '%a.dk' was fully reconstructed." pp_mident md

let print_files model = List.iter (print_file model)

let update_cfg elabonly checkonly =
  if elabonly && checkonly then
    begin
      Printf.eprintf "-elaboration-only and -checking-only are mutually exclusives";
      exit 4
    end;
  if elabonly then
    Cfg.set_checking false
  else if checkonly || elabonly then
    Cfg.set_solving false

let _ =
  let export = ref false in
  let output_dir = ref "/tmp" in
  let elaboration_only = ref false in
  let checking_only = ref false in
  let set_output_dir s = output_dir := s in
  let options =
    Arg.align
      [ ("-d", Arg.Int Basic.set_debug_mode, "N sets the debuging level to N")
      ; ("-e", Arg.Set export, " Generates an object file (\".dko\")")
      ; ( "--output-dir"
        , Arg.String set_output_dir
        , " Directory to print the files by default /tmp is used" )
      ; ("--elaboration-only", Arg.Set elaboration_only, " (debug) option")
      ; ("--checking-only", Arg.Set checking_only, " (debug) option")
      ; ("--debug", Arg.Set Universo.debug_mode, " Print debug informations in universo")
      ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage ;
    List.rev !files
  in
  Rule.allow_non_linear := true ;
  update_cfg !elaboration_only !checking_only;
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
