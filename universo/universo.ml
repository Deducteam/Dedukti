open Basic
open Pp
open Rule
open Entry
module C = Constraints

module Checker = struct
  let mk_entry md e =
    match e with
    | Decl (lc, id, st, ty) -> (
      match Uenv.declare lc id st ty with
      | OK () -> ()
      | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
    | Def (lc, id, opaque, ty, te) -> (
        let define = if opaque then Uenv.define_op else Uenv.define in
        match define lc id te ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
    | Rules rs -> (
        let open Rule in
        match Uenv.add_rules rs with
        | OK rs -> ()
        | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
    | Eval (_, red, te) -> (
      match Uenv.reduction ~red te with
      | OK te -> Format.printf "%a@." Pp.print_term te
      | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
    | Infer (_, red, te) -> (
      match Uenv.infer te with
      | OK ty -> (
        match Uenv.reduction ~red ty with
        | OK ty -> Format.printf "%a@." Pp.print_term ty
        | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
      | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
    | Check (_, assrt, neg, test) -> (
      match test with
      | Convert (t1, t2) -> (
        match Uenv.are_convertible t1 t2 with
        | OK ok when ok = not neg -> if not assrt then Format.printf "YES@."
        | OK _ when assrt -> failwith "Assertion failed."
        | OK _ -> Format.printf "NO@."
        | Err e -> Errors.fail_env_error (Uenv.get_signature ()) e )
      | HasType (te, ty) ->
        match Uenv.check te ty with
        | OK () when not neg -> if not assrt then Format.printf "YES@."
        | Err _ when neg -> if not assrt then Format.printf "YES@."
        | OK () when assrt -> failwith "Assertion failed."
        | Err _ when assrt -> failwith "Assertion failed."
        | _ -> Format.printf "NO@." )
    | DTree(lc,m,v) ->
      begin
        let m = match m with None -> Env.get_name () | Some m -> m in
        let cst = mk_name m v in
        match Env.get_dtree lc cst with
        | OK forest ->
          Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
        | Err e -> Errors.fail_signature_error e
      end
    | Print (_, s) -> Format.printf "%s@." s
    | Name (_, n) ->
        if not (mident_eq n md) then
          Printf.eprintf "[Warning] invalid #NAME directive ignored.\n%!"
    | Require (lc, md) ->
      match Uenv.import lc md with
      | OK () -> ()
      | Err e -> Errors.fail_signature_error e


  let mk_entry md e = mk_entry md e
end

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file ;
  let md = Uenv.init file in
  let entries = Parser.parse_channel md input in
  Errors.success "File '%s' was successfully parsed." file ;
  let entries =
    List.map (Elaboration.elaboration (Uenv.get_signature ())) entries
  in
  Errors.success "File '%s' was successfully elaborated." file ;
  List.iter (Checker.mk_entry md) entries ;
  Errors.success "File '%s' was successfully checked." file ;
  if export && not (Uenv.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Uenv.get_name ()) ;
  close_in input ;
  let file = Filename.concat output (string_of_mident md ^ ".dk") in
  (md, Format.formatter_of_out_channel (open_out file), entries)


let print_file model (md, fmt, entries) =
  Errors.success "File '%a.dk' was fully reconstructed." pp_mident md ;
  List.iter
    (fun x -> Pp.print_entry fmt (Reconstruction.reconstruction model x))
    entries


let print_files model = List.iter (print_file model)

let _ =
  let export = ref false in
  let output_dir = ref "/tmp" in
  let set_output_dir s = output_dir := s in
  let options =
    Arg.align
      [ ("-d", Arg.Int Basic.set_debug_mode, "N sets the debuging level to N")
      ; ("-e", Arg.Set export, " Generates an object file (\".dko\")")
      ; ( "--output-dir"
        , Arg.String set_output_dir
        , " Directory to print the files by default /tmp is used" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage ;
    List.rev !files
  in
  Rule.allow_non_linear := true ;
  try
    let entries' = List.map (run_on_file !output_dir !export) files in
    let _, model = Uenv.solve () in
    Errors.success "Constraints were successfully solved with Z3." ;
    print_files model entries'
  with
  | Sys_error err ->
      Printf.eprintf "ERROR %s.\n" err ;
      exit 1
  | Exit -> exit 3
