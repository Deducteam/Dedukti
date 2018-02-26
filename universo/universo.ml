open Basic
open Pp
open Rule
open Parser

module C = Constraints

module Checker =
struct
  let mk_entry md e =
    match e with
    | Decl(lc,id,st,ty)       ->
      begin
        match Env.declare lc id st ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error e
      end
    | Def(lc,id,opaque,ty,te) ->
      begin
        let define = if opaque then Env.define_op else Env.define in
        match define lc id te ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error e
      end
    | Rules(rs)               ->
      begin
        let open Rule in
        match Env.add_rules rs with
        | OK rs -> ()
        | Err e -> Errors.fail_env_error e
      end
    | Eval(_,red,te)          ->
      begin
        match Env.reduction ~red te with
        | OK te -> Format.printf "%a@." Pp.print_term te
        | Err e -> Errors.fail_env_error e
      end
    | Infer(_,red,te)         ->
      begin
        match Env.infer te with
        | OK ty ->
          begin
            match Env.reduction ~red ty with
            | OK ty -> Format.printf "%a@." Pp.print_term ty
            | Err e -> Errors.fail_env_error e
          end
        | Err e -> Errors.fail_env_error e
      end
    | Check(_,assrt,neg,test) ->
      begin
        match test with
        | Convert(t1,t2) ->
          begin
            match Env.are_convertible t1 t2 with
            | OK ok when ok = not neg -> if not assrt then Format.printf "YES@."
            | OK _  when assrt        -> failwith "Assertion failed."
            | OK _                    -> Format.printf "NO@."
            | Err e                   -> Errors.fail_env_error e
          end
        | HasType(te,ty) ->
          begin
            match Env.check te ty with
            | OK () when not neg -> if not assrt then Format.printf "YES@."
            | Err _ when neg     -> if not assrt then Format.printf "YES@."
            | OK () when assrt   -> failwith "Assertion failed."
            | Err _ when assrt   -> failwith "Assertion failed."
            | _                  -> Format.printf "NO@."
          end
      end
    | DTree(lc,m,v)           ->
      begin
        let m = match m with None -> Env.get_name () | Some m -> m in
        let cst = mk_name m v in
        match Env.get_dtree lc cst with
        | OK (Some(i,g)) -> Format.printf "%a\n" Dtree.pp_rw (cst,i,g)
        | _              -> Format.printf "No GDT.@."
      end
    | Print(_,s)              ->
      Format.printf "%s@." s
    | Name(_,n)               ->
      if not (mident_eq n md) then
        Printf.eprintf "[Warning] invalid #NAME directive ignored.\n%!"
end

module Indent =
struct
  let mk_entry fmt e =
  let open Format in
  match e with
  | Decl(_,id,stat,ty)      ->
      let stat = if stat = Signature.Definable then "def " else "" in
      fprintf fmt "@[<2>%s%a :@ %a.@]@.@." stat print_ident id print_term ty
  | Def(_,id,opaque,ty,te)  ->
      let key = if opaque then "thm" else "def" in
      begin
        match ty with
        | None    -> fprintf fmt "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                       print_ident id print_term te
        | Some ty -> fprintf fmt "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                       print_ident id print_term ty print_term te
      end
  | Rules(rs)               ->
      fprintf fmt "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) rs
  | Eval(_,cfg,te)          ->
      fprintf fmt "#EVAL%a %a.@." print_red_cfg cfg print_term te
  | Infer(_,cfg,te)         ->
      fprintf fmt "#INFER%a %a.@." print_red_cfg cfg print_term te
  | Check(_,assrt,neg,test) ->
      let cmd = if assrt then "#ASSERT" else "#CHECK" in
      let neg = if neg then "NOT" else "" in
      begin
        match test with
        | Convert(t1,t2) ->
            fprintf fmt "%s%s %a ==@ %a.@." cmd neg print_term t1 print_term t2
        | HasType(te,ty) ->
            fprintf fmt "%s%s %a ::@ %a.@." cmd neg print_term te print_term ty
      end
  | DTree(_,m,v)            ->
      begin
        match m with
        | None   -> fprintf fmt "#GDT %a.@." print_ident v
        | Some m -> fprintf fmt "#GDT %a.%a.@." print_mident m print_ident v
      end
  | Print(_, str)           ->
      fprintf fmt "#PRINT %S.@." str
  | Name(_,_)               ->
      ()
end

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  Env.init md;
  let entries = Parser.parse_channel md input in
  Errors.success "File '%s' was successfully parsed." file;
  let entries = List.map (Elaboration.elaboration (Env.get_signature ())) entries in
  Errors.success "File '%s' was successfully elaborated." file;
  List.iter (Checker.mk_entry md) entries;
  Errors.success "File '%s' was successfully checked." file;
  let model = Export.Z3.solve (Constraints.Naive.export ()) in
  Errors.success "File '%s' was successfully solved with Z3." file;
  let entries = List.map (Reconstruction.reconstruction model) entries in
  Errors.success "File '%s' was successfully reconstructed." file;
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ());
  close_in input;
  let file = Filename.concat output (string_of_mident md ^ ".dk") in
  (Format.formatter_of_out_channel (open_out file),
   entries)

let print_file (fmt,entries) =
  List.iter (Indent.mk_entry fmt) entries

let print_files = List.iter print_file

let _ =
  let export = ref false in
  let output_dir = ref "/tmp" in
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
      , " Directory to print the files by default /tmp is used" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  Rule.allow_non_linear := true;
  try
    print_files (List.map (run_on_file !output_dir !export) files);
  with
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
