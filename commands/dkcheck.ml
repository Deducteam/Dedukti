open Term
open Basic
open Parser

let verbose = ref false

let eprint lc fmt =
  if !verbose then
    let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline ()) Format.err_formatter fmt
  else Format.ifprintf Format.err_formatter fmt

let mk_entry md e =
  match e with
  | Decl(lc,id,st,ty)       ->
      begin
        eprint lc "Declaration of constant '%a'." pp_ident id;
        match Env.declare lc id st ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error e
      end
  | Def(lc,id,opaque,ty,te) ->
      begin
        let opaque_str = if opaque then " (opaque)" else "" in
        eprint lc "Definition of symbol '%a'%s." pp_ident id opaque_str;
        let define = if opaque then Env.define_op else Env.define in
        match define lc id te ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error e
      end
  | Rules(rs)               ->
      begin
        let open Rule in
        let get_infos p =
          match p with
          | Pattern(l,cst,_) -> (l,cst)
          | _                -> (dloc,mk_name (mk_mident "") qmark)
        in
        let r = List.hd rs in (* cannot fail. *)
        let (l,cst) = get_infos r.pat in
        eprint l "Adding rewrite rules for '%a'" pp_name cst;
        match Env.add_rules rs with
        | OK rs -> List.iter (eprint (get_loc_pat r.pat) "%a" pp_typed_rule) rs
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
              | OK ok when ok = not neg -> Format.printf "YES@."
              | OK _  when assrt        -> failwith "Assertion failed."
              | OK _                    -> Format.printf "NO@."
              | Err e                   -> Errors.fail_env_error e
            end
        | HasType(te,ty) ->
            begin
              match Env.check te ty with
              | OK () when not neg -> Format.printf "YES@."
              | Err _ when neg     -> Format.printf "YES@."
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

let run_on_stdin        = ref false

let export = ref false

let print_version () =
  Printf.printf "Dedukti %s\n%!" Version.version

let default_mident = ref None

let set_default_mident md = default_mident := Some md

let args = [
  ("-v"      , Arg.Set    verbose                , "Verbose mode" ) ;
  ("-d"      , Arg.Int    Basic.set_debug_mode   , "Debug mode" ) ;
  ("-e"      , Arg.Set    export                 , "Create a .dko" ) ;
  ("-nc"     , Arg.Clear  Errors.color           , "Disable colored output" ) ;
  ("-stdin"  , Arg.Set    run_on_stdin           , "Use standart input" ) ;
  ("-r"      , Arg.Set    Signature.ignore_redecl, "Ignore redeclaration" ) ;
  ("-version", Arg.Unit   print_version          , "Version" ) ;
  ("-coc"    , Arg.Set    Typing.coc             , "Typecheck the Calculus of Construction" ) ;
  ("-autodep", Arg.Set    Signature.autodep      ,
   "Automatically handle dependencies (experimental)") ;
  ("-I"      , Arg.String Basic.add_path         , "Add a directory to load path");
  ("-errors-in-snf",
               Arg.Set    Errors.errors_in_snf   , "Normalize the types in error messages");
  ("-cc"     , Arg.String Confluence.set_cmd     , "Set the external confluence checker");
  ("-nl"     , Arg.Set    Rule.allow_non_linear  , "Allow non left-linear rewrite rules");
  ("-module" , Arg.String set_default_mident     , "Give a default name to the current module");
]



let run_on_file file =
  let input = open_in file in
  Basic.debug 1 "Processing file '%s'..." file;
  let md =  Basic.mk_mident (match !default_mident with None -> file | Some str -> str) in
  Env.init md;
  Confluence.initialize ();
  Parser.handle_channel md (mk_entry md) input;
  Errors.success "File '%s' was successfully checked." file;
  ( if !export then
    if not (Env.export ()) then
      Errors.fail Basic.dloc "Fail to export module '%a'." Basic.pp_mident (Env.get_name ()) );
  Confluence.finalize ();
  close_in input

let _ =
  try
    begin
      Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
      if !run_on_stdin then (
        let md = Basic.mk_mident (
            match !default_mident with
            | None -> Basic.debug 0 "[Warning] no module name given"; "stdin"
            | Some str -> str)
        in
        Env.init md;
        Parser.handle_channel md (mk_entry md) stdin;
        Errors.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit          -> exit 3
