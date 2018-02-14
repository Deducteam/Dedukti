open Term
open Basic
open Parser

(* ********************************* *)

let verbose = ref false

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_mident name;
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id st pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  match Env.declare lc id st pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  match Env.define lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,cst,_) -> (l,cst)
  | _ -> (dloc,mk_name (mk_mident "") qmark)

let mk_rules = Rule.( function
  | [] -> ()
  | (rule::_) as lst ->
    begin
      let (l,cst) = get_infos rule.pat in
      eprint l "Adding rewrite rules for '%a'" pp_name cst;
      match Env.add_rules lst with
      | OK lst2 ->
        List.iter ( fun rule ->
            eprint (get_loc_pat rule.pat) "%a" pp_typed_rule rule
          ) lst2 ;
      | Err e -> Errors.fail_env_error e
    end
  )

let mk_entry = function
  | Decl(lc,id,st,te) -> mk_declaration lc id st te
  | Def(lc,id,false,pty,te) -> mk_definition lc id pty te
  | Def(lc,id,true,pty,te) -> mk_opaque lc id pty te
  | Rules(rs) -> mk_rules rs
  | Eval (lc,config, te) ->
    ( match Env.reduction ~red:config te with
      | OK te -> Format.printf "%a@." Pp.print_term te
      | Err e -> Errors.fail_env_error e )
  | Check(lc,false,false,(Convert(te1,te2))) ->
    ( match Env.are_convertible te1 te2 with
      | OK true -> Format.printf "YES@."
      | OK false -> Format.printf "NO@."
      | Err e -> Errors.fail_env_error e )
  | Check(lc,false,false,HasType(te,ty)) ->
    ( match Env.check te ty with
      | OK () -> Format.printf "YES@."
      | Err e -> Errors.fail_env_error e )
  | Check(lc,_,_,_) -> failwith "unsupported right now"
  | Infer (lc,config, te) ->
    ( match Env.infer te with
      | OK ty ->
        begin
          match Env.reduction ~red:config ty with
          | OK ty' -> Format.printf "%a@." Pp.print_term ty'
          | Err e -> Errors.fail_env_error e
        end
      | Err e -> Errors.fail_env_error e )
  | DTree (lc,m0,v) ->
    let m = match m0 with None -> Env.get_name () | Some m -> m in
    let cst = mk_name m v in
    ( match Env.get_dtree lc cst with
      | OK (Some (i,g)) -> Format.printf "%a\n" Dtree.pp_rw (cst,i,g)
      | _               -> Format.printf "No GDT.@." )
  | Print(lc,str)-> Format.printf "%s@." str

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
  Parser.handle_channel md mk_entry input;
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
        Parser.handle_channel md mk_entry stdin;
        Errors.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit          -> exit 3
