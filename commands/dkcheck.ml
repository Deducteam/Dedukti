open Term
open Basic
open Parser
open Entry

let eprint lc fmt =
  let (l,c) = of_loc lc in
  debug 1 ("line:%i column:%i " ^^ fmt) l c

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
          | _                -> (dloc,mk_name (mk_mident "") dmark)
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
        | OK (Some dtree) -> Format.printf "%a\n" Dtree.pp_dtree dtree
        | _               -> Format.printf "No GDT.@."
      end
  | Print(_,s)              ->
      Format.printf "%s@." s
  | Name(_,n)               ->
      if not (mident_eq n md)
      then warn "Invalid #NAME directive ignored.\n%!"

let mk_entry beautify md =
  if beautify then Pp.print_entry Format.std_formatter
  else mk_entry md


let run_on_file beautify export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  Env.init md;
  Confluence.initialize ();
  Parser.handle_channel md (mk_entry beautify md) input;
  if not beautify then
    Errors.success "File '%s' was successfully checked." file;
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_mident (Env.get_name ());
  Confluence.finalize ();
  close_in input


let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
  let options = Arg.align
    [ ( "-d"
      , Arg.Int Basic.set_debug_mode
      , "N sets the verbosity level to N" )
    ; ( "-v"
      , Arg.Unit (fun _ -> Basic.set_debug_mode 1)
      , " Verbose mode (equivalent to -d 1)" )
    ; ( "-q"
      , Arg.Unit (fun _ -> Basic.set_debug_mode (-1))
      , " Quiet mode (equivalent to -d -1" )
    ; ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )
    ; ( "-nc"
      , Arg.Clear Errors.color
      , " Disable colors in the output" )
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(mk_mident n))
      , "MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun _ -> Printf.printf "Dedukti %s\n%!" Version.version)
      , " Print the version number" )
    ; ( "-coc"
      , Arg.Set Typing.coc
      , " Typecheck the Calculus of Construction" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Add the directory DIR to the load path" )
    ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalize the types in error messages" )
    ; ( "-cc"
      , Arg.String Confluence.set_cmd
      , "CMD Set the external confluence checker command to CMD" )
    ; ( "-nl"
      , Arg.Set Rule.allow_non_linear
      , " Allow non left-linear rewriting rules" )
    ; ( "--beautify"
      , Arg.Set beautify
      , " Pretty printer. Print on the standard output" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  if !beautify && !export then
    begin
      Printf.eprintf "Beautify and export cannot be set at the same time\n";
      exit 2
    end;
  try
    List.iter (run_on_file !beautify !export) files;
    match !run_on_stdin with
    | None    -> ()
    | Some md ->
        Env.init md;
        Parser.handle_channel md (mk_entry !beautify md) stdin;
        if not !beautify then
          Errors.success "Standard input was successfully checked.\n"
  with
  | Parse_error(loc,msg) ->
      let (l,c) = of_loc loc in
      Printf.eprintf "Parse error at (%i,%i): %s\n" l c msg;
      exit 1
  | Sys_error err        -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit                 -> exit 3
