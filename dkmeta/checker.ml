open Basic

(* ********************************* *)

let verbose = ref false
let only_meta = ref false
let apply_on_rules = ref false

let sg_meta = ref (Signature.make (hstring "noname"))

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* the signature contains only meta rules and rules imported via previous files *)

let filter_meta_rules r =
            match r with
            | Rule.Delta(md,_) -> string_of_ident md = "meta"
            | Rule.Gamma(_,md,_) -> string_of_ident md = "meta"

let normalize ty =
  if !only_meta then
    begin
      Basic.do_beta := false;
      Reduction.select (Some filter_meta_rules)
    end;
  let ty' = Reduction.snf !sg_meta ty in
  if !only_meta then
    begin
      Basic.do_beta := true;
      Reduction.select None
    end;
  ty'

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Format.printf "#NAME %a.@.@." pp_ident name;
  sg_meta := Signature.make (hstring  (string_of_ident name));
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id st ty : unit =
  eprint lc "Declaration of symbol '%a'." pp_ident id;
  let ty' = normalize ty in
  let kw = match st with
    | Signature.Static -> ""
    | Signature.Definable -> "def "
    | Signature.Injective -> "inj "
  in
  Format.printf "@[<2>%s%a :@ %a.@]@.@." kw pp_ident id Pp.print_term ty';
  Signature.add_declaration !sg_meta lc id st ty'   (*
  match Env.declare_constant lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e  *)

let mk_definition lc id pty_opt pte : unit =
  let pty = match pty_opt with | None -> Typing.inference !sg_meta pte | Some(ty) -> ty in
  let pty' = normalize pty in
  let pte' = normalize pte in
  Signature.add_declaration !sg_meta lc id Signature.Definable pty';
  let name = Rule.Delta(Signature.get_name !sg_meta, id) in
  let rule =
    { Rule.name = name ;
      Rule.ctx = [] ;
      Rule.pat = Rule.Pattern(lc, Signature.get_name !sg_meta, id, []) ;
      Rule.rhs = pte' ;
    }
  in
  if not !only_meta then
    Signature.add_rules !sg_meta [rule];
  begin
  match pty_opt with
  | None ->
    Format.printf "@[<hv2>def %a :=@ %a.@]@.@." pp_ident id Pp.print_term pte'
  | Some _ ->
    Format.printf "@[<hv2>def %a :@ %a@ :=@ %a.@]@.@." pp_ident id Pp.print_term pty' Pp.print_term pte'
  end

   (*
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  match Env.define lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e *)

let mk_opaque lc id pty_opt pte = () (*
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e *)

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let rec normalize_pattern p = Rule.(
  match p with
  | Var(loc,id,i,pl) -> Var(loc,id,i, List.map normalize_pattern pl)
  | Pattern(loc,id,i,pl) -> Pattern(loc,id,i, List.map normalize_pattern pl)
  | Lambda(loc,id,p) -> Lambda(loc,id, normalize_pattern p)
  | Brackets(t) -> Brackets(normalize t))

let normalize_rule (rule,m) =
  ({rule with
    Rule.pat = normalize_pattern rule.Rule.pat;
    Rule.rhs = normalize rule.Rule.rhs}, m)

let mk_rules  = function
  | [] -> ()
  | ((rule, _)::_) as lst ->
    begin
      let (l,md,id) = get_infos rule.Rule.pat in
      eprint l "Adding rewrite rules for '%a.%a'" pp_ident md pp_ident id;
      let lst_meta = List.map fst
	(List.filter ( fun (_,t) -> t = Preterm.MetaRule) lst ) in
      let lst' = if !only_meta then lst_meta else (List.map fst lst) in
      begin
      match Env.(Signature.(Typing.(
      try
	let lst'' = List.map (Typing.check_rule !sg_meta) lst' in
	Signature.add_rules !sg_meta lst'';
	OK lst'' with
	| SignatureError e ->  Err (EnvErrorSignature e)
	| TypingError e -> Err (EnvErrorType e)
      )))
      with
      | OK _ -> ()
      | Err e -> Errors.fail_env_error e
      end;

      if !apply_on_rules then
	Format.printf "@[<v0>%a@].@.@." (pp_list "" Pp.print_untyped_rule) (List.map normalize_rule lst)
      else
	Format.printf "@[<v0>%a@].@.@." (pp_list "" Pp.print_untyped_rule) lst
    (*  match Env.add_rules lst' with
      | OK lst2 ->
        List.iter ( fun (ctx,pat,rhs) ->
            eprint (Rule.get_loc_pat pat) "%a" Rule.pp_rule2 (ctx,pat,rhs)
          ) lst2 ;
      | Err e -> Errors.fail_env_error e *)
    end

let mk_command lc = function
  | Cmd.Whnf te ->
     Format.printf "#WHNF@ %a." Pp.print_term (normalize te)
  | Cmd.Hnf te ->
     Format.printf "#HNF@ %a." Pp.print_term (normalize te)
  | Cmd.Snf te ->
     Format.printf "#SNF@ %a." Pp.print_term (normalize te)
  | Cmd.OneStep te ->
     Format.printf "#STEP@ %a." Pp.print_term (normalize te)
  | Cmd.Conv (te1,te2) ->
     Format.printf "#CONV@ %a,@ %a."
        Pp.print_term (normalize te1)
        Pp.print_term (normalize te2)
  | Cmd.Check (te,ty) ->
     Format.printf "#CHECK@ %a,@ %a."
        Pp.print_term (normalize te)
        Pp.print_term (normalize ty)
  | Cmd.Infer te ->
     Format.printf "#INFER@ %a." Pp.print_term (normalize te)
  | Cmd.Gdt (m0,v) ->
      begin match m0 with
      | None -> Format.printf "#GDT@ %a." pp_ident v
      | Some m -> Format.printf "#GDT@ %a.%a." pp_ident m pp_ident v
      end
  | Cmd.Print str ->
     Format.printf "#PRINT \"%s\"." str
  | Cmd.Other (cmd,_) ->
     failwith ("Unknown command '"^cmd^"'.\n")

let export = ref false

let mk_ending () =
  ( if !export then
    if not (Signature.export !sg_meta) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ()) );
  Confluence.finalize ()
