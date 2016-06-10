open Basics
open Pp

(* ********************************* *)

let verbose = ref false
let only_meta = ref false
let apply_on_rules = ref false

let sg_meta = ref (Signature.make (hstring "noname"))

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c;
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  ) else
    Printf.ifprintf stderr fmt

(* the signature contains only meta rules and rules imported via previous files *)

let normalize ty =
  if !only_meta then
    Basics.do_beta := false;
  let ty' = Reduction.snf !sg_meta ty in
  if !only_meta then
    Basics.do_beta := true; 
  ty'

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Format.printf "#NAME %a.@.@." print_ident name;
  sg_meta := Signature.make (hstring  (string_of_ident name));
  Confluence.initialize ()

let mk_declaration lc id pty : unit = 
  eprint lc "Declaration of constant '%a'." pp_ident id;
  let pty' = normalize pty in
  Format.printf "@[<2>%a :@ %a.@]@.@." print_ident id print_term pty';
  Signature.add_declaration !sg_meta lc id pty   (*
  match Env.declare_constant lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e  *)

let mk_definable lc id pty : unit = 
  eprint lc "Declaration of definable '%a'." pp_ident id;
  let pty' = normalize pty in
  Format.printf "@[<2>def %a :@ %a.@]@.@." print_ident id print_term pty';
  Signature.add_definable !sg_meta lc id pty'  (*
  match Env.declare_definable lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e *)

let mk_definition lc id pty_opt pte : unit = 
  let pty = match pty_opt with | None -> Typing.inference !sg_meta pte | Some(ty) -> ty in
  let pty' = normalize pty in
  let pte' = normalize pte in
  Signature.add_definable !sg_meta lc id pty;
  if not !only_meta then 
    Signature.add_rules !sg_meta [([], Rule.Pattern (lc,Signature.get_name !sg_meta, id, []), pte')];
  begin
  match pty_opt with
  | None ->
    Format.printf "@[<hv2>def %a :=@ %a.@]@.@." print_ident id print_term pte'
  | Some _ ->
    Format.printf "@[<hv2>def %a :@ %a@ :=@ %a.@]@.@." print_ident id print_term pty' print_term pte'
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

let normalize_rule ((c,p,t),m) = ((c, normalize_pattern p, normalize t),m)


let mk_rules  = function
  | [] -> ()
  | (((_,pat,_), _)::_) as lst ->
    begin
      let (l,md,id) = get_infos pat in
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
	Format.printf "@[<v0>%a@].@.@." (print_list "" print_rule) (List.map normalize_rule lst)
      else
	Format.printf "@[<v0>%a@].@.@." (print_list "" print_rule) lst
    (*  match Env.add_rules lst' with
      | OK lst2 ->
        List.iter ( fun (ctx,pat,rhs) ->
            eprint (Rule.get_loc_pat pat) "%a" Rule.pp_rule2 (ctx,pat,rhs)
          ) lst2 ;
      | Err e -> Errors.fail_env_error e *)
    end
		  
let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  ( if !export then
    if not (Signature.export !sg_meta) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ()) );
  Confluence.finalize ()
