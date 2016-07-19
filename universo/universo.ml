open Basics
open Pp
open Constraints
              
type entry = 
| Declaration of ident * Term.term 
| Definable of ident * Term.term
| Definition of ident * Term.term option * Term.term 
| Opaque of ident * Term.term option * Term.term 
| RewriteRule of (loc * ident) list * Rule.pattern * Term.term

type ast = entry list

let entries : (entry list) ref  = ref []
       
let reconstruction_of_entry env entry =
  match entry with
  | Declaration(id, t) -> Declaration(id, reconstruction env t)
  | Definable(id, t) -> Definable(id, reconstruction env t)
  | Definition(id, None, te) -> Definition(id, None, reconstruction env te)
  | Definition(id, Some(ty), te) -> Definition(id, Some(reconstruction env ty), reconstruction env te)
  | Opaque(id, None, te) -> Opaque(id, None, reconstruction env te)
  | Opaque(id, Some(ty), te) -> Opaque(id, Some(reconstruction env ty), reconstruction env te)
  | RewriteRule(ctx, pat, te) -> RewriteRule(ctx, pat, reconstruction env te)
                                        
module Checker = struct
 

(* ********************************* *)

let verbose = ref false

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c;
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  ) else
    Printf.ifprintf stderr fmt

(* ********************************* *)

let sg = ref (Signature.make (hstring "noname"))

let rec add_variables nv = Term.(
  let sort = mk_Const dloc (hstring "cic") (hstring "Sort") in
  List.iter (fun v -> ignore(Env.declare_constant dloc v sort)(* ;
    Format.printf "@[<2>%a :@ %a.@]@.@." print_ident v print_term sort *)) nv)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Format.printf "#NAME %a.@.@." print_ident name;
 (*  sg := Signature.make (hstring ((string_of_ident name) ^ "_")); *)
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  let pty' = Constraints.elaboration pty in
  entries := Declaration(id, pty')::!entries;
  match Env.declare_constant lc id pty' with
    | OK () -> () ;
(*      Format.printf "@[<2>%a :@ %a.@]@.@." 
	print_ident id print_term pty'; *)
    | Err e -> Errors.fail_env_error e

let mk_definable lc id pty : unit =
  eprint lc "Declaration of definable '%a'." pp_ident id;
  let pty' = Constraints.elaboration pty in
  entries := Definable(id, pty')::!entries;
  match Env.declare_definable lc id pty' with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  let pty_opt' = 
    match pty_opt with 
    | None -> None 
    | Some pty -> Some (Constraints.elaboration pty)
  in
  let pte' = Constraints.elaboration pte in
  entries := Definition(id, pty_opt', pte')::!entries;  
  match Env.define lc id pte' pty_opt' with
    | OK () -> () 
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  let pty_opt' = 
    match pty_opt with 
    | None -> None
    | Some pty -> Some (Constraints.elaboration pty)
  in
  let pte' = Constraints.elaboration pte in
  entries := Definition(id, pty_opt', pte')::!entries;    
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let mk_rules = function
  | [] -> ()
  | ((_,pat,pty)::_) as lst ->
     let lst' = List.map (fun (loc,pat, pty) ->
                    let pty' = Constraints.elaboration pty in (loc,pat,pty')) lst in
    begin
      let (l,md,id) = get_infos pat in
      eprint l "Adding rewrite rules for '%a.%a'" pp_ident md pp_ident id;
      let to_ru = List.rev_map (fun (loc, pat, te) -> RewriteRule(loc,pat,te)) lst' in
      entries := List.rev_append to_ru !entries;
      match Env.add_rules lst' with
      | OK lst2 ->
        List.iter ( fun (ctx,pat,rhs) ->
            eprint (Rule.get_loc_pat pat) "%a" Rule.pp_rule2 (ctx,pat,rhs)
          ) lst2 ;
      | Err e -> Errors.fail_env_error e
    end

let mk_command = Cmd.mk_command

let export = ref false

let print_entry entry = Pp.(
  match entry with
  | Declaration (id, ty) -> 
    Format.printf "@[%a :@;<1 2>%a.@]@." print_ident id print_term ty
  | Definable (id, ty) ->
    Format.printf "@[def %a :@;<1 2>%a.@]@." print_ident id print_term ty
  | Definition (id, None, te) -> 
    Format.printf "def @[%a :=@;<1 2>%a.@]@." print_ident id print_term te
  | Definition (id, Some(ty), te) ->
    Format.printf "def @[%a:@;<1 2>%a@;<1 2>:=@;<1 2>%a.@]@." print_ident id print_term ty print_term te
  | Opaque (id, ty_opt, te) -> failwith "TODO"
  | RewriteRule(loc, pat, te) -> Format.printf "@[%a. @]@." print_rule (loc,pat,te)
)
let print_entries entries = ignore(List.map (print_entry) entries)

let mk_ending () = (*
  if !debug_mode then print_constraints();
  if !debug_mode then print_var_of_name();
  if !debug_mode then print_entries !entries; *)
  let env = Constraints.solve() in   (*
  if !debug_mode then
    NameMap.iter _debug_print_entry univ_of_uname; *)
  
  let entries' = List.rev_map (reconstruction_of_entry env) !entries in
  print_entries entries';
  ( if !export then
      if not (Env.export ()) then
	  Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ()) );
  Confluence.finalize ()
                   
end




open Term

let run_on_stdin = ref false

module P = Parser.Make(Checker)

let parse lb = 
  try
    P.prelude Lexer.token lb ;
    while true do P.line Lexer.token lb done
  with
  | Tokens.EndOfFile -> ()
  | P.Error -> Errors.fail (Lexer.get_loc lb) 
    "Unexpected token '%s'." (Lexing.lexeme lb)

let args = [
  ("-v"    , Arg.Set Checker.verbose, "Verbose mode" ) ;
  ("-d"    , Arg.Set Basics.debug_mode,   "Debug mode" ) ;
  ("-stdin", Arg.Set run_on_stdin,              "Use standart input" ) ;	  ("-nl", Arg.Set Dtree.allow_non_linear, "Allow non left-linear rewrite rules")]

let run_on_file file =
  let input = open_in file in
    Basics.debug "Processing file '%s'..." file;
    parse (Lexing.from_channel input) ;
    Errors.success "File '%s' was successfully checked." file;
    close_in input


let _ =
  try
    begin
      Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
      if !run_on_stdin then (
        parse (Lexing.from_channel stdin) ;
        Errors.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err             -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit                      -> exit 3
