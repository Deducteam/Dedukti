open Basic
open Pp
open Rule

type entry =
| Declaration of ident * Signature.staticity * Term.term
| Definable of ident * Term.term
| Definition of ident * Term.term option * Term.term
| Opaque of ident * Term.term option * Term.term
| RewriteRule of Rule.untyped_rule

type ast = entry list

let entries : (entry list) ref  = ref []
(*
let reconstruction_of_entry env entry =
  match entry with
  | Declaration(id, st, t) -> Declaration(id, st, reconstruction env t)
  | Definable(id, t) -> Definable(id, reconstruction env t)
  | Definition(id, None, te) -> Definition(id, None, reconstruction env te)
  | Definition(id, Some(ty), te) -> Definition(id, Some(reconstruction env ty), reconstruction env te)
  | Opaque(id, None, te) -> Opaque(id, None, reconstruction env te)
  | Opaque(id, Some(ty), te) -> Opaque(id, Some(reconstruction env ty), reconstruction env te)
  | RewriteRule(rule) -> RewriteRule({rule with rhs= reconstruction env rule.rhs})
*)

module C = Constraints
module Checker = struct


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

let sg = ref (Signature.make (mk_mident "noname"))

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_mident name;
  Format.printf "#NAME %a.@.@." print_mident name;
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id st pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  let sg = Env.get_signature () in
  let pty' = C.elaboration sg pty in
  entries := Declaration(id, st, pty')::!entries;
  match Env.declare lc id st pty' with
    | OK () -> () ;
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  let sg = Env.get_signature () in
  let pty_opt' =
    match pty_opt with
    | None -> None
    | Some pty -> Some (C.elaboration sg pty)
  in
  let pte' = C.elaboration sg pte in
  entries := Definition(id, pty_opt', pte')::!entries;
  match Env.define lc id pte' pty_opt' with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  let sg = Env.get_signature () in
  let pty_opt' =
    match pty_opt with
    | None -> None
    | Some pty -> Some (C.elaboration sg pty)
  in
  let sg = Env.get_signature () in
  let pte' = C.elaboration sg pte in
  entries := Definition(id, pty_opt', pte')::!entries;
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Pattern (l,cst,_) -> (l,cst)
  | _ -> (dloc,mk_name (mk_mident "") qmark)

let mk_rules : untyped_rule list -> unit = Rule.(function
  | [] -> ()
  | (rule::_) as lst ->
    let sg = Env.get_signature () in
    let lst' : untyped_rule list = List.map (fun (rule : untyped_rule) ->
        {rule with rhs = C.elaboration sg rule.rhs} ) lst in
    begin
      let (l,cst) = get_infos rule.pat in
      eprint l "Adding rewrite rules for '%a'" pp_name cst;
      let to_ru = List.rev_map (fun (rule : untyped_rule) -> RewriteRule(rule)) lst' in
      entries := List.rev_append to_ru !entries;
      match Env.add_rules lst' with
      | OK lst2 ->
        List.iter ( fun rule ->
            eprint (Rule.get_loc_pat rule.pat) "%a" pp_typed_rule rule
          ) lst2 ;
      | Err e -> Errors.fail_env_error e
    end)

let mk_command c = failwith "not handle right now"

let export = ref false

let print_entry entry = Pp.(
  match entry with
    | Declaration (id, st, ty) ->
      begin
        match st with
        | Signature.Static -> Format.printf "@[%a :@;<1 2>%a.@]@." print_ident id print_term ty
        | Signature.Definable -> Format.printf "@[def %a :@;<1 2>%a.@]@." print_ident id print_term ty
      end;
  | Definable (id, ty) ->
    Format.printf "@[def %a :@;<1 2>%a.@]@." print_ident id print_term ty
  | Definition (id, None, te) ->
    Format.printf "def @[%a :=@;<1 2>%a.@]@." print_ident id print_term te
  | Definition (id, Some(ty), te) ->
    Format.printf "def @[%a:@;<1 2>%a@;<1 2>:=@;<1 2>%a.@]@." print_ident id print_term ty print_term te
  | Opaque (id, ty_opt, te) -> failwith "TODO"
  | RewriteRule(rule) -> Format.printf "@[%a. @]@." print_untyped_rule rule
)
let print_entries entries = ignore(List.map (print_entry) entries)

let mk_ending () =
  (*
  let env = solve() in
  let entries' = List.rev_map (reconstruction_of_entry env) !entries in
  *)
  print_entries !entries;
  Constraints.Constraints.info ();
  ( if !export then
      if not (Env.export ()) then
	  Errors.fail dloc "Fail to export module '%a'." pp_mident (Env.get_name ()) );
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
  ("-d"    , Arg.Int Basic.set_debug_mode,   "Debug mode" ) ;
  ("-stdin", Arg.Set run_on_stdin,              "Use standart input" ) ;
  ("-errors-in-snf", Arg.Set    Errors.errors_in_snf   , "Normalize the types in error messages");
  ("-nl", Arg.Set Rule.allow_non_linear, "Allow non left-linear rewrite rules")]

let run_on_file file =
  let input = open_in file in
    Basic.debug 1 "Processing file '%s'..." file;
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
