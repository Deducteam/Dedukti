open Basic

(* ********************************* *)
(* Open theory stuff                 *)

type typeop
type hol_type
type cst
type var
type term
type thm

type instr =
  | String of string
  | Int of int
  | AbsTerm
  | DefineConst
  | Var
  | VarTerm
  | VarType
  | Version

type ast = instr list

let string_of_instr x =
  match x with
  | String(s) -> Printf.sprintf "\"%s\"\n" s
  | Int(i) -> (string_of_int i)^"\n"
  | AbsTerm -> "absTerm\n"
  | DefineConst -> "defineConst\n"
  | Var -> "var\n"
  | VarTerm -> "varTerm\n"
  | VarType -> "varType\n"
  | Version -> "version\n"

let string_of_ast ast = List.fold_left (fun s x -> (string_of_instr x)^s) "" ast

let namespace = ref ""

let (cmds:ast ref) = ref []

let prelude : ast =
  [Int 6; Version]

let add_instr cmd =
  cmds := cmd::!cmds

(* ********************************* *)

let hol_module = hstring "hol"
let hol_type = hstring "type"

let (===) = Basic.ident_eq

let is_hol_type t =
  match t with
  | Term.Const(_, m, id) -> (m === hol_module) &&  (hol_type === id)
  | _ -> false

let rec instr_of_type t =
  match t with
  | Term.Const(_,m, id) ->
    [String (string_of_ident id); VarType]
  | _ -> failwith "todo type"

type env = (Basic.ident * Term.term) list

(* add a dictionnary *)
let rec instr_of_term env t =
  match t with
  | Term.Lam(_,x,Some tx, t') ->
    let it = (instr_of_term ((x,tx)::env) t') in
    let itx = instr_of_type tx in
    let ix  = (String (string_of_ident x)) in
    [ix]@itx@[Var]@it@[AbsTerm]
  | Term.Lam(_, _, None, _) -> failwith "every lambda should be typed"
  | Term.DB(_,id,i) ->
    (* ASSUMPTION : no clash in names variable as in \x\x.x*)
    let itx = instr_of_type (List.assoc id env) in
    let ix = (String (string_of_ident id)) in
    [ix]@itx@[Var;VarTerm]
  | _ -> failwith "todo term"

let mk_prelude lc name =
  namespace := string_of_ident name;
  List.iter add_instr prelude

let mk_declaration lc id pty : unit =
  if is_hol_type pty then
    begin
    add_instr (String (string_of_ident id));
    add_instr VarType
    end
  else
    failwith "todo declaration"


let mk_definable lc id pty : unit =
  match Env.declare_definable lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  let ty =
    match pty_opt with
    | Some ty -> ty
    | None -> failwith "missing type in the definition"
  in
  let iid = (String (string_of_ident id)) in
  let ite = instr_of_term [] pte in
  List.iter add_instr ([iid]@ite@[DefineConst])



let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let mk_rules = function
  | [] -> ()
  | ((_,pat,_)::_) as lst ->
    begin
      match Env.add_rules lst with
      | OK _ -> ()
      | Err e -> Errors.fail_env_error e
    end

let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  Printf.printf "%s\n" (string_of_ast !cmds)
