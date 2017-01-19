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
  | AppTerm
  | Cons
  | Const
  | ConstTerm
  | DefineConst
  | Nil
  | OpType
  | Pop
  | Pragma
  | Refl
  | Thm
  | TypeOp
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
  | AppTerm -> "appTerm\n"
  | Cons -> "cons\n"
  | Const -> "const\n"
  | ConstTerm -> "constTerm\n"
  | DefineConst -> "defineConst\n"
  | Nil -> "nil\n";
  | OpType -> "opType\n";
  | Pop -> "pop\n";
  | Pragma -> "pragma\n";
  | Refl -> "refl\n";
  | Thm -> "thm\n";
  | TypeOp -> "typeOp\n"
  | Var -> "var\n"
  | VarTerm -> "varTerm\n"
  | VarType -> "varType\n"
  | Version -> "version\n"

let string_of_ast ast = List.fold_left (fun s x -> (string_of_instr x)^s) "" ast

let namespace = ref ""

let (cmds:ast ref) = ref []

let add_instr cmd =
  cmds := cmd::!cmds

(* ********************************* *)

let counter = ref 0

let hol_module = hstring "hol"
let hol_sort = hstring "type"
let hol_eta = hstring "eta"
let hol_arrow = hstring "arrow"
let hol_forall = hstring "forall"
let hol_impl = hstring "impl"
let hol_prop = hstring "prop"
let hol_eps = hstring "eps"
let hol_forall_kind_type = hstring "forall_kind_type"

let (===) = Basic.ident_eq

let is_hol_const c t =
  match t with
  | Term.Const(_, m, id) -> (m === hol_module) &&  (c === id)
  | _ -> false

let is_hol_sort t = is_hol_const hol_sort t

let is_hol_eta t = is_hol_const hol_eta t

let is_hol_arrow t = is_hol_const hol_arrow t

let is_hol_forall t = is_hol_const hol_forall t

let is_hol_impl t = is_hol_const hol_impl t

let is_hol_prop t = is_hol_const hol_prop t

let is_hol_eps t = is_hol_const hol_eps t

let is_hol_forall_kind_type t = is_hol_const hol_forall_kind_type t

let is_hol_type t =
  match t with
  | Term.App (c, _, _) -> is_hol_eta c
  | _ -> false

let is_hol_proof t =
  match t with
  | Term.App (c, _, _) -> is_hol_eps c
  | _ -> false


let load_typeop str =
  [String str; TypeOp]

let load_const str=
  [String str; Const]

let forall_str = "!"

let equal_str = "="

let true_str = "T"

let arrow_str = "->"

let bool_str = "bool"

let impl_str = "==>"

let debug =
  [String "debug"; Pragma]


let bool_type =
  (load_typeop bool_str)@
  [Nil;OpType]


let arrow_type tyl tyr =
  (load_typeop arrow_str)@
  tyl@tyr@
  [Nil;Cons;Cons;OpType]

let forall_type ty =
  let p = arrow_type ty bool_type in
  arrow_type p bool_type

let impl_type = arrow_type bool_type (arrow_type bool_type bool_type)

let extract_type t =
  match t with
  | Term.App(c, ty, _) when is_hol_eta c -> ty
  | _ -> assert false

let rec instr_of_type t =
  match t with
  | Term.Const(_,m,id) when is_hol_prop t ->
    bool_type
  | Term.Const(_,m, id) ->
    [String (string_of_ident id); VarType]
  (* ASSUMPTION : no clash in names and id should start with a majuscule *)
  | Term.DB(_,id,i) ->
    [String (string_of_ident id); VarType]
  | Term.App(c, tyl, [tyr]) when is_hol_arrow c ->
    arrow_type (instr_of_type tyl) (instr_of_type tyr)
  | Term.App(c, Term.Lam(_,x, Some tx, ty), []) when is_hol_forall_kind_type c ->
    instr_of_type ty
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo type"


type env = (Basic.ident * Term.term) list

(* TODO add a dictionnary *)
let rec instr_of_term env t =
  match t with
  | Term.App(c, ty, [te])  when is_hol_forall c ->
    (load_const forall_str)@
    (forall_type (instr_of_type ty))@
    [ConstTerm]@
    (instr_of_term env te)@
    [AppTerm]
  | Term.App(c, tel, [ter]) when is_hol_impl c ->
    (load_const impl_str)@
    (impl_type)@
    [ConstTerm]@
    (instr_of_term env tel)@
    [AppTerm]@
    (instr_of_term env ter)@
    [AppTerm]
  | Term.App(f, a, args) ->
    List.fold_left
      (fun instr a -> instr@(instr_of_term env a)@[AppTerm]) (instr_of_term env f) (a::args)
  | Term.Lam(_,x,Some tx, t') when is_hol_sort tx ->
    instr_of_term env t'
  | Term.Lam(_,x,Some tx, t') ->
    let tx' = extract_type tx in
    let it = (instr_of_term ((x,tx')::env) t') in
    let itx = instr_of_type tx' in
    let ix  = (String (string_of_ident x)) in
    [ix]@itx@[Var]@it@[AbsTerm]
  | Term.Lam(_, _, None, _) -> failwith "every lambda should be typed"
  | Term.DB(_,id,i) ->
    (* ASSUMPTION : no clash in names variable as in \x\x.x*)
    let itx = instr_of_type (List.assoc id env) in
    let ix = (String (string_of_ident id)) in
    [ix]@itx@[Var;VarTerm]
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo term"


(* \x\P. \x.P = \x.true *)

let version =
  [Int 6;Version]

let prelude : ast =
  let version = version in (*
  let forall = load_const forall_str in
  let equal = load_const equal_str in
  let equalt = equal@[String "->"; TypeOp; String "bool"; TypeOp;Nil;OpType; String "->";TypeOp;String "bool"; TypeOp;Nil;OpType;String "bool"; TypeOp;Nil;OpType;Nil;Cons;Cons;OpType;Nil;Cons;Cons;OpType;ConstTerm] in
  let true' = load_const true_str in
  let truet = true'@[String "bool"; TypeOp; Nil; OpType;ConstTerm] in
  let refl = truet@[Refl] in
  let reflt = equalt@truet@[AppTerm]@truet@[AppTerm] in
  let thm = refl@[Nil]@reflt@[Thm] in
  thm *)
  version

let equal ty tl tr =
  (load_const equal_str)@
  (arrow_type ty (arrow_type ty bool_type))@[ConstTerm]@
  tl@[AppTerm]@
  tr@[AppTerm]

let mk_prelude lc name =
  namespace := string_of_ident name;
  List.iter add_instr prelude

let define_hol_type id =
  begin
    add_instr (String (string_of_ident id));
    add_instr VarType
  end

let define_hol_const id te =
  let const = [(String (string_of_ident id))] in
  let term = instr_of_term [] te in
  List.iter add_instr (const@term@[DefineConst])

let mk_declaration lc id pty : unit =
  if is_hol_sort pty then
    define_hol_type id
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
  if is_hol_type ty then
    define_hol_const id pte
  else
    failwith "todo definition"



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
