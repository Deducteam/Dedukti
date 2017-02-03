type instr =
  | String of string
  | Int of int
  | AbsTerm
  | AbsThm
  | AppTerm
  | AppThm
  | Assume
  | Axiom
  | BetaConv
  | Cons
  | Const
  | ConstTerm
  | DeductAntisym
  | Def
  | DefineConst
  | DefineConstList
  | DefineTypeOp
  | EqMp
  | HdTl
  | Load of int
  | Nil
  | OpType
  | Pop
  | Pragma
  | ProveHyp
  | Ref
  | Refl
  | Remove
  | Subst
  | Sym
  | Thm
  | Trans
  | TypeOp
  | Var
  | VarTerm
  | VarType
  | Version
(*
module type Memoize =
sig
  type 'a t
  type name
  type var
  type term
  type ty
  type thm
  type cmd

  val update_name : 'a -> instr list -> 'a t -> name
end

module Basic : Memoize =
struct
  type 'a t = 'a
  type name = instr list
  type var = instr list
  type term = instr list
  type ty = instr list
  type thm = { proof : instr list; hyp : instr list; term : instr list}
  type cmd = instr list

  let update_name _ instr _ = instr
end
*)

module S = Set.Make (struct type t = int let compare = compare end)

type name = int

type var = int

type term = int

type ty = int

type const = int * int

type hyp = S.t

type thm = int

type cmd = unit



type dict = (int, instr list) Hashtbl.t

type generated_entry = S.t

let dict : dict = Hashtbl.create 87

let const_seen  = Hashtbl.create 87

let generated_entry : generated_entry ref =  ref S.empty

let counter : int ref = ref 0

let version = [Int(6);Version]

let cmds : instr list ref = ref version

let save instrs =
  cmds := !cmds@instrs

let mk_ref n = [Int(n);Ref]

let update entry instrs seen =
  if Hashtbl.mem seen entry then
    Hashtbl.find seen entry
  else
    let x = !counter in
    incr counter;
    Hashtbl.add seen entry x;
    let def = [Int(x);Def] in
    Hashtbl.add dict x (instrs@def);
    x

let update_cons name instrs =
  let xthm = !counter in
  incr counter;
  let xconst = !counter in
  incr counter;
  let def = [Int(xthm);Def;Int(xconst);Def;Pop;Pop] in
  save (instrs@def);
  generated_entry := S.add xthm (S.add xconst !generated_entry);
  Hashtbl.add const_seen name (xconst, xthm)

let mk_name =
  let rec mk_namespace l =
    List.fold_left (fun s x -> s^x^".") "" l
  in
  let seen = Hashtbl.create 87 in
  fun namespace str ->
    let name = (mk_namespace namespace)^str in
    update name [String name] seen


let mk_var =
  let seen = Hashtbl.create 87 in
  fun name ty ->
    let instr = [Load name;Load ty;Var] in
    update (name,ty) instr seen


let mk_varType =
  let seen = Hashtbl.create 87 in
  fun name ->
    update name [Load name;VarType] seen


let mk_appTerm =
  let seen = Hashtbl.create 87 in
  fun f t ->
    let instr = [Load f;Load t;AppTerm] in
    update (f,t) instr seen

let mk_absTerm =
  let seen = Hashtbl.create 87 in
  fun v t ->
    let instr = [Load v;Load t;AbsTerm] in
    update (v,t) instr seen

let mk_varTerm =
  let seen = Hashtbl.create 87 in
  fun var ->
    let instr = [Load var;VarTerm] in
    update var instr seen

let term_of_const =
  let seen = Hashtbl.create 87 in
  fun const ty ->
    let instr = [Load (fst const);Load ty;ConstTerm] in
    update (const,ty) instr seen


let const_of_name name =
  if Hashtbl.mem const_seen name then
    Hashtbl.find const_seen name
  else
    failwith "const not declared"

let rec mk_hyp l = List.fold_left (fun set x -> S.add x set) S.empty l

let list_of_hyp hyp =
  S.fold (fun x l -> (Load x)::l) hyp []

let mk_list =
  let seen = Hashtbl.create 87 in
  fun l ->
    let instr =  List.fold_left (fun l x -> x::(l@[Cons])) [Nil] l in
    update l instr seen

let mk_axiom =
  let seen = Hashtbl.create 87 in
  fun hyp term ->
    let hyp = mk_list (list_of_hyp hyp) in
    let instr = [Load hyp;Load term;Axiom] in
    update (hyp,term) instr seen


let thm_of_const const =
  snd const


let mk_const name term =
  let instrs = [Load name; Load term;DefineConst] in
  update_cons name instrs

(*
let mk_termconst =
  let seen = Hashtbl.create 87 in
  fun str ty ->
    let name = mk_name [] str in
    let instr = [name;Const;ty;ConstTerm] in
    update (name,ty) instr seen

let mk_const str ty l =
    let const = mk_termconst str ty in
    let rec apply term l =
      match l with
      | [] -> term
      | x::t -> apply (mk_appTerm term x) t
    in
    apply const l
    *)

let mk_thm term hyp thm =
  save [Load thm;Load (mk_list (list_of_hyp hyp));Load term;Thm]

let mk_remove x = [Int(x);Remove;Pop]

let rec load_instr n =
  if S.mem n !generated_entry then
    string_of_instrs (mk_ref n)
  else
    let instr = string_of_instrs (Hashtbl.find dict n) in
    generated_entry := S.add n !generated_entry;
    instr

and string_of_instr x =
  match x with
  | String(s) -> Printf.sprintf "\"%s\"\n" s
  | Int(i) -> (string_of_int i)^"\n"
  | AbsTerm -> "absTerm\n"
  | AbsThm -> "absThm\n"
  | AppTerm -> "appTerm\n"
  | AppThm -> "appThm\n"
  | Assume -> "assume\n"
  | Axiom -> "axiom\n"
  | BetaConv -> "betaConv\n"
  | Cons -> "cons\n"
  | Const -> "const\n"
  | ConstTerm -> "constTerm\n"
  | DeductAntisym -> "deductAntisym\n"
  | Def -> "def\n"
  | DefineConst -> "defineConst\n"
  | DefineConstList -> "defineConstList\n"
  | DefineTypeOp -> "defineTypeOp\n"
  | EqMp -> "eqMp\n"
  | HdTl -> "hdTl\n"
  | Load(n) -> load_instr n
  | Nil -> "nil\n";
  | OpType -> "opType\n";
  | Pop -> "pop\n";
  | Pragma -> "pragma\n";
  | ProveHyp -> "proveHyp\n";
  | Ref -> "ref\n";
  | Refl -> "refl\n";
  | Remove -> "remove\n";
  | Subst -> "subst\n";
  | Sym -> "sym\n";
  | Thm -> "thm\n";
  | Trans -> "trans\n";
  | TypeOp -> "typeOp\n"
  | Var -> "var\n"
  | VarTerm -> "varTerm\n"
  | VarType -> "varType\n"
  | Version -> "version\n"

and string_of_instrs instrs = List.fold_left (fun s x -> s^(string_of_instr x)) "" instrs

let remove_keys () =
  let rec remove x =
    if x = 0 then []
    else mk_remove (x-1)@(remove (x-1))
  in
  save (remove !counter)

let debug () = save [String("debug");Pragma]

let to_string () = string_of_instrs !cmds
