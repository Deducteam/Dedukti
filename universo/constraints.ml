open Export
open Cfg

module type S =
sig
  type t

  val mk_constraint : cstr -> unit

  val infos : unit -> string

  val export : unit -> t

  val import : t -> unit

  val to_set : t -> ConstraintsSet.t
end

module UF =
struct
  let uf = Hashtbl.create 10007

  let rec find l =
    try
      let l' = Hashtbl.find uf l in
      let f = find l' in
      if Basic.ident_eq f l' then
        l'
      else
        begin
          Hashtbl.add uf l' f;
          f
        end
    with _ -> l

  let union l r =
    let l' = find l in
    let r' = find r in
    if Basic.ident_eq l' r' then
      ()
    else
      Hashtbl.add uf l' r'

  let reset () =
    Hashtbl.clear uf
end

module Naive(S:SOLVER) =
struct

  type t = ConstraintsSet.t

  let constraints = ref ConstraintsSet.empty

  let of_name n = Basic.((string_of_mident (md n), string_of_ident (id n)))

  let rec of_univ u =
    let open Cic in
    let open Basic in
    match u with
    | Var s -> S.mk_var s
    | Prop -> S.mk_prop
    | Type i -> S.mk_type i
    | Succ u -> S.mk_succ (of_univ u)
    | Max(ul,ur) -> S.mk_max (of_univ ul) (of_univ ur)
    | Rule(ul,ur) -> S.mk_rule (of_univ ul) (of_univ ur)

  let mk_constraint (left,right) =
    let eleft = of_univ left in
    let eright = of_univ right in
    constraints := ConstraintsSet.add (left, right) !constraints;
    S.mk_eq  eleft eright

  let infos () = "Infos is not implemented yet"

  let export () = !constraints

  let import cstrs =
    S.reset ();
    ConstraintsSet.iter (fun (l,r) -> S.mk_eq (of_univ l) (of_univ r)) cstrs;
    constraints := cstrs

  let to_set t = t
end

module NaiveSyn : S = Naive(Export.Z3Syn)

let s : (module S) ref = ref ((module NaiveSyn) : (module S))

let to_handler s h =
  let (module S:SOLVER) = (module (val s:SOLVER)) in
  if h = "naive" then
    (module NaiveSyn:S)
  else
    failwith "unrecognized handler"

(*
open Uvar

open Basic

type var = Basic.ident

type univ =
  | Prop
  | Type of int

type constraints =
  | Univ of var * univ
  | Eq of var * var
  | Max of var * var * var
  | Succ of var * var
  | Rule of var * var * var

let term_of_univ univ =
  let rec term_of_nat i =
    assert (i>= 0);
    if i = 0 then
      Cic.mk_z
    else
      Cic.mk_s (term_of_nat (i-1))
  in
  match univ with
  | Prop -> Cic.mk_prop
  | Type i -> Cic.mk_type (term_of_nat i)

module Variables = Set.Make (struct type t = Basic.ident let compare = compare end)

module ConstraintsSet = Set.Make (struct type t = constraints let compare = compare end)

module CS = ConstraintsSet

let var_of_ident ident = UF.find ident

let global_constraints = ref ConstraintsSet.empty

let add_constraint c =
  global_constraints := ConstraintsSet.add c !global_constraints

let hash_univ = Hashtbl.create 5

let string_of_univ univ =
  match univ with
  | Prop -> "prop"
  | Type(i) -> "type"^(string_of_int i)

let var_of_univ univ =
  if Hashtbl.mem hash_univ univ then
    var_of_ident (Hashtbl.find hash_univ univ)
  else
    let uvar = mk_ident (string_of_univ univ) in
    Hashtbl.add hash_univ univ uvar;
    add_constraint(Univ(uvar, univ));
    uvar

let add_constraint_prop =
  fun ident ->
    let v = var_of_ident ident in
    UF.union v (var_of_univ Prop)

let add_constraint_type =
  fun v u ->
    let v = var_of_ident v in
    UF.union v (var_of_univ u)

let add_constraint_eq v v' =
  let v = var_of_ident v in
  let v' = var_of_ident v' in
  add_constraint (Eq(v,v'));
  UF.union v v'

let add_constraint_succ v v' =
  let v = var_of_ident v in
  let v' = var_of_ident v' in
  add_constraint (Succ(v,v'))

let add_constraint_max v v' v'' =
  let v = var_of_ident v in
  let v' = var_of_ident v' in
  let v'' = var_of_ident v'' in
  add_constraint (Max(v,v',v''))

let add_constraint_rule v v' v'' =
  let v = var_of_ident v in
  let v' = var_of_ident v' in
  let v'' = var_of_ident v'' in
  add_constraint (Rule(v,v',v''))

module VarSet = Set.Make(struct type t = Basic.ident let compare = compare end)

let info constraints =
  let open Cic in
  let prop,ty,neq,eq,succ,max,rule,nl = ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0 in
  let vars =
    CS.fold (fun x vs ->
        match x with
        | Eq(n,n') -> incr eq;
          VarSet.add n (VarSet.add n' vs)
        | Succ(n,n') -> incr succ;
          VarSet.add n (VarSet.add n' vs)
        | Max(n,n', n'') -> incr max;
          VarSet.add n (VarSet.add n' (VarSet.add n'' vs))
        | Rule(n,n', n'') -> incr rule;
          VarSet.add n (VarSet.add n' (VarSet.add n'' vs))
        | Univ(n,u) -> begin match u with | Prop -> incr prop | Type _ -> incr ty end;
          VarSet.add n vs
      ) constraints VarSet.empty
  in
  let print fmt () =
    Format.fprintf fmt "Number of variables: %d@." (VarSet.cardinal vars);
    Format.fprintf fmt "Number of constraints:@.";
    Format.fprintf fmt "@[prop:%d@]@." !prop;
    Format.fprintf fmt "@[ty  :%d@]@." !ty;
    Format.fprintf fmt "@[eq  :%d@]@." !eq;
    Format.fprintf fmt "@[succ:%d@]@." !succ;
    Format.fprintf fmt "@[max :%d@]@." !max;
    Format.fprintf fmt "@[rule:%d@]@." !rule;
  in
  Format.asprintf "%a" print ()

let fresh_var_rule l r =
  mk_ident ("r"^string_of_ident l^","^string_of_ident r)

let fresh_var_max l r =
  mk_ident ("m"^string_of_ident l^","^string_of_ident r)

let fresh_var_succ l =
  mk_ident ("s"^(string_of_ident l))

let rec simple s =
  let open Cic in
  if is_succ s then
    let s' = extract_succ s in
    let s' = simple s' in
    if is_prop s' then
      term_of_univ (Type 0)
    else if is_type s' then
      failwith "This bug should be reported (simple 1)"
    else mk_succ s'
  else if is_type s then
    begin
      assert_type_zero s;
      term_of_univ (Type 0)
    end
  else if is_rule s then
    let s1,s2 = extract_rule s in
    let s1' = simple s1 in
    let s2' = simple s2 in
    if is_prop s1' then
      s2'
    else if Term.term_eq s1' s2' then
      s2'
    else if is_prop s2' then
      mk_prop
    else
      mk_rule s1 s2
  else
  if is_max s then
    let s1,s2 = extract_max s in
    let s1' = simple s1 in
    let s2' = simple s2 in
    if is_prop s1' then
      s2'
    else if is_prop s2' then
      s1'
    else if Term.term_eq s1' s2' then
        s1'
    else
      mk_max s1' s2'
  else
    s

let rec extract_universe sg (s:Term.term) =
  let open Cic in
  let s = simple s in
  if is_uvar s then
    UF.find (ident_of_uvar s)
  else if is_prop s then
    var_of_univ Prop
  else if is_type s then
    begin
      assert_type_zero s;
      var_of_univ (Type 0)
    end
  else if is_succ s then
    let s' = extract_succ s in
    let v' = extract_universe sg s' in
    let v = fresh_var_succ v' in
    add_constraint_succ v' v;
    v
  else if is_rule s then
    begin
      let s1,s2 = extract_rule s in
      let s1' = extract_universe sg s1 in
      let s2' = extract_universe sg s2 in
      let l = fresh_var_rule s1' s2' in
      add_constraint_rule s1' s2' l;
      l
    end
  else if is_max s then
    begin
      let s1,s2 = extract_max s in
      let s1' = extract_universe sg s1 in
      let s2' = extract_universe sg s2 in
      let l = fresh_var_max s1' s2' in
      add_constraint_max s1' s2' l;
      l
    end
  else
    begin
      Format.printf "%a@." Term.pp_term s;
      failwith "This bug should be reported (extract_universe)"
    end

let string_of_var n = string_of_ident (UF.find n)

let pp_univ fmt = function
  | Prop -> Format.fprintf fmt "Prop"
  | Type i -> Format.fprintf fmt "Type %d" i

let pp_constraint fmt = function
  | Eq(v,v') -> Format.fprintf fmt "%a =? %a" pp_ident v pp_ident v'
  | Succ(v,v') -> Format.fprintf fmt "Succ(%a) =? %a" pp_ident v pp_ident v'
  | Max(v,v',v'') -> Format.fprintf fmt "Max(%a,%a) =? %a" pp_ident v pp_ident v' pp_ident v''
  | Rule(v,v',v'') -> Format.fprintf fmt "Rule(%a,%a) =? %a" pp_ident v pp_ident v' pp_ident v''
  | Univ(v,u) -> Format.fprintf fmt "%a =? %a" pp_ident v pp_univ u

let pp_cs fmt = ConstraintsSet.iter (fun x -> Format.fprintf fmt "%a@." pp_constraint x)

let export () =
  global_constraints := !global_constraints;
  !global_constraints

let import cs =
  UF.reset ();
  global_constraints := cs
*)
