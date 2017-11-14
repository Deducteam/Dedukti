(********** universes' variables ************)

module UVar =
struct

  type uvar = Basic.ident

  let basename = "univ_variable"

  let is_uvar v =
    let s = Basic.string_of_ident v in
    let n = String.length basename in
    String.length s > n && String.sub s 0 n = basename

  let fresh =
    let counter = ref 0 in
    fun () ->
      let name = Format.sprintf "%s%d" basename !counter in
      incr counter; Basic.mk_ident name

end


module Mapping =
struct

  type index = int

  exception MappingError of index

  type t =
    {
      to_index: (UVar.uvar, index) Hashtbl.t;
      from_index: (index, UVar.uvar) Hashtbl.t
    }

  let memory =
    {
      to_index = Hashtbl.create 251;
      from_index = Hashtbl.create 251
    }

  let to_index =
    let counter = ref 0 in
    fun name ->
      if Hashtbl.mem memory.to_index name then begin
        Hashtbl.find memory.to_index name end
      else
        let n = !counter in
        Hashtbl.add memory.to_index name n;
        Hashtbl.add memory.from_index n name;
        incr counter; n

  let from_index n =
    if Hashtbl.mem memory.from_index n then
      Hashtbl.find memory.from_index n
    else
      raise (MappingError n)

end

module Constraints =
struct

  module M = Mapping

  type constraints =
    | Eq of M.index * M.index
    | Succ of M.index * M.index
    | Le of M.index * M.index
    | Rule of M.index * M.index * M.index (* leave it abstract for now *)

  module ConstraintSet = Set.Make (struct type t = constraints let compare = compare end)

  module CS = ConstraintSet

  let global_constraints = ref ConstraintSet.empty

  let add_constraint c =
    global_constraints := ConstraintSet.add c !global_constraints

  let add_constraint_eq ident ident' =
    let n = M.to_index ident in
    let n' = M.to_index ident' in
    add_constraint (Eq(n,n'))

  let add_constraint_succ ident ident' =
    let n = M.to_index ident in
    let n' = M.to_index ident' in
    add_constraint (Succ(n,n'))

  let add_constraint_le ident ident' =
    let n = M.to_index ident in
    let n' = M.to_index ident' in
    add_constraint (Le(n,n'))

  let add_constraint_rule ident ident' ident'' =
    let n = M.to_index ident in
    let n' = M.to_index ident' in
    let n'' = M.to_index ident'' in
    add_constraint (Rule(n,n',n''))

  let info () =
    let eq,succ,le,rule = ref 0, ref 0, ref 0, ref 0 in
    CS.iter (fun x ->
        match x with
        | Eq _ -> incr eq
        | Succ _ -> incr succ
        | Le _ -> incr le
        | Rule _ -> incr rule) !global_constraints;
    Format.printf "Number of constraints:@.";
    Format.printf "@[eq  :%d@]@." !eq;
    Format.printf "@[succ:%d@]@." !succ;
    Format.printf "@[le  :%d@]@." !le;
    Format.printf "@[rule:%d@]@." !rule
end

let cic = Basic.mk_mident "cic"

let mk_const id = Term.mk_Const Basic.dloc (Basic.mk_name cic id)

let succ = Basic.mk_name cic (Basic.mk_ident "succ")

let sort = Basic.mk_name cic (Basic.mk_ident "Sort")

let lift = Basic.mk_name cic (Basic.mk_ident "lift")

let rule = Basic.mk_name cic (Basic.mk_ident "rule")

let prop = Basic.mk_name cic (Basic.mk_ident "prop")

let utype = Basic.mk_name cic (Basic.mk_ident "type")

let is_const cst t =
  match t with
  | Term.Const(_,n) -> Basic.name_eq cst n
  | _ -> false

let is_prop t =
  match t with
  | Term.Const(_,n) when is_const prop t -> true
  | _ -> false

let is_type t =
  match t with
  | Term.App(t,_,[]) when is_const utype t -> true
  | _ -> false

let is_uvar t =
  match t with
  | Term.Const(_,n) when UVar.is_uvar (Basic.id n) -> true
  | _ -> false

let is_succ t =
  match t with
  | Term.App(c,arg,[]) when is_const succ c -> true
  | _ -> false

let is_lift t =
  match t with
  | Term.App(c, s1, [s2;a]) when is_const lift c -> true
  | _ -> false

let is_rule t =
  match t with
  | Term.App(c, s1, [s2]) when is_const rule c -> true
  | _ -> false

let extract_uvar t =
  match t with
  | Term.Const(_,n) when UVar.is_uvar (Basic.id n) -> Basic.id n
  | _ -> failwith "is not an uvar"

let extract_succ t =
  match t with
  | Term.App(c,arg,[]) when is_const succ c -> extract_uvar arg
  | _ -> failwith "is not a succ"

let extract_lift t =
  match t with
  | Term.App(c,s1,[s2;a]) when is_const lift c -> extract_uvar s1, extract_uvar s2
  | _ -> failwith "is not a lift"

let extract_rule t =
  match t with
  | Term.App(c, s1, [s2]) when is_const rule c -> extract_uvar s1, extract_uvar s2
  | _ -> failwith "is not a rule"

module V = UVar

let rec generate_constraints (l:Term.term) (r:Term.term) =
  (*
  Format.printf "debug: %a@." Term.pp_term l;
  Format.printf "debug: %a@." Term.pp_term r; *)
  if is_uvar l && is_uvar r then
    let l = extract_uvar l in
    let r = extract_uvar r in
    Constraints.add_constraint_eq l r;
    true
  else if is_succ l && is_uvar r then
    let l = extract_succ l in
    let r = extract_uvar r in
    Constraints.add_constraint_succ l r;
    true
  else if is_uvar l && is_succ r then
    generate_constraints r l (* just a switch of arguments *)
  else if is_lift l && is_uvar r then
    let s1,s2 = extract_lift l in
    let r = extract_uvar r in
    Constraints.add_constraint_eq s2 r;
    Constraints.add_constraint_le s1 s2;
    true
  else if is_uvar l && is_lift r then
    generate_constraints r l (* just a switch of arguments *)
  else if is_rule l && is_uvar r then
    let s1,s2 = extract_rule l in
    let r = extract_uvar r in
    Constraints.add_constraint_rule s1 s2 r;
    true
  else if is_uvar r && is_rule l then
    generate_constraints r l (* just a switch of arguments *)
  else
    false
let new_uvar sg =
  let id = UVar.fresh () in
  let md = Signature.get_name sg in
  let name = Basic.mk_name md id in
  let cst = Term.mk_Const Basic.dloc name in
  Signature.add_declaration sg Basic.dloc id Signature.Static (Term.mk_Const Basic.dloc sort);
  cst

let rec elaboration sg term =
  let open Term in
  (* can be optimized by fixing prop *)
  if is_prop term || is_type term then
    new_uvar sg
  else
    match term with
    | App(f, a, al) ->
      let f' = elaboration sg f in
      let a' = elaboration sg a in
      let al' = List.map (elaboration sg) al in
      mk_App f' a' al'
    | Lam(loc, id, t_opt, t) ->
      let t' = elaboration sg t in
      begin
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x -> let x' = elaboration sg x in
          mk_Lam loc id (Some x') t'
      end
    | Pi(loc, id, ta, tb) ->
      let ta' = elaboration sg ta in
      let tb' = elaboration sg tb in
      mk_Pi loc id ta' tb'
    | _ ->     term
