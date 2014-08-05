(** Optimize terms before evaluation *)

open Types

type optim_rule = term -> term option

let optimize ~rules t =
  let rec aux rs t = match rs with
    | [] -> t
    | r :: rs' ->
        (* try rule [r] *)
        match r t with
        | None -> aux rs' t
        | Some t' -> aux rules t' (* try all rules again *)
  in
  aux rules t

(** {2 Rules} *)

module TSet = Set.Make(
struct
  type t = term
  let compare = term_compare
end)

(* map from terms *)
module TMap = Map.Make(
struct
  type t = term
  let compare = term_compare
end)

module IMap = Map.Make(
struct
  type t = int
  let compare (i:int) j = Pervasives.compare i j
end)

module CSE = struct
  type mset = {
    exprs : int TMap.t;  (* expression -> number of occurrences *)
    var2expr : TSet.t VarMap.t;  (* var -> expressions containing it *)
    occ2expr : TSet.t IMap.t;  (* number of occurrences -> expressions *)
  }

  let empty_mset = {
    exprs=TMap.empty;
    var2expr=VarMap.empty;
    occ2expr = IMap.empty;
  }

  let _varmap_get map x = try VarMap.find x map with Not_found -> TSet.empty
  let _imap_get map x = try IMap.find x map with Not_found -> TSet.empty

  let _varmap_add map x e =
    let set = _varmap_get map x in
    VarMap.add x (TSet.add e set)

  let _imap_add map i e =
    let set = _imap_get map i in
    IMap.add i (TSet.add e set) map

  let _imap_remove map i e =
    try
      let set = IMap.find i map in
      let set' = TSet.remove e set in
      if TSet.is_empty set' then IMap.remove i map else IMap.add i set' map
    with Not_found -> map

  (* add [n] occurrences of [e] to the [mset] *)
  let _add_expr ~n mset e =
    assert (n>0);
    try
      let n' = TMap.find e mset.exprs in
      let occ2expr = _imap_remove mset.occ2expr n' e in
      let occ2expr = _imap_add occ2expr (n+n') e in
      {mset with exprs = TMap.add e (n+n') mset.exprs; occ2expr;}
    with Not_found ->
      let vars = term_vars e in
      let var2expr = VarSet.fold
        (fun v map ->
          let set = _varmap_get map v in
          VarMap.add v (TSet.add e set) map
        ) vars mset.var2expr
      in
      let occ2expr = _imap_add mset.occ2expr n e in
      { exprs=TMap.add e n mset.exprs; var2expr; occ2expr }

  (* remove term [e] completely (out of scope) *)
  let _remove_expr mset e =
    try
      let n = TMap.find e mset.exprs in
      let occ2expr = _imap_remove mset.occ2expr n e in
      (* remove the term, it has no occurrences any more *)
      let vars = term_vars e in
      let var2expr = VarSet.fold
        (fun v map ->
          try
            let set = VarMap.find v map in
            let set' = TSet.remove e set in
            if TSet.is_empty set'
              then VarMap.remove v map
              else VarMap.add v set' map
          with Not_found -> map
        ) vars mset.var2expr
      in
      {var2expr; exprs=TMap.remove e mset.exprs; occ2expr}
    with Not_found -> mset

  (* remove expressions that depend on the variable [v] *)
  let _remove_var mset v =
    try
      let set = VarMap.find v mset.var2expr in
      TSet.fold (fun t mset -> _remove_expr mset t) set mset
    with Not_found -> mset

  exception Found of term

  let _find f mset =
    let bindings = IMap.bindings mset.occ2expr in
    let bindings = List.rev bindings in
    try
      List.iter
        (fun (n,set) ->
          TSet.iter (fun t -> if f t n then raise (Found t)) set
        ) bindings;
      None
    with Found t -> Some t

  let _merge_mset a ~into =
    TMap.fold
      (fun t n acc -> _add_expr ~n acc t)
      a.exprs into

  (* replace term [sub] with [var] in t *)
  let replace ~var t ~sub =
    let rec aux t =
      if term_eq t sub
      then mk_Var dloc var
      else match t with
        | Kind | Type _ | Const _ | Meta _ | Var _ -> t
        | App (f, a, l) ->
            mk_App (aux f) (aux a) (List.map aux l)
        | Lam (l,x,a,b) -> mk_Lam l x (aux a) (aux b)
        | Pi (l,x,a,b) -> mk_Pi l x (aux a) (aux b)
        | Let (l,x,a,b) -> mk_Let l x (aux a) (aux b)
    in
    aux t

  let _fold_map f acc l =
    let rec aux acc ret l = match l with
      | [] ->  List.rev ret, acc
      | x::tail ->
          let y, acc = f acc x in
          aux acc (y::ret) tail
    in
    aux acc [] l

  let _let_name = hstring "_v"

  let _term_contains ~var t =
    let rec aux t = match t with
      | Const _ | Meta _ | Kind | Type _ -> false
      | Var (_,v') -> Var.equal var v'
      | App (f,a,l) -> aux f || aux a || List.exists aux l
      | Lam (_,v',a,b)
      | Let (_,v',a,b)
      | Pi (_, Some v',a,b) ->
          if Var.equal var v' then false else aux a || aux b
      | Pi (_, None,a,b) -> aux a || aux b
    in aux t

  (* recursive function that eliminates common subexpressions,
    if they are "big enough". [n] incremented at each transformation.
    @param n number of transformations done
    @param k current depth
    @param acc multiset of sub-expressions met so far
    @return normalized term and multiset of subexpressions *)
  let rec fix ~n acc t : term * mset =
    match t with
    | Kind | Type _ | Var _ | Const _ | Meta _ -> t, acc
    | App (f, a, l) ->
        let f, mset = fix ~n empty_mset f in
        let a, mset = fix ~n mset a in
        let l, mset = _fold_map (fix ~n) mset l in
        let t = mk_App f a l in
        (* the new term, t, is a subexpression eligible
          for elimination (might require non-trivial evaluation) *)
        t, _add_expr ~n:1 (_merge_mset mset ~into:acc) t
    | Let (l,x,a,b) ->
        let b, acc = fix ~n acc b in
        let acc = _remove_var acc x in
        mk_Let l x a b, acc
    | Pi (l,x_opt,a,b) ->
        (* just rewrite under. Pi is not an expression to eliminate *)
        let b, acc = fix ~n acc b in
        let acc = match x_opt with
          | None -> acc
          | Some x -> _remove_var acc x
        in
        mk_Pi l x_opt a b, acc
    | Lam (l,x,a,b) ->
        (* ignore the [a]; compute set of expressions occurring in b *)
        let b, mset = fix ~n empty_mset b in
        (* any sub-expressions to share? Such an expression must:
           - occur more than once in e
           - contain x (the bound var), otherwise it should be shared deeper
           TODO: also favor bigger terms, if possible (threshold?) *)
        match _find (fun e n -> n >= 2 && _term_contains ~var:x e) mset with
        | Some e ->
            (* replace b with "let var := e in b" where [var] is a fresh var *)
            let var = Var.fresh_of_ident _let_name in
            flush stderr; flush stdout;
            let b' = mk_Let dloc var e (replace ~var b ~sub:e) in
            let t' = mk_Lam l x a b' in
            incr n;
            Global.debug_no_loc 4 "  CSE of %a" Pp.pp_term t;
            Global.debug_no_loc 4 "    into %a" Pp.pp_term t';
            Global.debug_no_loc 4 "    factoring (%a)" Pp.pp_term e;
            (* now try to find new/other subexpressions in t' *)
            fix ~n acc t'
        | None ->
            (* do nothing *)
            let mset = _remove_var mset x in
            mk_Lam l x a b, _merge_mset mset ~into:acc
end


let common_subexpr_elim t =
  (* n: number of rewriting steps *)
  let n = ref 0 in
  let t', map = CSE.fix ~n CSE.empty_mset t in
  if !n = 0 then None else Some t'

