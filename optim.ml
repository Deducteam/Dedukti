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

(* set of (term,depth) *)
module TISet = Set.Make(
struct
  type t = term*int
  let compare = term_compare_depth
end)

(* map of (term,depth) to anything *)
module TIMap = Map.Make(
struct
  type t = term*int
  let compare = term_compare_depth
end)

module IMap = Map.Make(
struct
  type t = int
  let compare (i:int) j = Pervasives.compare i j
end)

module CSE = struct
  type mset = {
    n_to_set : TISet.t IMap.t;
      (* map n -> subexpressions that occur n times *)

    expr_to_n : int TIMap.t;
      (* map expr -> number of times it occurs *)
  }

  let empty_mset = { n_to_set=IMap.empty; expr_to_n=TIMap.empty; }

  let _replace_imap ~f ~x0 k m =
    try
      let x = IMap.find k m in
      IMap.add k (f x) m
    with Not_found ->
      IMap.add k (f x0) m

  (* mark an occurrence of (t,i) *)
  let _add ~n m (t,i) =
    try
      let count = TIMap.find (t,i) m.expr_to_n in
      let count' = count+n in
      let n_to_set = m.n_to_set in
      let n_to_set = _replace_imap count n_to_set
        ~x0:TISet.empty ~f:(TISet.remove (t,i)) in
      let n_to_set = _replace_imap count' n_to_set
        ~x0:TISet.empty ~f:(TISet.add (t,i)) in
      { n_to_set; expr_to_n = TIMap.add (t,i) count' m.expr_to_n; }
    with Not_found ->
      let n_to_set = _replace_imap 1 m.n_to_set
        ~x0:TISet.empty ~f:(TISet.add (t,i)) in
      { n_to_set; expr_to_n = TIMap.add (t,i) 1 m.expr_to_n; }

  (* merge two multisets. [into] is merged into [a] *)
  let _merge a ~into =
    IMap.fold
      (fun n set acc ->
        TISet.fold (fun (t,k) acc -> _add ~n acc (t,k)) set acc
      ) into.n_to_set a

  exception FoundPair of term * int

  (* find a (t,i) that satisfies f, if any. Pairs are testing by decreasing
      number of occurrences *)
  let _find f m =
    try
      IMap.iter
        (fun n set ->
          TISet.iter
            (fun (t,i) -> if f (t,i) n then raise (FoundPair (t,i))) set
        ) m.n_to_set;
      None
    with FoundPair (t,i) -> Some (t,i)

  (* replace term e with a de Bruijn index in t, and shift the other
    bound variables.. k is the number of surrounding binders *)
  let replace_shift ~name k t e =
    let rec aux k t =
      if term_eq t e
      then mk_DB dloc name k   (* replace with de Bruijn *)
      else match t with
        | Kind | Type _ | Const _ | Meta _ -> t
        | DB (l,x,n) -> if n<k then t else mk_DB l x (n+1)
        | App (f, a, l) ->
            mk_App (aux k f) (aux k a) (List.map (aux k) l)
        | Lam (l,x,a,b) -> mk_Lam l x (aux k a) (aux (k+1) b)
        | Pi (l,x,a,b) -> mk_Pi l x (aux k a) (aux (k+1) b)
        | Let (l,x,a,b) -> mk_Let l x (aux k a) (aux (k+1) b)
    in
    aux k t

  let _fold_map f acc l =
    let rec aux acc ret l = match l with
      | [] ->  List.rev ret, acc
      | x::tail ->
          let y, acc = f acc x in
          aux acc (y::ret) tail
    in
    aux acc [] l

  (* recursive function that eliminates common subexpressions,
    if they are "big enough". [n] incremented at each transformation.
    @param n number of transformations done
    @param k current depth
    @param acc multiset of sub-expressions met so far
    @return normalized term and multiset of subexpressions *)
  let rec fix ~n k acc t : term * mset =
    Global.debug 5 (get_loc t) "  ... CSE on %a" Pp.pp_term t;
    match t with
    | Kind | Type _ | DB _ | Const _ | Meta _ -> t, acc
    | App (f, a, l) ->
        let f, mset = fix ~n k empty_mset f in
        let a, mset = fix ~n k mset a in
        let l, mset = _fold_map (fix ~n k) mset l in
        let t = mk_App f a l in
        (* the new term, t, is a subexpression at depth k that is eligible
          for elimination (might require non-trivial evaluation) *)
        t, _add ~n:1 (_merge mset ~into:acc) (t,k)
    | Let (l,x,a,b) ->
        let b, acc = fix ~n (k+1) acc b in
        mk_Let l x a b, acc
    | Pi (l,x,a,b) ->
        (* just rewrite under. Pi is not an expression to eliminate *)
        let b, acc = fix ~n (k+1) acc b in
        mk_Pi l x a b, acc
    | Lam (l,x,a,b) ->
        (* ignore the [a]; compute set of expressions occurring in b *)
        let b, mset = fix ~n (k+1) empty_mset b in
        (* any sub-expressions to share? Such an expression must:
           - occur more than once in e
           - be well-scoped within the environment of b
           - contain x (the bound var), otherwise it should be shared deeper *)
        match _find
          (fun (e,k_e) n ->
            n >= 2
            && Subst.can_unshift (k_e-k-1) e
            && Subst.contains 0 b
          ) mset
        with
        | Some (e,k_e) ->
            (* replace b with "let x=e in b" where x is a fresh name *)
            let e = Subst.unshift (k_e-k-1) e in
            let name = gensym() in
            let b' = mk_Let dloc name e (replace_shift ~name 0 b e) in
            let t' = mk_Lam l x a b' in
            incr n;
            Global.debug_no_loc 3 "  CSE of %a" Pp.pp_term t;
            Global.debug_no_loc 3 "    into %a" Pp.pp_term t';
            Global.debug_no_loc 3 "    factoring (%a)" Pp.pp_term e;
            (* now try to find new/other subexpressions in t' *)
            fix ~n k acc t'
        | None ->
            (* do nothing *)
            mk_Lam l x a b, _merge mset ~into:acc
end


let common_subexpr_elim t =
  (* n: number of rewriting steps *)
  let n = ref 0 in
  let t', map = CSE.fix ~n 0 CSE.empty_mset t in
  if !n = 0 then None else Some t'

