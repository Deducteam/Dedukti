open Basic
open Term

module SS = Exsubst.ExSubst

type Debug.flag += D_SRChecking
let _ = Debug.register_flag D_SRChecking "SRChecking"

let srfuel = ref 1

(* Check whether two pairs of terms are unifiable (one way or the other) *)
let cstr_eq ((n1,t1,u1):cstr) ((n2,t2,u2):cstr) =
  let t1',u1' = Subst.shift n2 t1, Subst.shift n2 u1 in
  let t2',u2' = Subst.shift n1 t2, Subst.shift n1 u2 in
  (term_eq t1' t2' && term_eq u1' u2') || (term_eq t1' u2' && term_eq u1' t2')

module SRChecker(R:Reduction.S) =
struct
  type t =
    {
      subst    : SS.t;
      unsolved : cstr list;
      unsatisf : cstr list
    }

  let empty : t = { subst = SS.identity; unsolved=[]; unsatisf=[] }

  let get_subst c = c.subst
  let get_unsat c = match c.unsatisf with [] -> None | c::_ -> Some c

  let snf sg c d =
    let rec aux fuel t =
      let t1,flag = SS.apply c.subst d t in
      let t2 = R.snf sg t1 in
      if flag && fuel <> 0 then aux (fuel-1) t2 else t2 in
    aux !srfuel

  let whnf sg c d =
    let rec aux fuel t =
      let t1,flag = SS.apply c.subst d t in
      let t2 = R.whnf sg t1 in
      if flag && fuel <> 0 then aux (fuel-1) t2 else t2 in
    aux !srfuel

  (* Syntactical match against all unsolved equations *)
  let term_eq_under_cstr (eq_cstr:cstr list) : term -> term -> bool =
    let rec aux = function
      | [] -> true
      | (n,t1,t2)::tl ->
         List.exists (cstr_eq (n,t1,t2)) eq_cstr ||
         match t1,t2 with
         | App(h1,a1,l1), App(h2,a2,l2) ->
            List.length l1 = List.length l2 &&
            aux ((n,h1,h2)::(n,a1,a2)::(List.map2 (fun x y -> (n,x,y)) l1 l2)@tl)
         | Lam(_,_,_,t1), Lam(_,_,_,t2) -> aux ((n+1,t1,t2)::tl)
         | Pi(_,_,a1,b1), Pi(_,_,a2,b2) -> aux ((n,a1,a2)::(n+1,b1,b2)::tl)
         | _ -> term_eq t1 t2 && aux tl
    in fun t1 t2 -> eq_cstr <> [] && aux [(0,t1,t2)]

  let convertible (sg:Signature.t) (c:t) (depth:int) (ty_inf:term) (ty_exp:term) : bool =
    R.are_convertible sg ty_inf ty_exp ||
    match SS.is_identity c.subst, c.unsolved with
    | true, [] -> false
    | true, _ -> term_eq_under_cstr c.unsolved (R.snf sg ty_inf) (R.snf sg ty_exp)
    | false,_ ->
       let snf_ty_inf = snf sg c depth ty_inf in
       let snf_ty_exp = snf sg c depth ty_exp in
       R.are_convertible sg snf_ty_inf snf_ty_exp ||
       term_eq_under_cstr c.unsolved snf_ty_inf snf_ty_exp

    (* **** PSEUDO UNIFICATION ********************** *)


  let rec add_to_list q acc l1 l2 = match l1, l2 with
    | [], [] -> Some acc
    | h1::t1, h2::t2 -> add_to_list q ((q,h1,h2)::acc) t1 t2
    | _, _ -> None

  let unshift_reduce sg q t =
    try Some (Subst.unshift q t)
    with Subst.UnshiftExn ->
      ( try Some (Subst.unshift q (R.snf sg t))
    with Subst.UnshiftExn -> None )

  (** Under [d] lambdas, checks whether term [te] *must* contain an occurence
      of any variable that satisfies the given predicate [p],
      *even when substituted or reduced*.
      This check make no assumption on the rewrite system or possible substitution
      - any definable symbol are "safe" as they may reduce to a term where no variable occur
      - any applied meta variable (DB index > [d]) are "safe" as they may be
      substituted and reduce to a term where no variable occur
      Raises VarSurelyOccurs if the term [te] *surely* contains an occurence of one
      of the [vars].
  *)
  let sure_occur_check sg (d:int) (p:int -> bool) (te:term) : bool =
    let exception VarSurelyOccurs in
    let rec aux = function
      | [] -> ()
      | (k,t) :: tl -> (* k counts the number of local lambda abstractions *)
     match t with
     | Kind | Type _ | Const _ -> aux tl
     | Pi  (_,_,     a,b) -> aux ((k,a)::(k+1,b)::tl)
     | Lam (_,_,None  ,b) -> aux (       (k+1,b)::tl)
     | Lam (_,_,Some a,b) -> aux ((k,a)::(k+1,b)::tl)
     | DB (_,_,n) -> if n >= k && p (n-k) then raise VarSurelyOccurs else aux tl
     | App (f,a,args) ->
       begin
         match f with
         | DB (_,_,n) ->
           if n >= k && p (n-k)
           then raise VarSurelyOccurs
           else if n >= k + d (* a matching variable *)
           then aux tl
           else aux ( (k, a):: (List.map (fun t -> (k,t)) args) @ tl)
         | Const (l,cst) when Signature.is_static sg l cst ->
           (  aux ( (k, a):: (List.map (fun t -> (k,t)) args) @ tl) )
         | _ -> aux tl
         (* Default case encompasses:
            - Meta variables: DB(_,_,n) with n >= k + d
            - Definable symbols
            - Lambdas (FIXME: when can this happen ?)
            - Illegal applications  *)
       end
    in
    try aux [(0,te)]; false
    with VarSurelyOccurs -> true

(** Under [d] lambdas, gather all free variables that are *surely*
    contained in term [te]. That is to say term [te] will contain
    an occurence of these variables *even when substituted or reduced*.
    This check make no assumption on the rewrite system or possible substitutions
    - applied definable symbols *surely* contain no variable as they may
    reduce to terms where their arguments are erased
    - applied meta variable (DB index > [d]) *surely* contain no variable as they
    may be substituted and reduce to a term where their arguments are erased
    Sets the indices of *surely* contained variables to [true] in the [vars]
    boolean array which is expected to be of size (at least) [d].
*)
  let gather_free_vars (d:int) (terms:term list) : bool array =
    let vars = Array.make d false in
    let rec aux = function
      | [] -> ()
      | (k,t) :: tl -> (* k counts the number of local lambda abstractions *)
     match t with
     | DB (_,_,n) -> (if n >= k && n < k + d then vars.(n-k) <- true); aux tl
     | Pi  (_,_,     a,b) -> aux ((k,a)::(k+1,b)::tl)
     | Lam (_,_,None  ,b) -> aux (       (k+1,b)::tl)
     | Lam (_,_,Some a,b) -> aux ((k,a)::(k+1,b)::tl)
     | App (f,a,args)     -> aux ((k,f)::(k,a):: (List.map (fun t -> (k,t)) args) @ tl)
     | _ -> aux tl
    in aux (List.map (fun t -> (0,t)) terms); vars

  let try_solve q args t =
    try
      let dbs = List.map (function DB(_,_,n) -> n | _ -> raise Matching.NotUnifiable) args in
      Some (Matching.solve q (LList.of_list dbs) t)
    with Matching.NotUnifiable -> None

  let rec pseudo_u sg flag (s:t) : cstr list -> bool*t = function
    | [] -> (flag, s)
    | (q,t1,t2)::lst -> begin
        let t1' = whnf sg s q t1 in
        let t2' = whnf sg s q t2 in
        Debug.(debug D_SRChecking) "Processing: %a = %a" pp_term t1' pp_term t2';
        let dropped ()     = pseudo_u sg flag s lst in
        let unsolved ()    = pseudo_u sg flag { s with unsolved=(q,t1',t2')::s.unsolved } lst in
        let unsatisf ()    = pseudo_u sg true { s with unsatisf=(q,t1',t2')::s.unsolved } lst in
        let subst db ar te = pseudo_u sg true { s with subst   =SS.add s.subst db ar te } lst in
        if term_eq t1' t2' then dropped ()
        else
          match t1', t2' with
          | Kind, Kind | Type _, Type _       -> assert false (* Equal terms *)
          | DB (_,_,n), DB (_,_,n') when n=n' -> assert false (* Equal terms *)
          | _, Kind | Kind, _ |_, Type _ | Type _, _ -> unsatisf ()

          | Pi (_,_,a,b), Pi (_,_,a',b') ->
            pseudo_u sg true s ((q,a,a')::(q+1,b,b')::lst)
          | Lam (_,_,_,b), Lam (_,_,_,b') ->
            pseudo_u sg true s ((q+1,b,b')::lst)

          (* Potentially eta-equivalent terms *)
          | Lam (_,i,_,b), a when !Reduction.eta ->
            let b' = mk_App (Subst.shift 1 a) (mk_DB dloc i 0) [] in
            pseudo_u sg true s ((q+1,b,b')::lst)
           | a, Lam (_,i,_,b) when !Reduction.eta ->
             let b' = mk_App (Subst.shift 1 a) (mk_DB dloc i 0) [] in
             pseudo_u sg true s ((q+1,b,b')::lst)

           (* A definable symbol is only be convertible with closed terms *)
           | Const (l,cst), t when not (Signature.is_static sg l cst) ->
             if sure_occur_check sg q (fun k -> k <= q) t then unsatisf() else unsolved()
           | t, Const (l,cst) when not (Signature.is_static sg l cst) ->
             if sure_occur_check sg q (fun k -> k <= q) t then unsatisf() else unsolved()

           (* X = Y :  map either X to Y or Y to X *)
           | DB (l1,x1,n1), DB (l2,x2,n2) when n1>=q && n2>=q ->
             let (n,t) = if n1<n2
               then (n1,mk_DB l2 x2 (n2-q))
               else (n2,mk_DB l1 x1 (n1-q)) in
             subst (n-q) 0 t

           (* X = t :
              1) make sure that t is possibly closed and without occurence of X
           2) if by chance t already is so, then map X to t
              3) otherwise drop the constraint *)
           | DB (_,_,n), t when n>=q ->
             if sure_occur_check sg q (fun k -> k < q || k = n) t
             then unsatisf()
             else begin
               match unshift_reduce sg q t with
               | None    -> unsolved()
               | Some ut ->
                 let n' = n-q in
                 if Subst.occurs n' ut
                 then
                   let t' = R.snf sg ut in
                   if Subst.occurs n' t' then unsatisf() else subst n' 0 t'
                 else subst n' 0 ut
             end
           | t, DB (_,_,n) when n>=q ->
             if sure_occur_check sg q (fun k -> k < q || k = n) t
             then unsatisf()
             else begin
               match unshift_reduce sg q t with
               | None    -> unsolved()
               | Some ut ->
                 let n' = n-q in
                 if Subst.occurs n' ut
                 then
                   let t' = R.snf sg ut in
                   if Subst.occurs n' t' then unsatisf() else subst n' 0 t'
                 else subst n' 0 ut
             end

           (* f t1 ... tn    /    X t1 ... tn  =  u
              1) Gather all free variables in t1 ... tn
              2) Make sure u only relies on these variables
           *)
           | App (DB (_,_,n),a,args), t when n >= q ->
             let occs = gather_free_vars q (a::args) in
             if sure_occur_check sg q (fun k -> k < q && not occs.(k)) t
             then unsatisf()
             else begin
               match try_solve q (a::args) t with
               | None    -> unsolved()
               | Some ut ->
                 let n' = n-q in
                 let t' = if Subst.occurs n' ut then ut else R.snf sg ut in
                 if Subst.occurs n' t' then unsolved()
                 (* X = t[X]  cannot be turned into a (extended-)substitution *)
                 else subst n' (1+(List.length args)) t'
              end
           | t, App (DB (_,_,n),a,args) when n >= q ->
              let occs = gather_free_vars q (a::args) in
              if sure_occur_check sg q (fun k -> k < q && not occs.(k)) t
              then unsatisf()
              else begin
                match try_solve q (a::args) t with
                | None    -> unsolved()
                | Some ut ->
                  let n' = n-q in
                  let t' = if Subst.occurs n' ut then ut else R.snf sg ut in
                  if Subst.occurs n' t' then unsolved()
                  (* X = t[X]  cannot be turned into a (extended-)substitution *)
                  else subst n' (1+(List.length args)) t'
              end
           | App (Const (l,cst),a,args), t when not (Signature.is_static sg l cst) ->
             let occs = gather_free_vars q (a::args) in
             if sure_occur_check sg q (fun k -> k < q && not occs.(k)) t
             then unsatisf() else unsolved()
           | t, App (Const (l,cst),a,args) when not (Signature.is_static sg l cst) ->
             let occs = gather_free_vars q (a::args) in
             if sure_occur_check sg q (fun k -> k < q && not occs.(k)) t
             then unsatisf() else unsolved()

           | App (f,a,args), App (f',a',args') ->
             (* f = Kind | Type | DB n when n<q | Pi _
              * | Const name when (is_static name) *)
             begin
               match add_to_list q lst args args' with
               | None -> unsatisf() (* Different number of arguments. *)
               | Some lst2 -> pseudo_u sg true s ((q,f,f')::(q,a,a')::lst2)
             end

           | _, _ -> unsatisf()
       end

  let compile_cstr (sg : Signature.t) (cstr : cstr list) : t =
    (* Successively runs pseudo_u to apply solved constraints to the remaining
       unsolved constraints in the hope to deduce more constraints in solved form *)
    let rec process_solver fuel sol =
      match pseudo_u sg false { sol with unsolved = [] } sol.unsolved with
      | false, s    -> s (* When pseudo_u did nothing *)
      | true , sol' ->
        if fuel = 0 then sol'
        else process_solver (fuel-1) {sol' with subst=SS.mk_idempotent sol'.subst}
    in
    (* TODO: this function is given some fuel. In practice 1 is enough for all tests.
       We should write a test to force a second reentry in the loop. *)
    process_solver (!srfuel) {subst=SS.identity;unsolved=cstr;unsatisf=[]}

  let optimize sg c = (* Substitutes are put in SNF *)
    { c with unsolved=List.map (fun (n,t,u) -> (n,R.snf sg t,R.snf sg u)) c.unsolved }
end
