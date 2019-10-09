open Basic
open Format
open Term
open Ac

type Debug.flag += D_matching
let _ = Debug.register_flag D_matching "Matching"

exception NotUnifiable

type var_p = int * int LList.t

(* TODO: add loc to this to better handle errors *)
type 'a eq_problem = int * int * int LList.t * 'a

type 'a ac_problem = int * ac_ident * int * (var_p list) * ('a list)

type pre_matching_problem = {
  pm_eq_problems : int eq_problem list;
  pm_ac_problems : int ac_problem list;
  pm_miller      : int array
}

type te = term Lazy.t

type status =
  | Unsolved
  | Solved of te
  | Partly of ac_ident * te list

type matching_problem = {
  eq_problems : te eq_problem list;
  ac_problems : te ac_problem list;
  status      : status array;
  miller      : int array
}

(**     Printing functions       **)

let pp_pos fmt p = fprintf fmt "stack.%a" pp_print_int p
let pp_te fmt t = fprintf fmt "%a" pp_term (Lazy.force t)

let pp_var_type fmt (i,args) =
  if LList.is_empty args
  then fprintf fmt "%i" i
  else fprintf fmt "%i[%a]" i (pp_list " " pp_print_int) (LList.lst args)

let pp_njoks fmt n = if n > 0 then fprintf fmt " + %i _" n

let pp_eq_problem pp_a fmt (_,vp,args,t) =
  fprintf fmt "%a = %a" pp_var_type (vp,args) pp_a t

let pp_ac_problem pp_a fmt (_,aci,joks,vars,terms) =
  fprintf fmt "{ %a%a } =(%a) { %a }"
    (pp_list " , " pp_var_type) vars
    pp_njoks joks pp_ac_ident aci
    (pp_list " , " pp_a) terms

let pp_indexed_status fmt (i,st) = match st with
  | Unsolved -> ()
  | Solved a -> fprintf fmt "%i = %a" i pp_te a
  | Partly(aci,terms) ->
     fprintf fmt "%i = %a{ %i', %a }" i
             pp_ac_ident aci i
             (pp_list " ; " pp_te) terms

let pp_mp_status sep fmt mp_s =
  let stl = Array.to_list (Array.mapi (fun i st -> (i,st)) mp_s) in
  if List.exists (function (_,Unsolved) -> false | _ -> true) stl
  then fprintf fmt "%swith [ %a ]" sep (pp_list " and " pp_indexed_status) stl

let pp_mp_problems sep pp_a fmt eq_p ac_p =
  fprintf fmt "[ %a | %a ]"
    (pp_list sep (pp_eq_problem pp_a)) eq_p
    (pp_list sep (pp_ac_problem pp_a)) ac_p

let pp_pre_matching_problem sep fmt mp =
  pp_mp_problems sep pp_pos fmt mp.pm_eq_problems mp.pm_ac_problems

let pp_matching_problem sep fmt mp =
  pp_mp_problems sep pp_te fmt mp.eq_problems mp.ac_problems;
  pp_mp_status   sep       fmt mp.status

(**  Problem conversion from pre_problem  *)
let mk_matching_problem f g pre_problem =
  {
    eq_problems =
      List.map (fun (d,i,args,p) -> (d,i,args,f p))
        pre_problem.pm_eq_problems;
    ac_problems =
      List.map (fun (d,aci,joks,vars,rhs) -> (d,aci,joks,vars,g rhs))
        pre_problem.pm_ac_problems;
    status   = Array.make (Array.length pre_problem.pm_miller) Unsolved;
    miller   = Array.copy pre_problem.pm_miller
  }

(** Solve the following problem for lambda term X:
 *    (lambda^[depth]. X) [args] = [t] where [args] are distincts bound variables
 * Raises NotUnifiable if [t] contains variables not in [args]. *)
let solve_miller (depth:int) (args:int LList.t) (te:term) : term =
  let size = LList.len args in
  let arr = Array.make depth None in
  List.iteri ( fun i n -> arr.(n) <- Some (size-i-1) ) (LList.lst args);
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (l,x,n) as t ->
      if n < k             (* var bound in te *) then t
      else if n >= k+depth (* var free  in te *) then mk_DB l x (n-depth+size)
      else mk_DB l x (match arr.(n-k) with None -> raise NotUnifiable | Some n' -> n'+k)
    | Lam (l,x,a,b) -> mk_Lam l x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (l,x,a,b) -> mk_Pi  l x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in
  aux 0 te

(** [solve n k_lst te] solves the following higher-order unification problem:
    (unification modulo beta)

    x{_1} => x{_2} => ... x{_[n]} => X x{_i{_1}} .. x{_i{_m}}
    {b =}
    x{_1} => x{_2} => ... x{_[n]} => [te]

    where X is the unknown, x{_i{_1}}, ..., x{_i{_m}} are distinct bound variables
    in the local context and [te] is a term.

   If the free variables of [te] that are in x{_1}, ..., x{_[n]} are also in
   x{_i{_1}}, ..., x{_i{_m}} then the problem has a unique solution modulo beta that is
   x{_i{_1}} => .. => x{_i{_m}} => [te].
   Otherwise this problem has no solution and the function raises [NotUnifiable].

   Since we use deBruijn indexes, the problem is given as the equation

   x{_1} => ... => x{_[n]} => X DB(k{_0}) ... DB(k{_m}) =~ x{_1} => ... => x{_[n]} => [te]

   and where [k_lst] = [\[]k{_0}[; ]k{_1}[; ]...[; ]k{_m}[\]].
*)
let solve (depth:int) (args:int LList.t) (te:term) : term =
  if LList.is_empty args
  then try Subst.unshift depth te with Subst.UnshiftExn -> raise NotUnifiable
  else solve_miller depth args te

(* Solves  lambda^[d]  X [args]1 ... [args]n = [t]
   Raises NotUnifiable if no solution exists *)
let force_solve reduce d args t =
  if d == 0 then (assert(LList.is_empty args); t)
  else
    let te = Lazy.force t in
    Lazy.from_val( try solve d args te
                   with NotUnifiable -> solve d args (reduce te) )

(* Try to solve returns None if it fails *)
let try_force_solve reduce d args t =
  try Some (force_solve reduce d args t)
  with NotUnifiable -> None

let rec add_n_lambdas n t =
  if n == 0 then t else add_n_lambdas (n-1) (mk_Lam dloc dmark None t)

let lazy_add_n_lambdas n t =
  if n == 0 then t
  else Lazy.from_val (add_n_lambdas n (Lazy.force t))


(** Returns term [t] applied to (local) variables [args] from a local context of size [d] *)
(* let convert_solution t args d =
 *   let args_DB = List.map (fun k -> mk_DB dloc dmark k) (LList.lst args) in
 *   mk_App2 (Subst.shift d (Lazy.force t)) args_DB *)

(** Apply a Miller solution to variables *)
let apply_args t l = mk_App2 t (List.map (fun k -> mk_DB dloc dmark k) (LList.lst l))


(** Compute all right hand terms fixed by substitution i = t *)
let compute_sols i sol =
  let rec aux acc = function
  | [] -> acc
  | (v,args) :: tl when v == i ->
     aux ( (apply_args sol args) :: acc) tl
  | _ :: tl -> aux acc tl in
  aux []

(** Compute AC list of all right hand terms fixed by substitution i = +{ [terms] } *)
let compute_all_sols i vars =
  let rec aux acc = function
    | [] -> acc
    | sol :: osol ->
       aux (List.rev_append (compute_sols i sol vars) acc) osol
  in
  aux []

(** Remove term [sol] once from list [l]  *)
let remove_sol convertible sol l =
  let rec aux acc = function
    | [] -> None
    | hd :: tl ->
       if convertible (Lazy.force hd) sol
       then Some (List.rev_append acc tl)
       else aux (hd::acc) tl in
  aux [] l

(** Remove each term in [sols] once from the [terms] list. *)
let rec remove_sols_occs convertible sols terms = match sols with
  | [] -> Some(terms)
  | sol :: tl -> bind_opt (remove_sols_occs convertible tl)
                          (remove_sol convertible sol terms)

let filter_vars i = List.filter (fun (j,_) -> i != j)
let var_exists  i = List.exists (fun (j,_) -> i == j)


(* Updating a matching_problem is done by updating successively
   the atomic problems. While updating an atomic problems:
   - [raise Fail_update] means the whole matching_problem is now unsolvable
   - [Some a]            means the problem is replaced with [a]
   - [None]              means the problem is now solved / trivial / irrelevant
                         remove it from the matching_problem *)
exception Fail_update

let update f =
  let rec update acc = function
    | [] -> Some (List.rev acc)
    | hd :: tl ->
      try match f hd with
        | None         -> update acc      tl
        | Some a       -> update (a::acc) tl
      with Fail_update -> None
  in
  update []

let update_ac_problems ac_f pb =
  map_opt
    (fun nac -> { pb with ac_problems = nac  })
    (update ac_f pb.ac_problems)

let update_problems eq_f ac_f pb =
  bind_opt
    (fun neq ->
       map_opt
         (fun nac -> { pb with eq_problems = neq ; ac_problems = nac  })
         (update ac_f pb.ac_problems))
    (update eq_f pb.eq_problems)


let update_status i s pb =
  let nstat = Array.copy pb.status in
  nstat.(i) <- s;
  {pb with status = nstat}

(** Resolves variable [i] = [t] *)
let set_unsolved reducer convertible whnf pb i sol =
  let filter_eq p =
    let (d, vi, args, ti) = p in
      if vi <> i then Some p
      else
        let lambdaed = add_n_lambdas pb.miller.(i) (Lazy.force sol) in
        let shifted = Subst.shift d lambdaed in
        if convertible (Lazy.force ti) (apply_args shifted args)
        then None else raise Fail_update
  in
  let filter_ac (d,aci,joks,vars,terms) =
    let sol = whnf (Lazy.force sol) in
    (* If sol's whnf is still headed by the same AC-symbol then flatten it. *)
    let flat_sols = force_flatten_AC_term reducer (fst aci) sol in
    (* If aci represent ACU symbol, remove corresponding neutral element. *)
    let flat_sols =
      match snd aci with
      | ACU neu -> List.filter (fun x -> not (convertible neu x)) flat_sols
      | _ -> flat_sols in
    let lambdaed = List.map (add_n_lambdas pb.miller.(i)) flat_sols in
    let shifted = List.map (Subst.shift d) lambdaed in
    let sols = compute_all_sols i vars shifted in
    match remove_sols_occs convertible sols terms with
    | None -> raise Fail_update
    | Some nterms ->
      let nvars = filter_vars i vars in
      if nvars = []
      then if nterms = [] || joks > 0
        then None
        else raise Fail_update
      else Some (d,aci,joks,nvars,nterms) in
  map_opt (update_status i (Solved sol))  (* update status of variable [i] *)
    (update_problems filter_eq filter_ac pb)     (* if the substitution is compatible *)

let set_partly pb i aci =
  assert(pb.status.(i) == Unsolved);
  update_status i (Partly(aci,[])) pb

(* A problem of the shape +{X, Y, Z} = +{} has been found.
   Partly solved variable   X = +{a,b,c}
   May no longer be added extra arguments, it is in solved form.
   For each other problem still containing it, remove this variable from the LHS
*)
let close_partly reduce convertible whnf pb i =
  match pb.status.(i) with
  | Partly(aci,terms) ->
     (* Remove occurence of variable i from all m.v headed AC problems. *)
    let filter p =
      let (d,aci',joks,vars,rhs) = p in
      if ac_ident_eq aci aci' && var_exists i vars
      then
         ( match filter_vars i vars with
           | [] -> if rhs = [] || joks > 0 then None else raise Fail_update
           | filtered_vars -> Some (d,aci,joks,filtered_vars,rhs) )
      else Some p
     in
     begin
       match update_ac_problems filter pb with
       | None -> None (* If the substitution is incompatible, then fail *)
       | Some nprob ->
         if terms = [] then (* If [i] is closed on empty list: X = +{} *)
           match aci with
           | _, ACU neu -> (* When + is ACU, it's ok, X = neutral *)
             set_unsolved reduce convertible whnf nprob i (Lazy.from_val neu)
           | _ -> None (* Otherwise no solution *)
         else
           let sol = Lazy.from_val (unflatten_AC aci (List.map Lazy.force terms)) in
           set_unsolved reduce convertible whnf nprob i sol
     end
  | _ -> assert false

let add_partly convertible pb i sol =
  match pb.status.(i) with
  | Partly(aci,terms) ->
    let filter p =
      let (d,aci',joks,vars,terms) = p in
      if ac_ident_eq aci aci' && var_exists i vars
      then
        let lambdaed = add_n_lambdas pb.miller.(i) (Lazy.force sol) in
        let shifted = Subst.shift d lambdaed in
        let sols = compute_sols i shifted vars in
        begin
          match remove_sols_occs convertible sols terms with
          | None        -> raise Fail_update
          | Some nterms -> Some (d,aci,joks,vars,nterms)
        end
      else Some p
    in
    map_opt
      (update_status i (Partly(aci,sol :: terms))) (* Update status [i] *)
      (update_ac_problems filter pb)               (* If update was a success. *)
  | _ -> assert false


(*
let get_all_ac_symbols pb i =
  let set = ref [] in
  let is_in_set e s = List.exists (ac_ident_eq e) s in
  let add e = if not (is_in_set e !set) then set := e :: !set in
  let aux = function
    | _, AC(aci,_,vars,_) -> if List.exists (fun (j,_) -> j == i) vars then add aci
    | _ -> assert false
  in
  List.iter aux pb.problems;
  !set
*)

(* Fetches most interesting problem and most interesting variable in it.
   Returns None iff the list of remaining problems is empty
   Current implementation always returns the first problem
   and select a variable in it based on the highest following score:
*)
let fetch_next_problem pb =
  match pb.ac_problems with
  | [] -> None
  | p :: other_problems ->
    Some (p, other_problems,
          let (_,aci,_,vars,_) = p in
          (* Look for most interesting variable in the set. *)
          let score (i,_) =
            match pb.status.(i) with
            | Unsolved -> 0
            | Partly(aci',sols) ->
              if ac_ident_eq aci aci' then 1 + List.length sols else max_int-1
            | Solved _ -> assert false
            (* Variables are removed from all problems when they are solved *)
          in
          let aux (bv,bs) v =
            let s = score v in
            if s < bs then (v,s) else (bv,bs) in
          fst (List.fold_left aux ((-1,LList.nil),max_int) vars)
         )

let get_subst pb =
  if pb.ac_problems <> [] then None else
    let aux i = function
      | Solved sol -> Some (lazy_add_n_lambdas pb.miller.(i) sol)
      | _ -> None in
    Some( Array.mapi aux pb.status )

let solve_ac_problem reduce convertible whnf =
  let rec solve_next pb =
    match fetch_next_problem pb with
    | None -> get_subst pb
    (* If no problem left, compute substitution and return (success !) *)

    | Some (p, other_problems, (i,args)) ->
      (* Else explore the problem fetched... *)
      match p with
      | (_,_,joks,[],terms) -> (* If we've chosen an AC equation: +{ X ... } = +{} *)
        (* Left hand side is now empty.
           Either fail (non empty RHS and no joker to match it)
           or simply discard AC equation. *)
        if terms = [] || joks > 0
        then solve_next { pb with ac_problems = other_problems }
        else None
      | (d,aci,_,_,rhs_terms) ->
        (* If we've chosen an AC equation: +{ X, ... } = +{ ... } *)
        match pb.status.(i) with
        | Partly(aci',_) -> (* If X = +{X' ... }*)
          assert (ac_ident_eq aci aci'); (* It can't be the case that X = max{X' ... } *)
          let rec try_add_terms = function
            | [] -> try_solve_next (close_partly reduce convertible whnf pb i)
            | t :: tl -> (* Pick a term [t] in RHS set *)
              let sol = try_force_solve reduce d args t in
              (* Solve  lambda^[d]  X [args]1 ... [args]n = [t] *)
              let npb = bind_opt (add_partly convertible pb i) sol in
              (* Add the solution found to the AC-set of partial solution for [i] *)
              match try_solve_next npb with (* Keep solving *)
              | None -> try_add_terms tl
              (* If it failed, backtrack and proceed with the other RHS terms *)
              | a -> a in (* If it succeeds, return the solution *)
          try_add_terms rhs_terms
        | Unsolved ->
          let rec try_eq_terms = function
            | t :: tl ->  (* Pick a term [t] in the RHS set *)
              let sol = try_force_solve reduce d args t in
              (* Solve  lambda^[d]  X [args]1 ... [args]n = [t] *)
              let npb = bind_opt (set_unsolved reduce convertible whnf pb i) sol in
              (* Hope that we can just set X = solution and have solved for X *)
              (* FIXME: This is highly inefficient !
                 We first try X = sol then try again  X = +{sol ...}
                 thus trying to solve twice the same problem *)
              ( match try_solve_next npb with
                | None -> try_eq_terms tl
                (* If it failed, backtrack and proceed with the other RHS terms *)
                | a -> a )  (* If it succeeds, return the solution *)
            | [] -> (* If all terms have been tried unsucessfully, then
                     * the variable [i] is a combination of terms under an AC symbol. *)
              (* NOTE: It seems the commented block should remain commented:
                 At this point, the problem
                     +{X ...} = +{a b ... z}
                 Can only have a solution of the shape X = +{...} otherwise
                 X would be equal to one of the a b ... z  which has already been checked.
                 CAREFUL ! If we remove the check (cf FIXME above) then we need to try
                 all possible AC symbols for X.
              *)
              (*
              let possible_symbols = get_all_ac_symbols pb i in
              let rec try_symbols = function
                | [] -> None
                | aci :: tl ->
              in
              try_symbols possible_symbols
              *)
              solve_next (set_partly pb i aci)
          in
          try_eq_terms rhs_terms
        | Solved _ -> assert false
  and try_solve_next pb = bind_opt solve_next pb in
  solve_next


(* Rearranges to have easiest AC sets first.
   - Least number of variables first
   - Least number of LHS terms first
   - In case of a tie, problems with at least a joker should be handled second
   *)
let ac_rearrange problems =
  let ac_f j v t = (List.length v, -List.length t, j > 0) in
  let comp (_,_,j1,v1,t1) (_,_,j2,v2,t2) =
    compare (ac_f j1 v1 t1) (ac_f j2 v2 t2) in
  List.sort comp problems

(* Main solving function.
   Processes equationnal problems as they can be deterministically solved right away
   then hands over to non deterministic AC solver. *)
let rec solve_problem reduce convertible whnf pb =
  Debug.(debug D_matching "Problem: %a@." (pp_matching_problem "    ") pb);
  match pb.eq_problems with
  | [] ->
    (* Call AC solver on rearranged AC problems (easiest first) *)
    solve_ac_problem reduce convertible whnf
      {pb with ac_problems = ac_rearrange pb.ac_problems }
  | (d,v,args,rhs) :: other_problems ->
    assert (pb.status.(v) == Unsolved); (* Selected var is unsolved *)
    let solu = try_force_solve reduce d args rhs in
    let npb = bind_opt
        (set_unsolved reduce convertible whnf {pb with eq_problems=other_problems} v)
        solu in
    (* Update the rest of the problems with the solved variable and keep solving *)
    bind_opt (solve_problem reduce convertible whnf) npb
