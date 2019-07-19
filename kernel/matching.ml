open Basic
open Format
open Term
open Rule
open Ac

type Debug.flag += D_matching
let _ = Debug.register_flag D_matching "Matching"

exception NotUnifiable

type var_p = int * int LList.t

(* TODO: add loc to this to better handle errors *)
type 'a problem =
  | Eq of var_p * 'a
  | AC of ac_ident * int * (var_p list) * ('a list)

type pre_matching_problem = {
  pm_problems : int problem depthed list;
  pm_miller   : int array
}

type te = term Lazy.t

type status =
  | Unsolved
  | Solved of te
  | Partly of ac_ident * te list

type matching_problem = {
  problems : te problem depthed list;
  status   : status array;
  miller   : int array
}

(**     Printing functions       **)

let pp_pos fmt p = fprintf fmt "stack.%a" pp_print_int p
let pp_te fmt t = fprintf fmt "%a" pp_term (Lazy.force t)

let pp_depthed pp_a fmt (d,a) = fprintf fmt "%a" pp_a a

let pp_var_type fmt (i,args) =
  if LList.is_empty args
  then fprintf fmt "%i" i
  else fprintf fmt "%i (%a)" i (pp_list " " pp_print_int) (LList.lst args)

let rec pp_njoks fmt n = if n > 0 then fprintf fmt " + %i _" n

let pp_problem pp_a fmt = function
  | Eq(vp,t) -> fprintf fmt "%a = %a" pp_var_type vp pp_a t
  | AC(aci,joks,vars,terms) ->
     fprintf fmt "{ %a%a } =(%a) { %a }"
             (pp_list " , " pp_var_type) vars
             pp_njoks joks pp_ac_ident aci
             (pp_list " , " pp_a) terms

let pp_mp_problems sep pp_a fmt mp_p =
  fprintf fmt "[ %a ]" (pp_list sep (pp_depthed (pp_problem pp_a))) mp_p

let pp_indexed_status fmt (i,st) = match st with
  | Unsolved -> ()
  | Solved a -> fprintf fmt "%i = %a" i pp_te a
  | Partly(aci,terms) ->
     fprintf fmt "%i = %a{ %i', %a }" i
             pp_ac_ident aci i
             (pp_list " ; " pp_te) terms

let pp_mp_status sep fmt mp_s =
  let stl = Array.to_list (Array.mapi (fun i st -> (i,st)) mp_s) in
  if List.exists (function (i,Unsolved) -> false | _ -> true) stl
  then fprintf fmt "%swith [ %a ]" sep (pp_list " and " pp_indexed_status) stl

let pp_pre_matching_problem sep fmt mp = pp_mp_problems sep pp_pos fmt mp.pm_problems

let pp_matching_problem sep fmt mp =
  fprintf fmt "%a%a" (pp_mp_problems sep pp_te) mp.problems
                     (pp_mp_status   sep      ) mp.status

(**  Problem conversion from pre_problem  **)
let mk_matching_problem f g pre_problem =
  let convert_problem = function
    | Eq (vp,p)                -> Eq (vp, f p)
    | AC (aci,joks,vars,terms) -> AC(aci,joks,vars,g terms) in
  {
    problems =  List.map (fun (i,p) -> (i, convert_problem p)) pre_problem.pm_problems;
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
let solve d args t =
  if LList.is_empty args
  then try Subst.unshift d t with Subst.UnshiftExn -> raise NotUnifiable
  else solve_miller d args t

let force_solve reduce d i args t =
  if d == 0 then (assert(LList.is_empty args); t)
  else
    let te = Lazy.force t in
    Lazy.from_val( try solve d args te
                   with NotUnifiable -> solve d args (reduce te) )

let try_force_solve reduce d i args t =
  try Some (force_solve reduce d i args t)
  with NotUnifiable -> None

let rec add_n_lambdas n t =
  if n == 0 then t else add_n_lambdas (n-1) (mk_Lam dloc dmark None t)

let lazy_add_n_lambdas n t =
  if n == 0 then t
  else Lazy.from_val (add_n_lambdas n (Lazy.force t))


(** Returns term [t] applied to (local) variables [args] from a local context of size [d] *)
let convert_solution t args d =
  let args_DB = List.map (fun k -> mk_DB dloc dmark k) (LList.lst args) in
  mk_App2 (Subst.shift d (Lazy.force t)) args_DB

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
let var_exists  i = List.exists (fun (j,_) -> j == i)

type 'a update_res = Fail | Throw | Keep of 'a

let update_problems f pb =
  let rec update acc = function
    | [] -> Some {pb with problems = List.rev acc}
    | (d,p) :: tl ->
       match f d p with
       | Fail -> None
       | Throw -> update acc tl
       | Keep a -> update ( (d,a) :: acc) tl in
  update [] pb.problems

let update_status i s pb =
  let nstat = Array.copy pb.status in
  nstat.(i) <- s;
  {pb with status = nstat}

(** Resolves variable [i] = [t] *)
let set_unsolved reducer convertible whnf pb i sol =
  let filter d = function
    | Eq((vi,args),ti) as p ->
       if vi <> i then Keep p
       else
         let lambdaed = add_n_lambdas pb.miller.(i) (Lazy.force sol) in
         let shifted = Subst.shift d lambdaed in
         if convertible (Lazy.force ti) (apply_args shifted args)
         then Throw else Fail
    | AC(aci,joks,vars,terms) ->
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
       | None -> Fail
       | Some nterms ->
          let nvars = filter_vars i vars in
          if nvars = []
          then if nterms = [] || joks > 0
               then Throw
               else Fail
          else Keep ( AC(aci,joks,nvars,nterms)) in
  map_opt (update_status i (Solved sol))  (* update status of variable [i] *)
          (update_problems filter pb)     (* if the substitution is compatible *)

let set_partly pb i aci =
  assert(pb.status.(i) == Unsolved);
  update_status i (Partly(aci,[])) pb

let close_partly reduce convertible whnf pb d i =
  match pb.status.(i) with
  | Partly(aci,terms) ->
     (* Remove occurence of variable i from all m.v headed AC problems. *)
     let filter d = function
       | AC(aci',joks,vars,terms)
            when ac_ident_eq aci aci' && var_exists i vars ->
          let nvars = filter_vars i vars in
          if nvars = []
          then if terms = [] || joks > 0 then Throw else Fail
          else Keep( AC(aci,joks,nvars,terms) )
       | p -> Keep p
     in
     begin
       match update_problems filter pb with
       | None -> None (* If the substitution is incompatible, then fail *)
       | Some nprob ->
          let (cst,alg) = aci in
          if terms = [] then (* If [i] is closed on empty list *)
            match alg with
            | ACU neu ->
               set_unsolved reduce convertible whnf nprob i (Lazy.from_val neu)
            | _ -> None
          else
            let sol = Lazy.from_val (unflatten_AC aci (List.map Lazy.force terms)) in
            set_unsolved reduce convertible whnf nprob i sol
     end
  | _ -> assert false

let add_partly convertible pb i sol =
  match pb.status.(i) with
  | Partly(aci,terms) ->
     let filter d = function
       | AC(aci',joks,vars,terms)
            when ac_ident_eq aci aci' && var_exists i vars ->
          let lambdaed = add_n_lambdas pb.miller.(i) (Lazy.force sol) in
          let shifted = Subst.shift d lambdaed in
          let sols = compute_sols i shifted vars in
          begin
            match remove_sols_occs convertible sols terms with
            | None        -> Fail
            | Some nterms -> Keep( AC(aci,joks,vars,nterms) )
          end
       | p -> Keep p
     in
     map_opt
       (update_status i (Partly(aci,sol :: terms)))  (* Update status [i] *)
       (update_problems filter pb)                   (* If update was a success. *)
  | _ -> assert false


let get_all_ac_symbols pb i =
  let set = ref [] in
  let is_in_set e s = List.exists (ac_ident_eq e) s in
  let add e = if not (is_in_set e !set) then set := e :: !set in
  let rec aux (d,p) = match p with
    | Eq _ -> assert false
    | AC(aci,_,vars,_) -> if List.exists (fun (j,_) -> j == i) vars then add aci
  in
  List.iter aux pb.problems;
  !set

(** Fetches most interesting problem and most interesting variable in it. *)
let fetch_next_problem pb =
  match pb.problems with
  | [] -> None
  | (d, p) :: other_problems ->
     match p with
     | Eq((i,args),_) -> Some ((d,p),other_problems,(i,args))
     | AC(aci,joks,[],terms) -> Some ((d,p),other_problems,(-1,LList.nil))
     | AC(aci,joks,first :: vars,terms) ->
        (* Look for most interesting variable in the set. *)
        let score (i,args) =
          match pb.status.(i) with
          | Unsolved -> 0
          | Partly(aci',sols) ->
             if ac_ident_eq aci aci' then 1 + List.length sols else max_int
          | Solved _ -> assert false
        in
        let aux (bv,bs) v =
          let s = score v in
          if s < bs then (v,s) else (bv,bs) in
        let (best,_) = List.fold_left aux (first, score first) vars in
        Some ((d,p),other_problems,best)

let first_rearrange problems =
  (* TODO: better rearrange to have easiest AC sets first. *)
  let ac_f j v t = (List.length v, -List.length t, j > 0) in
  let comp (_,a) (_,b) = match a, b with
    | Eq _, AC _ -> -1
    | AC _, Eq _ -> 1
    | AC(_,j1,v1,t1), AC(_,j2,v2,t2) -> compare (ac_f j1 v1 t1) (ac_f j2 v2 t2)
    | _ -> 0 in
  List.sort comp problems


let get_subst pb =
  if pb.problems <> [] then None else
    let aux i = function
      | Solved sol -> Some (lazy_add_n_lambdas pb.miller.(i) sol)
      | _ -> None in
    Some( Array.mapi aux pb.status )


(** Main solving function *)
let solve_problem reduce convertible whnf pb =
  let problems = first_rearrange pb.problems in
  let rec solve_next pb =
    if pb.problems <> []
    then Debug.(debug D_matching "Problem: %a" (pp_matching_problem "    ") pb);
    let try_solve_next pb = bind_opt solve_next pb in
    match fetch_next_problem pb with
    | None -> get_subst pb (* If no problem left, compute substitution and return (success !) *)
    | Some ((d, p), other_problems, (i,args)) -> (* Else explore the problem fetched... *)
      match p with
      | Eq((j,_), term) -> (* If it's an easy equational problem*)
        begin
          assert (j == i);
          assert (pb.status.(i) == Unsolved);
          let solu = try_force_solve reduce d i args term in
          let npb = bind_opt
              (set_unsolved reduce convertible whnf {pb with problems=other_problems} i)
              solu in
          (* Update the rest of the problems with the solved variable and keep solving *)
          try_solve_next npb
        end
      | AC(aci,joks,[],terms) -> (* Left hand side empty. Fail or discard AC equation. *)
        if terms = [] || joks > 0
        then solve_next { pb with problems = other_problems }
        else None
      | AC(aci,joks,vars,terms) -> (* An AC equation *)
        match pb.status.(i) with
        | Partly(aci',sols) when ac_ident_eq aci aci' ->
          let rec try_add_terms = function
            | [] -> try_solve_next (close_partly reduce convertible whnf pb d i)
            | t :: tl ->
              let sol = try_force_solve reduce d i args t in
              let npb = bind_opt (add_partly convertible pb i) sol in
              match try_solve_next npb with
              | None -> try_add_terms tl
              | a -> a in
          try_add_terms terms
        | Partly(acip',sols) -> assert false
        | Unsolved ->
          let rec try_eq_terms = function
            | [] -> (* If all terms have been tried unsucessfully, then
                     * the variable [i] is a combination of terms under an AC symbol. *)
              let symbols = [aci] in (*get_all_ac_symbols pb i in*)
              let rec try_symbols = function
                | [] -> None
                | aci :: tl ->
                  match solve_next (set_partly pb i aci) with
                  | None -> try_symbols tl
                  | a -> a
              in
              try_symbols symbols
            | t :: tl ->
              let sol = try_force_solve reduce d i args t in
              let npb = bind_opt (set_unsolved reduce convertible whnf pb i) sol in
              match try_solve_next npb with
              | None -> try_eq_terms tl
              | a -> a in
          try_eq_terms terms
        | Solved _ -> assert false
  in
  solve_next { pb with problems = problems }
