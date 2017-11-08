open Basic
open Format
open Term
open Rule

exception NotUnifiable

(** ([n], [t]) represents the term represented by [t] under [n] lambda abstractions. *)
type 'a depthed = int * 'a

type problem_type =
  | Syntactic
  | MillerPattern of int LList.t

let mk_problem_type = function
  | [] -> Syntactic
  | l -> MillerPattern (LList.of_list l)

type 'a problem_relation =
  | Eq        of 'a depthed
  | AC_Subset of int * int

(* TODO: add loc to this to better handle errors *)
type 'a ac_set  = ident * ident * int * 'a list

type 'a problem =
  | Solved of 'a
  | Unsolved of (problem_type * 'a problem_relation) list

type 'a matching_problem =
  {
    problems  : 'a problem array;
    ac_sets   : 'a ac_set depthed array;
  }


(**     Printing functions       **)

let pp_pos = pp_print_int
let pp_lazy_term fmt t = fprintf fmt "%a" pp_term (Lazy.force t)

let pp_depthed pp_a fmt (d,a) = fprintf fmt "%a" pp_a a

let pp_problem_type fmt = function 
  | Syntactic       -> fprintf fmt "Sy"
  | MillerPattern l -> fprintf fmt "Mi[%a]" (pp_list " " pp_print_int) (LList.lst l)

let pp_problem pp_a fmt = function
  | Eq p            -> fprintf fmt "= %a" (pp_depthed pp_a) p
  | AC_Subset (i,n) -> fprintf fmt "%i times in set %i" n i

let pp_typed_problem pp_a fmt (ptype,p) =
  fprintf fmt "%a %a" pp_problem_type ptype (pp_problem pp_a) p

let pp_indexed_prob pp_a fmt (i, prob) = match prob with
  | Unsolved [Syntactic      , Eq p] -> fprintf fmt "%i = %a"      i (pp_depthed pp_a) p
  | Unsolved [MillerPattern l, Eq p] ->
     fprintf fmt "%i (%a) = %a" i (pp_list " " pp_print_int) (LList.lst l) (pp_depthed pp_a) p
  | Unsolved plist -> fprintf fmt "%i : { %a }" i (pp_list " ; " (pp_typed_problem pp_a)) plist
  | Solved t -> fprintf fmt "%i = %a" i pp_a t

let pp_mp_problems pp_a fmt mp_p =
  fprintf fmt "[ %a ]" (pp_list " and " (pp_indexed_prob pp_a))
          (Array.to_list (Array.mapi (fun i x -> (i,x)) mp_p))

let pp_ac_set pp_a fmt (m,v,nvars,l) =
  fprintf fmt "%i vars in %a.%a{%a}" nvars pp_ident m pp_ident v (pp_list "; " pp_a) l
let pp_indexed_depthed_ac_set pp_a fmt (i,dset) =
  fprintf fmt "%i : %a" i (pp_depthed (pp_ac_set pp_a)) dset

let pp_mp_ac_sets sep pp_a fmt mp_acs =
  if Array.length mp_acs > 0 then
    fprintf fmt "%swith AC sets [ %a ]" sep
            (pp_list " ; " (pp_indexed_depthed_ac_set pp_a))
            (Array.to_list (Array.mapi (fun i dset -> (i,dset)) mp_acs))

let pp_matching_problem sep pp_a fmt mp =
  fprintf fmt "%a%a" (pp_mp_problems     pp_a) mp.problems
                     (pp_mp_ac_sets  sep pp_a) mp.ac_sets

let pp_int_matching_problem   sep = pp_matching_problem sep pp_pos
let pp_lterm_matching_problem sep = pp_matching_problem sep pp_lazy_term


(**     Problem convertion         **)
let convert_problems f g pb =
  let convert_problem = function
    | Eq (d,p)        -> Eq (d, f p)
    | AC_Subset (i,n) -> AC_Subset (i,n) in
  let convert_typed_p (ptype,p) = (ptype, convert_problem p) in
  let convert_prob = function
    | Solved t   -> Solved (f t)
    | Unsolved l -> Unsolved (List.map convert_typed_p l) in
  let convert_ac_set (d,(m,v,j,l)) = (d,(m,v,j,g l)) in
  {
    problems = Array.map convert_prob   pb.problems;
    ac_sets  = Array.map convert_ac_set pb.ac_sets;
  }


let update_dbs (depth:int) (dbs:int LList.t) (te:term) : term =
  let size = LList.len dbs in
  let arr = Array.make size None in
  (* XXX: could be computed once for all at compile time *)
  let _ = List.iteri ( fun i n -> arr.(n) <- Some (size-i-1) ) (LList.lst dbs) in
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (l,x,n) as t ->
        if n < k (* var bound in te *) then t
        else if n >= k+depth (* var free in te*) then
          mk_DB l x (n+size)
        else
          ( match arr.(n-k) with
            | None -> raise NotUnifiable
            | Some n' -> mk_DB dloc x (n'+k) )
    | Lam (l,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi  dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in
  aux 0 te

let solve (depth:int) (k_lst:int LList.t) (te:term) : term =
  List.fold_left (fun te _ -> mk_Lam dloc qmark None te)
                 (update_dbs depth k_lst te)
                 (LList.lst k_lst)

let unshift reduce (d,t) =
  if d = 0 then t
  else let te = Lazy.force t in
       Lazy.from_val (try Subst.unshift d te
                      with Subst.UnshiftExn -> Subst.unshift d (reduce te))

let force_solve_eq_problem reduce (d,t) = function
  | Syntactic -> unshift reduce (d,t)
  | MillerPattern l ->
     if LList.is_empty l then unshift reduce (d,t)
     else let t = Lazy.force t in
          let res = try solve d l t with NotUnifiable -> solve d l (reduce t) in
          Lazy.from_val (Subst.unshift d res)


(** Remove [n] occurences of term [t] from list [l] under [d] lambdas *)
let remove_n_occ reduce convertible t d n l =
  let rec aux acc n l =
    if n == 0 then Some (List.rev_append acc l) else
      match l with
      | [] -> None
      | hd :: tl ->
         if convertible (Lazy.force (unshift reduce (d,hd))) t
         then aux acc       (n-1) tl
         else aux (hd::acc) n     tl in
  aux [] n l


let try_set reduce convertible pb i t =
  match pb.problems.(i) with
  | Solved _ -> None
  | Unsolved pbs ->
     let update (ptype,p) = match p with
       | AC_Subset(i,n) ->
          let (d, (m, v, k, terms)) = pb.ac_sets.(i) in
          let l = (match ptype with Syntactic -> [] | MillerPattern l -> LList.lst l) in
          let t = mk_App2 (Lazy.force t) (List.map (fun k -> mk_DB dloc qmark k) l) in
          begin
            assert(k >= n);
            match remove_n_occ reduce convertible t d n terms with
            | Some nterms when k > n || List.length nterms == 0 ->
               pb.ac_sets.(i) <- (d, (m, v, k-n, nterms)); true
            | _ -> false
          end
       | Eq(d, ti) -> convertible (Lazy.force t) (Lazy.force ti)
     in
     let ok = List.for_all update pbs in
     if ok then ( pb.problems.(i) <- Solved t; Some pb) else None

let rec process f i pb =
  if i >= Array.length pb.problems then Some pb else
    match pb.problems.(i) with
    | Solved _ -> process f (i+1) pb
    | Unsolved pbs -> bind_opt (process f (i+1))
                               (f pb i pbs)

(** Solve all Eq problems *)
let process_eqs reduce convertible problem =
  let f pb ind =
    let rec aux = function
      | [] -> Some pb
      | (ptype, Eq e) :: tl ->
         try_set reduce convertible pb ind
                 (force_solve_eq_problem reduce e ptype)
      | hd :: tl -> aux tl in
    aux in
  process f 0 problem


let process_one_variable_sets reduce convertible to_comb problem =
  let f pb ind =
    let rec aux = function
      | [] -> Some pb
      | (ptype, AC_Subset(i,n)) :: tl ->
         let (d,(m,v,k,terms)) = pb.ac_sets.(i)  in
         if      n <  k then Some pb
         else if n == 1 && k == 1 then
           let t = force_solve_eq_problem reduce (d, to_comb m v terms) ptype in
           try_set reduce convertible pb ind t
         else aux tl
      | _ -> assert false in
    aux in
  process f 0 problem

let fetch_lowest_score_var pb =
  let score (ptype,p) = match p with
    | Eq _ -> assert false
    | AC_Subset(i,n) ->
       let (d,(m, v, k, terms)) = pb.ac_sets.(i) in
       (n, (List.length terms) - k + n) in
  let rec search_l m i = function
    | [] -> m
    | hd::tl ->
       let (maxv, maxs) = m in
       let s = score hd in
       let nm = (if s > maxs then ((i,hd), s) else m) in
       search_l nm i tl in
  let rec search m i =
  if i >= Array.length pb.problems then m else
    match pb.problems.(i) with
    | Solved _     -> search m                  (i+1)
    | Unsolved pbs -> search (search_l m i pbs) (i+1)  in
  let dummy = (-1, (Syntactic,AC_Subset(0,0)))  in
  let ((i,prob),s) = search (dummy, (-1,-1)) 0 in
  (i, prob)

let l_init l f =
  let rec aux i = if i == l then [] else (f i) :: aux (i+1) in aux 0

let init size n = l_init size (fun x -> x >= size - n)

let next (size, max, i, bag) =
  let rec aux = function
    | [] -> None
    | false :: true :: l -> Some (true :: false :: l)
    | false :: l -> map_opt (fun x -> false :: x) (aux l)
    | true  :: l -> map_opt append (aux l)
  and append = function
    | [] -> [true]
    | true :: l -> true :: true :: l
    | false :: l -> false :: (append l)
  in
  match aux bag with
  | Some nbag -> Some (size, max, i, nbag)
  | None -> if i >= max then None else Some (size, max, i+1, init size (i+1))

let first size max min = (size, max, min, init size min)

let get_subset (size,max,i,bag) l =
  let rec aux acc flag e = if flag then e::acc else acc in
  List.fold_left2 aux [] bag l


let is_solved = function
  | None -> true
  | Some pb -> array_for_all (function Solved _ | Unsolved [] -> true | _ -> false) pb.problems

let get_subst =
  map_opt (fun pb -> Array.map (function Solved t -> Some t | _ -> None) pb.problems)


let rec process_all reduce convertible to_comb pb =
  debug 1 "---------------------";
  debug 1 "%a" (pp_lterm_matching_problem "\n") pb;
  debug 1 "---------------------";
  let var, (ptype, p) = fetch_lowest_score_var pb in
  match p with
  | Eq _ -> assert false
  | AC_Subset(i,n) ->
     let d, (m,v,k,terms) = pb.ac_sets.(i) in
     assert(k >= n);
     let nb_terms = List.length terms in
     let rec search bag =
       let subset = get_subset bag terms in
       let (_,_,_,b) = bag in
       debug 1 "Permut: %i (%a)" (List.length subset) (pp_list "; " pp_print_bool) b;
       let t = force_solve_eq_problem reduce (d, to_comb m v subset) ptype in
       let npb = {
           problems = Array.copy pb.problems;
           ac_sets = Array.copy pb.ac_sets
         } in
       match try_set reduce convertible npb var t with
       | None -> (debug 1 "Fail"; bind_opt search (next bag))
       | Some npb -> if is_solved (Some npb)
                     then (debug 1 "Success !"; Some npb)
                     else (debug 1 "Found var %i !" var;
                           process_all reduce convertible to_comb npb)
     in
     if n == k && nb_terms mod n <> 0 then None
     else
       let size_min = if n == k then nb_terms / n else 1 in
       let size_max = (nb_terms + n - k) / n in
       debug 1 "try var %i from set %i: %i %i %i" var i nb_terms size_max size_min;
       search (first nb_terms size_max size_min)


let solve_problem reduce convertible to_comb pb =
  let to_comb m v terms = Lazy.from_val (to_comb dloc m v (List.map Lazy.force terms)) in
  let pb = process_eqs reduce convertible pb in
  if is_solved pb then get_subst pb else
  let pb = bind_opt (process_one_variable_sets reduce convertible to_comb) pb in
  if is_solved pb then get_subst pb else
    get_subst (bind_opt (process_all reduce convertible to_comb) pb)

