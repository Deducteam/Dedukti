open Basic
open Format
open Term
open Rule

exception NotUnifiable

(** ([n], [t]) represents the term represented by [t] under [n] lambda abstractions. *)
type 'a depthed = int * 'a

type dpos  = int depthed
type dterm = (term lazy_t) depthed

type problem_type =
  | Syntactic
  | MillerPattern of int LList.t

let mk_problem_type = function
  | [] -> Syntactic
  | l -> MillerPattern (LList.of_list l)

type 'a problem_relation =
  | Eq        of 'a depthed
  | AC_Subset of int * int

type 'a ac_set  = 'a list depthed * ident * ident * int

type 'a problem =
  | Solved of 'a
  | Unsolved of (problem_type * 'a problem_relation) list

type 'a matching_problem =
  {
    problems  : 'a problem array;
    ac_sets   : 'a ac_set array;
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

let pp_indexed_ac_set pp_a fmt (i,(l,m,v,nvars)) =
  fprintf fmt "%i : %i vars in %a.%a{%a}" i nvars pp_ident m pp_ident v
          (pp_depthed (pp_list "; " pp_a)) l

let pp_mp_problems pp_a fmt mp_p =
  fprintf fmt "[ %a ]" (pp_list " and " (pp_indexed_prob pp_a))
          (Array.to_list (Array.mapi (fun i x -> (i,x)) mp_p))

let pp_mp_ac_sets sep pp_a fmt mp_acs =
  if Array.length mp_acs > 0 then
    fprintf fmt "%swith AC sets [ %a ]" sep
            (pp_list " ; " (pp_indexed_ac_set pp_a))
            (Array.to_list (Array.mapi (fun i x -> (i,x)) mp_acs))

let pp_mp_subst sep pp_a fmt arr =
  if Array.fold_left (fun a b -> a || b <> None) false arr then
    let pp_aux fmt = function None -> fprintf fmt "_"
                            | Some a -> fprintf fmt "%a" pp_a a in
    fprintf fmt "%ssubst = [%a]" sep (pp_list " " pp_aux) (Array.to_list arr)

let pp_matching_problem sep pp_a fmt mp =
  fprintf fmt "%a%a"
          (pp_mp_problems     pp_a) mp.problems
          (pp_mp_ac_sets  sep pp_a) mp.ac_sets

let pp_int_matching_problem   sep = pp_matching_problem sep pp_pos
let pp_lterm_matching_problem sep = pp_matching_problem sep pp_lazy_term


(**     Problem convertion         **)



let convert_problems f g pb =
  let convert_ac_set ((d,l),m,v,j) = ((d,g l),m,v,j) in
  let convert_problem = function
    | Eq (d,p)        -> Eq (d, f p)
    | AC_Subset (i,n) -> AC_Subset (i,n) in
  let convert_typed_p (ptype,p) = (ptype, convert_problem p) in
  let convert_prob = function
    | Solved t   -> Solved (f t)
    | Unsolved l -> Unsolved (List.map convert_typed_p l) in
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

(** Updates the substitution with simple Eq problems *)
let process_eqs reduce pb =
  let comp = ref [] in
  let try_set pb i t =
    match pb.subst.(i) with
    | None    -> pb.subst.(i) <- Some t
    | Some ti -> comp := (ti,t) :: !comp in
  let rec aux i = function
    | [] -> []
    | (ptype, Eq e) :: tl ->
       try_set pb i (force_solve_eq_problem reduce e ptype);
       aux i tl
    | hd :: tl -> hd :: (aux i tl) in
  Array.iteri (fun i l -> pb.problems.(i) <- aux i l) pb.problems;
  !comp


let rec remove_n_occ convertible t n l =
  if n == 0 then Some l else
    match l with
    | [] -> None
    | hd :: tl ->
       if convertible hd t
       then remove_n_occ convertible t (n-1) tl
       else remove_n_occ convertible t n     tl


let try_set convertible pb i t =
  let ok = match pb.subst.(i) with
    | None    -> pb.subst.(i) <- Some t; true
    | Some ti -> convertible ti t in
  if not ok then None else
  let update (ptype,p) = match p with
    | AC_Subset(i,n) ->
       let l = (match ptype with Syntactic -> [] | MillerPattern l -> LList.lst l) in
       let t = mk_App2 t (List.map (fun k -> mk_DB dloc qmark k) l) in
       let ( (d,terms), m, v, k) = pb.ac_sets.(i) in
       begin
         assert(k >= n);
         match remove_n_occ convertible t n terms with
         | Some nterms -> pb.ac_sets.(i) <- ( (d,nterms), m, v, k-n); true
         | None -> false
       end
    | _ -> assert false
  in
  let ok = List.for_all update pb.problems.(i) in
  if not ok then None
  else (pb.problems.(i) <- []; Some pb)


(** Update problems using current substitution. *)
let update_problems convertible pb i t =
  Array.iteri
    (fun i a ->
      match pb.subst.(i) with
      | None -> ()
      | Some t ->
         begin
           
         end)
    pb.problems;
  pb

let process_one_variable_sets reduce convertible to_comb check pb =
  let rec aux = function
    | [] -> []
    | (ptype, AC_Subset(i,n)) as hd :: tl ->
       let ( (d,terms), m, v, k) = pb.ac_sets.(i)  in
       if      n >  k then hd :: (aux tl)
       else if n == k then
         let t = (d, to_comb dloc m v terms) in
         begin
           pb.subst.(i) <- Some (force_solve_eq_problem reduce t ptype);
           aux tl
         end
       else assert false
    | _ -> assert false in
  Array.iteri f pb.ac_sets;


let fetch_lowest_score pb =
  let g a (ptype,p) = match p with
    | Eq _ -> assert false
    | AC_Subset(i,n) ->
       let ( (d,terms), m, v, k) = pb.ac_sets.(i) in
       (assert(k >= n); min a (1 + (List.length terms) * (k-n))) in
  let f l = List.fold_left g max_int l in
  let mini = ref 0 in
  let minv = ref max_int in
  Array.iteri
    (fun i a ->
      let v = f a in
      if v < !minv then (mini := i; minv := v) )
    pb.problems;
  !mini



let solve_problem reduce convertible to_comb pb =
  let to_comb l m v terms = Lazy.from_val (to_comb l m v (List.map Lazy.force terms)) in
  let check (a,b) = not (convertible (Lazy.force a) (Lazy.force b)) in
  let check_l l = List.exists check l in
  let cstr = process_eqs reduce pb in
  if check_l cstr then None else
  let cstr = process_one_variable_sets reduce to_comb check pb in
  if check_l cstr then None else
  let i = fetch_lowest_score pb in
  Some pb.subst


let generator l =
  
