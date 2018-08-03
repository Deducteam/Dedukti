(** Tools used for the matrices labeling edges in the call-graph studied by sizechange.ml *)

open Basic
open Term
open Rule

exception NormalizationError
exception OverTruncation
       
let weights = ref 5
let depth   = ref 5
                  
(** The type [cmp] is the type of terms described in Hyvernat's paper (2013) *)
type cmp = Var | Constr of string*cmp | Destr of string*cmp | Tuple of int*cmp
           | Proj of int*cmp | Weight of int*cmp
                      
(** String representation. *)
let rec cmp_to_string : cmp -> string = function
  | Var         -> ""
  | Constr(s,c) -> s^(cmp_to_string c)
  | Destr(s,c)  -> s^"-"^(cmp_to_string c)
  | Tuple(i,c)  -> "T"^(string_of_int i)^(cmp_to_string c)
  | Proj(i,c)   -> "P"^(string_of_int i)^(cmp_to_string c)
  | Weight(i,c) -> "<"^(string_of_int i)^">"^(cmp_to_string c)

(** The pretty printer for the type [cmp] *)
let pp_cmp fmt c=
  Format.fprintf fmt "%s" (cmp_to_string c)

(** [normalize] ensures that all [cmp] terms are of the form constructors followed by weights followed by destructors *) 
let rec normalize : cmp -> cmp =
  let rec fetch_const_dest : cmp -> cmp = function
    | Destr(s1,Constr(s2,c)) ->
       begin
         if s1=s2
         then fetch_const_dest c
         else raise NormalizationError
       end
    | Destr(s1,Tuple(j,c))   -> raise NormalizationError
    | Destr(s1,Weight(j,c))  -> fetch_const_dest (Weight(j-1,c))
    | Proj(i,Constr(s2,c))   -> raise NormalizationError
    | Proj(i,Tuple(j,c))     ->
       begin
         if i=j
         then fetch_const_dest c
         else raise NormalizationError
       end
    | Proj(i,Weight(j,c))    -> fetch_const_dest (Weight(j-1,c))
    | Weight(i,Constr(s2,c)) -> fetch_const_dest (Weight(i+1,c))
    | Weight(i,Tuple(j,c))   -> fetch_const_dest (Weight(i+1,c))
    | Weight(i,Weight(j,c))  -> fetch_const_dest (Weight(i+j,c))
    | t -> t
  in function
  | Var          -> Var
  | Constr(s1,c) -> Constr(s1,normalize c)
  | Destr(s1,c)  -> fetch_const_dest (Destr(s1,normalize c))
  | Tuple(i,c)   -> Tuple(i,normalize c)
  | Proj(i,c)    -> fetch_const_dest (Proj(i,normalize c))
  | Weight(i,c)  -> fetch_const_dest (Weight(i,normalize c))

(** [approximates ensures that the depth is smaller than the global reference [depth]. It assumes that is argument has been normalized before. *)
let approximate : cmp -> cmp =
  (** [size_dest] counts the number of destructors in the right part of the argument *)
  let size_dest : cmp -> int =
    let rec size_dest_bis : int -> cmp -> int = fun acc ->
      function
      | Var         -> acc
      | Destr(_,c)  -> size_dest_bis (acc+1) c
      | Proj(_,c)   -> size_dest_bis (acc+1) c
      | Weight(_,c) -> size_dest_bis acc c
      | _           -> assert false (* The [cmp] is supposed to be in normal form *)
    in fun c -> size_dest_bis 0 c
  in
  (** [approx_dest c i] cut the [i] first destructors of [c] *)
  let rec approx_dest : int -> cmp -> cmp = fun acc c ->
    if acc<=0
    then c
    else
      match c with
      | Weight(i,d) -> Weight(i,approx_dest acc d)
      | Destr(_,d)  -> Weight(-1,approx_dest (acc-1) d)
      | Proj(_,d)   -> Weight(-1,approx_dest (acc-1) d)
      | _           -> assert false (* The [cmp] is supposed to be in normal form and [c] its right part *)
  in
  let rec approx_constr : int -> cmp -> cmp = fun acc c ->
    if acc=0
    then
      let norm=normalize (Weight(0,c))
      in normalize (approx_dest (!depth - (size_dest norm)) (norm))
    else
      match c with
      | Constr(s1,d) -> Constr(s1,approx_constr (acc-1) d)
      | Tuple (i,d)  -> Tuple (i,approx_constr (acc-1) d)
      | c            -> approx_constr 0 c
  in fun c -> approx_constr !depth c

(** [truncate] ensures that the weights between constructors and destructors are between -(!weights) and (!weights)-1. It assume that its argument has been normalized before. *)
let rec truncate : cmp -> cmp = function
  | Weight(i,c) ->
     begin
       if i< -(!weights)
       then Weight(-(!weights),c)
       else if i>= !weights
       then raise OverTruncation
       else Weight(i,c)
     end
  | Constr(s,c) -> Constr(s,truncate c)
  | Tuple(i,c)  -> Tuple(i,truncate c)
  | c           -> c
                            
let usable_form : cmp -> cmp =
  fun c -> truncate (approximate (normalize c))

(** [paste c1 c2] return the [cmp] where [c2] is nested in [c1] instead of the [Var] *)
let rec paste : cmp -> cmp -> cmp = fun c1 c2 ->
  match c1 with
  | Var         -> c2
  | Constr(s,c) -> Constr(s,paste c c2)
  | Destr(s,c)  -> Destr(s,paste c c2)
  | Tuple(i,c)  -> Tuple(i,paste c c2)
  | Proj(i,c)   -> Proj(i,paste c c2)
  | Weight(i,c) -> Weight(i,paste c c2)
                    
(** Addition operation (minimum) *)
let (<+>) : cmp list -> cmp list -> cmp list = fun l1 l2 ->
  
                                                    

(** Multiplication operation. *)
let (<*>) : cmp -> cmp -> cmp = fun e1 e2 ->
  match (e1, e2) with
  | (Infi, _   ) | (_, Infi) -> Infi
  | (Min1, _   ) | (_, Min1) -> Min1
  | (Zero, Zero)             -> Zero

(** Reduce by 1 a cmp *)
let minus1 : cmp -> cmp =
  function
  | Zero -> Min1
  | n -> n

(** Compute the minimum of a list *)
let mini : cmp list -> cmp = fun l ->
  List.fold_left (<+>) Infi l

(** Type of a size change matrix. *)
type matrix =
  { w   : int             ; (* Number of argument of callee *)
    h   : int             ; (* Number of argument of caller *)
    tab : cmp array array   (* The matrix of size h*w *)
  }

(** The pretty printer for the type [matrix] *)
let pp_matrix fmt m=
  Format.fprintf fmt "w=%i, h=%i@.tab=[[%a]]" m.w m.h
    (pp_arr "]@.     [" (pp_arr "," pp_cmp)) m.tab
    
(** Matrix product. *)
let prod : matrix -> matrix -> matrix =
  fun m1 m2 ->
    assert (m1.w = m2.h); (* The matrix must be compatible to perform product *)
    let tab =
      Array.init m1.h
        (fun y ->
	   Array.init m2.w
             (fun x ->
                let r = ref Infi in
                for k = 0 to m1.w - 1 do
	          r := !r <+> (m1.tab.(y).(k) <*> m2.tab.(k).(x))
                done; !r
             )
        )
    in
    { w = m2.w ; h = m1.h ; tab }
      
(** Check if a matrix corresponds to a decreasing idempotent call. *)
let decreasing : matrix -> bool = fun m ->
  assert (m.w = m.h); (* Only square matrices labeling a self-looping arrow need to be study *)
  try
    for k = 0 to m.w - 1 do
      if m.tab.(k).(k) = Min1
      then raise Exit
    done;
    false
  with Exit -> true

(** Check if a matrix subsumes another one (i.e. gives more infomation). *)
let subsumes : matrix -> matrix -> bool = fun m1 m2 ->
  try
    Array.iteri
      (fun y l ->
         Array.iteri
           (fun x e ->
              if not (e >= m2.tab.(y).(x))
              then raise Exit
           ) l
      ) m1.tab;
    true
  with Exit -> false


(** Compare a term and a pattern, using an int indicating under how many lambdas the comparison occurs *)
let rec comparison :  int -> term -> pattern -> cmp =
  fun nb t p ->
    let rec comp_list : cmp -> pattern list -> term list -> cmp =
      fun cur lp lt ->
        match lp,lt with
          | [], _ | _, [] -> cur
          | a::l1, b::l2  ->
            begin
              match (comparison nb b a), cur with
	      | _   , Infi -> assert false
       (* We are sure, that the current state [cur] cannot contain a Infi, else the Infi would be the result of the function and no recursive call would be needed *)
              | Infi, _    -> Infi
              | Min1, _    -> comp_list Min1 l1 l2
      	      | _   , Min1 -> comp_list Min1 l1 l2
      	      | Zero, Zero -> comp_list Zero l1 l2
      	    end
    in
    match p,t with
    | Var (_,_,n,_), DB (_,_,m) -> if n+nb=m then Zero else Infi (* Two distinct variables are uncomparable *)
    | Var (_,_,n,_), App(DB(_,_,m),_,_) -> if n+nb=m then Zero else Infi (* A variable when applied has the same size as if it was not applied *)
    | Lambda(_,_,Var(_,_,n,_)), DB(_,_,m) -> if n+nb=m+1 then Zero else Infi
    | Lambda(_,_,Var(_,_,n,_)), App(DB(_,_,m),_,_) -> if n+nb=m+1 then Zero else Infi
    | Pattern (_,f,lp), App(Const(_,g),t1,lt) when (name_eq f g) ->
       begin
	 comp_list Zero lp (t1::lt)
       end
    | Pattern (_,_,l),t -> minus1 (mini (List.map (comparison nb t) l))
    | Lambda(_,_,pp),Lam(_,_,_,tt) -> comparison nb tt pp
    | _ -> Infi

(** [matrix_of_lists m lp n lt n] compare each term of a list [lt] with a list of pattern [lp] considering that we are under [nb] lambdas and add some Infi to respect the arities of the caller and called functions *)
let matrix_of_lists : int -> pattern list -> int -> term list -> int -> matrix =
  fun m lp n lt nb ->
    let mm= min m (List.length lp) in
    let nn= min n (List.length lt) in
    let tab =Array.make_matrix m n Infi in
    for i=0 to mm-1 do
      let p=List.nth lp i in
      for j=0 to nn-1 do
	let t=List.nth lt j in
	tab.(i).(j) <- comparison nb t p
      done;
    done;
    {h=m ; w=n ; tab}

	
(** Replace the right hand side of a rule with the term chosen *)
let term2rule : rule_infos -> term -> rule_infos = fun r t ->
  {l=r.l;
   name=r.name;
   cst=r.cst;
   args=r.args;
   rhs=t;
   esize=r.esize;
   pats=r.pats;
   constraints=[]}
