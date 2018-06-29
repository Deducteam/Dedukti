(** Tools used for the matrices labeling edges in the call-graph studied by sizechange.ml *)

open Basic
open Term
open Rule

(** Representation of the set {-1, 0, ∞} *)
type cmp = Min1 | Zero | Infi

(** String representation. *)
let cmp_to_string : cmp -> string =
  function
  | Min1 -> "<"
  | Zero -> "="
  | Infi -> "?"

(** The pretty printer for the type [cmp] *)
let pp_cmp fmt c=
  Format.fprintf fmt "%s" (cmp_to_string c)

(** Addition operation (minimum) *)
let (<+>) : cmp -> cmp -> cmp = fun e1 e2 ->
  match (e1, e2) with
  | (Min1, _   ) | (_, Min1) -> Min1
  | (Zero, _   ) | (_, Zero) -> Zero
  | (Infi, Infi)             -> Infi

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
