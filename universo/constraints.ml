(********** universes' variables ************)
       
let basename = "univ_variable"

let is_univ_variable v =
  let s = Basics.string_of_ident v in
  let n = String.length basename in
  String.length s > n && String.sub s 0 n = basename
          
(* 0 is reserved for a special purpose *)
let counter = ref 1

(* each univ variable is attached to a unique natural number *)
let var_of_name:(Basics.ident, int) Hashtbl.t = Hashtbl.create 83

(* get the number attached to the univ variable *)  
let uvar name =
  if Hashtbl.mem var_of_name name then begin
    Hashtbl.find var_of_name name end
  else
    let n = !counter in
    Hashtbl.add var_of_name name n;
    incr counter; n


(********** universes' constraints ************)
                    
type 'a constraints = 
| Eq of 'a * 'a 
| Lt of 'a * 'a

module ConstraintSet = Set.Make (struct type t = int constraints let compare = compare end)

(* a constraint is added by the typechecker when it can not unify two univ variables or two terms using univ variables. *)
                                
let constraints = ref ConstraintSet.empty
                    
let add_constraint_eq n n' = 
  let (v,v') = uvar n ,uvar n' in
  constraints := ConstraintSet.add (Eq(v,v')) !constraints

let add_constraint_lt n n' =
  let (v,v') = uvar n ,uvar n' in
  constraints := ConstraintSet.add (Lt(v,v')) !constraints
