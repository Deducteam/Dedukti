open Basics

let univ_m = hstring "univs"
let base_m = hstring "cic"
let univ_constructors =
  [hstring "succ"; hstring "max"; hstring "type"]
let sort = Term.mk_Const dloc (hstring "cic") (hstring "Sort")
                          
(********** universes' variables ************)
       
let basename = "univ_variable"

let c = ref 0

let fresh_uname () =
  let str = basename ^ (string_of_int !c) in
  incr c;
  let v = hstring str in
  ignore(Env.declare_constant dloc v sort);
  v


let is_univ_variable v =
  let s = string_of_ident v in
  let n = String.length basename in
  String.length s > n && String.sub s 0 n = basename
          
(* 0 is reserved for a special purpose *)
let counter = ref 1

(* each univ variable is attached to a unique natural number *)
let var_of_name = Hashtbl.create 83

(* get the number attached to the univ variable *)  
let uvar name =
  if Hashtbl.mem var_of_name name then begin
    Hashtbl.find var_of_name name end
  else
    let n = !counter in
    Hashtbl.add var_of_name name n;
    incr counter; n

(* Replace all terms of types Sort by a variable *)          
let rec elaboration term = Term.(
  let m_name = Env.get_name() in
  match term with
  (* the constant is an explicit universe *)
  | Const(loc,m,s) when Basics.ident_eq m univ_m -> 
     mk_Const loc m_name (fresh_uname())
  (* capture universe like "cic.type (cic.s cic.z)" *)
  | App(Const(loc,m,s), _, _) when Basics.ident_eq m base_m &&
   List.exists (fun x -> Basics.ident_eq s x) univ_constructors ->     
     mk_Const loc m_name (fresh_uname())
  | App(f, a, al) ->
    let f' = elaboration f in
    let a' = elaboration a in
    let al' = List.map elaboration al in
    mk_App f' a' al'
  | Lam(loc, id, t_opt, t) ->
     let t' = elaboration t in
     begin
       match t_opt with
       | None -> mk_Lam loc id t_opt t'
       | Some x -> let x' = elaboration x in
                   mk_Lam loc id (Some x') t'
     end
  | Pi(loc, id, ta, tb) ->
     let ta' = elaboration ta in
     let tb' = elaboration tb in     
     mk_Pi loc id ta' tb'
  | _ ->     term
                           )
                    
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

(********** graph generation ***************)

module Graph = Graphmp
                                   
let uff uf i = Unionfind.find uf i                                   
                                   
(* from the constraints, create a graph *)
let graph_of_constraints cs =
  (* indices start with 1 *)
  let uf = Unionfind.create (Hashtbl.length var_of_name + 1) in
  let rec aux c (uf,g) =
    match c with
    | Eq(i,j) -> (Unionfind.union uf i j,g) 
    | Lt(i,j) -> (uf,Graph.add_edge g (uff uf i) (uff uf j))
  in
  let init_graph = Hashtbl.fold (fun k v g -> Graph.add_vertex g (uff uf v)) 
    var_of_name Graph.empty in
  let (uf,g) = ConstraintSet.fold aux cs (uf,init_graph) in
  let roots = Graph.roots g in
  (* Printf.printf "%d\n" (List.length roots); *)
  (uf, List.fold_left (fun g n-> Graph.add_edge g 0 n) g roots)


(************* computing the shortest path and get the results ***********)

let sort_of_nat n = Term.(
  let ctype = mk_Const dloc (hstring "cic") (hstring "type") in
  let cp = mk_Const dloc (hstring "cic") (hstring "prop") in
  let cz = mk_Const dloc (hstring "cic") (hstring "z") in
  let cs = mk_Const dloc (hstring "cic") (hstring "s") in
  let rec aux n =
    match n with 
    | 0 -> assert false
    | 1 -> assert false
    | 2 -> mk_App cs cz []
    | _ -> mk_App cs (aux (n-1)) []
  in
  match n with
  | 0 -> cp
  | 1 -> mk_App ctype cz []
  | _ -> mk_App ctype (aux n) []
                    )
                        
(* associate to each univ variable the universe found *)
let env = Hashtbl.create 83 

type result = (Basics.ident,Term.term) Hashtbl.t
                         
(* return a Map that associate for each univ variable its universe *)
let solve() = 
  let (uf,g) = graph_of_constraints !constraints in
  let f = Graph.shortest_path g 0 in
  (* -1 because of the artifical node 0 *)
  let sort_of_var v = sort_of_nat  ((f (uff uf v)) -1) in
  Hashtbl.iter (fun k v -> Hashtbl.add k (sort_of_var v) env) var_of_name;
  env


(************** replace each univ variable by it's solution ***************)
    
(* replace it's univ variable by it's universe *)
let rec reconstruction env term = Term.(
  match term with
  | Const(loc,m,v) when is_univ_variable v ->
    Hashtbl.find env v
  | App(f, a, al) -> 
    let f' = reconstruction env f in
    let a' = reconstruction env a in
    let al' = List.map (reconstruction env) al in
    (mk_App f' a' al')
  | Lam(loc, id, t_opt, t) ->
    let t' = reconstruction env t in
    begin
      match t_opt with
      | None -> mk_Lam loc id t_opt t'
      | Some x -> let x' = reconstruction env x in mk_Lam loc id (Some x') t'
    end
  | Pi(loc, id, ta, tb) ->
    let ta' = reconstruction env ta in
    let tb' = reconstruction env tb in
    mk_Pi loc id ta' tb'
  | _ -> term
)
    
(*    
  (uf, Hashtbl.fold (fun k v n -> UnivMap.add k ((get_sol_of_node r (uff uf v)) -1) n) 
    Constraints.var_of_name NameMap.empty) 
 *)
                                   
                                   
(*
let _debug_print_entry s i = Printf.printf "Variable : %s and var : %d\n" (string_of_ident s) i
let _debug_print_entry2 k v = Printf.printf "Var : %d and value : %d\n" k v

let print_constraint c =
  let find_key v = 
    Hashtbl.fold (fun k v' c -> if v=v' then string_of_ident k else if c = "" then "" else c) var_of_name "" in
  match c with
  | Eq(i,j) -> Printf.printf "%s = %s\n" (find_key i)  (find_key j)
  | Lt(i,j) -> Printf.printf "%s < %s\n" (find_key i) (find_key j)

let print_constraints() = ConstraintSet.iter print_constraint !constraints
let print_var_of_name() = Hashtbl.iter (fun k v -> _debug_print_entry k v) var_of_name
let uff uf i = Unionfind.find uf i
 *)
               
