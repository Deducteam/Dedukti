(** Size change principle. This module implements a decision procedure based
    on the work of Chin Soon Lee, Neil D. Jones and Amir M. Ben-Amram (POPL
    2001). It is used by PML to check that typing and subtyping proofs are
    well-founded. Most of this module comes from an implementation of Rodolphe 
    Lepigre and Christophe Raffalli. *)

open Basic
open Term
open Rule
open Format
    
(** Representation of the set {-1, 0, ∞} *)
type cmp = Min1 | Zero | Infi

(** String representation. *)
let cmp_to_string : cmp -> string =
  function Min1 -> "<" | Zero -> "=" | Infi -> "?"

(** Addition operation (minimum) *)
let (<+>) : cmp -> cmp -> cmp = min

(** Multiplication operation. *)
let (<*>) : cmp -> cmp -> cmp = fun e1 e2 ->
  match (e1, e2) with
  | (Infi, _   ) | (_   , Infi) -> Infi
  | (Min1, _   ) | (_   , Min1) -> Min1
  | (Zero, Zero) -> Zero

(** Type of a size change matrix. *)
type matrix =
  { w   : int
  ; h   : int
  ; tab : cmp array array }

(** Matrix product. *)
let prod : matrix -> matrix -> matrix = fun m1 m2 ->
  assert (m1.w = m2.h);
  let tab =
    Array.init m1.h (fun y ->
      Array.init m2.w (fun x ->
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
  assert (m.w = m.h);
  try
    for k = 0 to m.w - 1 do
      if m.tab.(k).(k) = Min1 then raise Exit
    done; false
  with Exit -> true

(** Check if a matrix subsumes another one (i.e. gives more infomation). *)
let subsumes : matrix -> matrix -> bool = fun m1 m2 ->
  try
    Array.iteri (fun y l ->
      Array.iteri (fun x e ->
        if not (e >= m2.tab.(y).(x)) then raise Exit
      ) l
    ) m1.tab; true
  with Exit -> false

(** Index of a function symbol. *)
type index = int

(** Conversion to int. *)
let int_of_index : index -> int = fun i -> i

(** Index of the root. *)
let root : index = -1

(** Map with indices as keys. *)
module IMap =
  struct
    include Map.Make(
      struct
        type t = index
        let compare = compare
      end)

    (** [find k m] will not raise [Not_found] because it will always be used
        when we are sure that the given key [k] is mapped in [m]. *)
    let find : key -> 'a t -> 'a =
      fun k m -> try find k m with Not_found -> assert false
  end

(** A call [{callee; caller; matrix; is_rec}] represents a call to the
    function symbol with key [callee] by the function symbole with the
    key [caller]. The [matrix] gives the relation between the parameters
    of the caller and the callee. The coefficient [matrix.(a).(b)] give
    the relation between the [a]-th parameter of the caller and the
    [b]-th argument of the callee. The boolean [is_rec] is true when the
    call is a reccursive call (i.e. a call to a generalised hypothesis
    lower in the tree. It is [false] for every call to subtyping in the
    typing algorithm and the same goes for rules introducing a new
    induction hypothesis. Every other call refers to a previously
    introduced induction hypothesis and its boolean is [true]. *)
type call =
  { callee : index  (** Key of the function symbol being called. *)
  ; caller : index  (** Key of the calling function symbol. *)
  ; matrix : matrix (** Size change matrix of the call. *)
  ; is_rec : bool   (** Indicates if this is a recursive call. *) }

(** Representation of a function symbol. *)
type symbol =
  { index : index        (** Index for use in a [call]. *)
  ; name  : string       (** Name of the symbol. *)
  ; arity : int          (** Arity of the symbol (number of args). *)
  ; args  : string array (** Name of the arguments. *) }

(** Internal state of the SCT, including the representation of symbols and
    the call graph. *)
type call_graph =
  { next_index : index ref
  ; symbols    : symbol IMap.t ref
  ; calls      : call list ref }

(** Creation of a new initial [call_graph]. It contains the initial root
    symbol. *)
let create : unit ->  call_graph ref =
  let root = { index = -1 ; name  = "R" ; arity = 0 ; args  = [||] } in
  let syms = IMap.singleton (-1) root in
  fun () -> ref { next_index = ref 0 ; symbols = ref syms ; calls = ref [] }


let initialize = create ()
let table = ref []
let nom_module s= ref (string_of_ident s)
  

(** Creation of a new symbol. The return value is the key of the created
    symbol, to be used in calls. *)
let create_symbol : bool -> string -> string array -> unit =
  let g= !initialize in
  fun vb name args ->
    let arity = Array.length args in
    let index = !(g.next_index) in
    let sym = {index ; name ; arity ; args} in
    table:=(name,index)::!table;
    if vb then (let rec print_list = function
      | [] -> ()
      | (s,n)::l -> printf "%s,%d " s (int_of_index n); print_list l
    in printf "La table vaut :"; print_list !table;printf "@.");
    incr g.next_index;
    g.symbols := IMap.add index sym !(g.symbols)

let rec find_arity=function
  | Pi(_,_,_,t) -> 1+find_arity t
  | _ -> 0

let add_const vb v t=
  let n=find_arity t in
  match t with
  | Pi(_,name,arg,res) -> begin
    if vb then (printf "Nouvelle fonction : ";
    printf "%s %a@." (string_of_ident v) pp_term t);
    create_symbol vb (string_of_ident v) (Array.make n "x")
  end
  | _ -> ()

(** Copy a call graph. *)
let copy : call_graph -> call_graph =
  fun g ->
    { next_index = ref !(g.next_index)
    ; symbols    = ref !(g.symbols)
    ; calls      = ref !(g.calls) }

(** Test if the call graph is empty. *)
let is_empty : call_graph -> bool =
  fun g -> !(g.calls) = []

(** Add a new call to the call graph. *)
let add_call : call option-> unit =
  let g= !initialize in
  fun c -> match c with
  | Some cc -> g.calls := cc :: !(g.calls)
  | None -> ()

exception Manquant


let comparaison p t =
  match p,t with
  | Var (_,_,n,[]),DB (_,_,m) -> printf "CAS COOL !!@.Pattern : %a@.Terme : %a@." pp_pattern p pp_term t; if n=m then Zero else Infi
  | Pattern (_,_,_,Var(_,_,n,[])::[]),DB(_,_,m) -> printf "CAS COOL !!@.Pattern : %a@.Terme : %a@." pp_pattern p pp_term t; if n=m then Min1 else Infi
  | _ -> printf "Pattern : %a@.Terme : %a@." pp_pattern p pp_term t; Infi
  
let matrice l1 l2=
  let n=List.length l1 in
  let m=List.length l2 in
  let res =ref [] in
  for i=0 to m-1 do
    let p=List.nth l2 i in
    let loc_res =ref [] in
    for j=0 to n-1 do
      let t=List.nth l1 j in
      loc_res:=(comparaison p t)::!loc_res
    done;
    res:=(Array.of_list (List.rev !loc_res))::!res
  done;
  {w=m ; h=n ; tab = Array.of_list (List.rev !res)}

let rule_to_call r =
  let symbs= !(!initialize.symbols) in
  let get_caller= match r.pat with 
    | Pattern (_,_,v,lp) -> Some (lp,(snd (List.find (fun (x,_) -> x=(string_of_ident v)) !table)))
    | p -> printf "ProblèmeCaller avec %a@." pp_pattern p ; None
  in
  let get_callee= match r.rhs with
    | Const (_,_,_) | DB (_,_,_) -> None
    | App (Const(_,_,v),t1,lt) -> Some (t1::lt,(snd (List.find (fun (x,_) -> x=(string_of_ident v)) !table)))
    | p -> printf "ProblèmeAppelee avec %a@." pp_term p ; None
  in
  try match get_callee,get_caller with
  | None, _ -> raise Manquant
  | _, None -> raise Manquant
  | Some (l1,a), Some (l2,b) ->  Some {callee=a ; caller = b ; matrix=matrice l1 l2 ; is_rec = false}
  with Manquant -> None
  
let add_rules vb l =
  let g = fun r -> r.pat in  
  if vb then printf "Règles : @.";
  let f _ = () in
  if vb then f (List.map (printf "%a@." pp_pattern) (List.map g l));
  f (List.map add_call (List.map rule_to_call l)); 
  if vb then printf "@."

let latex_print_calls () =
  let ff= std_formatter in
  let tbl = !initialize in
  let arities = IMap.bindings !(tbl.symbols) in
  let calls = !(tbl.calls) in
  fprintf ff "digraph G {\n";
  let arities = List.filter (fun (j,_) ->
    List.exists (fun c -> j = c.caller || j = c.callee) calls)
    (List.rev arities)
  in
  let numbering = List.mapi (fun i (j,_) -> (j,i)) arities in
  let index j = List.assoc j numbering in
  let not_unique name =
    List.fold_left (fun acc (_,sym) -> if sym.name = name then acc+1 else acc) 0 arities
                   >= 2
  in
  let f (j,sym) =
    if not_unique sym.name then
      fprintf ff "    N%d [ label = \"%s_{%d}\" ];\n" (index j) sym.name (index j)
    else
      fprintf ff "    N%d [ label = \"%s\" ];\n" (index j) sym.name
  in
  List.iter f (List.filter (fun (i,_) ->
    List.exists (fun c -> i = c.caller || i = c.callee) calls) arities);
  let print_call arities {callee = i; caller = j; matrix = m} =
    let {name = namej; arity = aj; args = prj} =
      try List.assoc j arities with Not_found -> assert false
    in
    let {name = namei; arity = ai; args = pri} =
      try List.assoc i arities with Not_found -> assert false
    in
    fprintf ff "    N%d -> N%d [label = \"\\\\left(\\\\begin{smallmatrix}"
      (index j) (index i);
    for i = 0 to ai - 1 do
      if i > 0 then fprintf ff "\\\\cr";
      for j = 0 to aj - 1 do
        if j > 0 then fprintf ff "&";
        let c =
          match m.tab.(j).(i) with
          | Infi -> "\\\\infty"
          | Zero -> "0"
          | Min1 -> "-1"
        in
        fprintf ff "%s" c
      done;
    done;
    fprintf ff "\\\\end{smallmatrix}\\\\right)\"]\n%!"
  in
  List.iter (print_call arities) calls;
  fprintf ff "  }\n"
