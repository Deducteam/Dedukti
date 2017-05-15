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
  printf "%n %n,%n %n@." m1.h m1.w m2.h m2.w;
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
let create : unit -> call_graph ref =
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

let cree_array n=
  let res=Array.make n "" in
  for i =0 to n-1 do
    res.(i)<-"x"^(string_of_int i)
  done;
  res
     
let add_fonc vb v t=
  let n=find_arity t in
  match t with
  | Kind -> ()
  | Type _ -> ()
  | DB (_,_,_) -> ()
  | Const (_,_,_) -> ()
  | App (_,_,_) -> ()
  | Lam (_,_,_,_) -> printf "AddFonc d'un lambda % a, je ne sais pas quoi faire @." pp_term t
  | Pi(_,name,arg,res) -> create_symbol vb (string_of_ident v) (cree_array n)


let add_symb vb v q=
  match q with
  | Kind -> printf "AddSymb Kind, je ne sais pas que faire @."
  | Type _ -> ()
  | DB (_,_,_) -> printf "AddSymb variable %a, je ne sais pas que faire @." pp_term q
  | Const (_,_,_) -> printf "AddSymb constante %a, je ne sais pas que faire @." pp_term q
  | App (_,_,_) -> ()
  | Lam (_,_,_,_) -> printf "Addsymb lambda % a, je ne sais pas quoi faire @." pp_term q
  | Pi(_,name,arg,res) -> printf "AddSymb pi %a, je ne sais pas quoi faire @." pp_term q
           
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
let add_call : call-> unit =
  let g= !initialize in
  fun cc -> g.calls := cc :: !(g.calls)

let rec comparaison t p=
  let moins1 =function
    | Zero -> Min1
    | n -> n
  in
  let rec mini cur=function
    | []-> cur
    | Min1::l -> Min1
    | Zero::l -> mini Zero l
    | Infi::l -> mini cur l
  in
  let rec comp_list cur lp lt=
    assert (List.length lp=List.length lt);
    match lp,lt with
    | [],[] -> cur
    | a::l1,b::l2 -> begin
      match comparaison b a,cur with
      | Infi, _ -> Infi
      | Min1,_ -> comp_list Min1 l1 l2
      | _, Min1 -> comp_list Min1 l1 l2
      | Zero,Zero -> comp_list Zero l1 l2
    end
  in
  match p,t with
  | Var (_,_,n,[]),DB (_,_,m) -> if n=m then Zero else Infi
  | Pattern (_,_,f,lp), App(Const(_,_,g),t1,lt) when (ident_eq f g) ->  comp_list Zero lp (t1::lt)
  | Pattern (_,_,_,l),t -> moins1 (mini Infi (List.map (comparaison t) l))
  | _ -> Infi
  
let matrice l1 l2=
  let n=List.length l1 in
  let m=List.length l2 in
  let res =ref [] in
  for i=0 to m-1 do
    let p=List.nth l2 i in
    let loc_res =ref [] in
    for j=0 to n-1 do
      let t=List.nth l1 j in
      loc_res:=(comparaison t p)::!loc_res
    done;
    res:=(Array.of_list (List.rev !loc_res))::!res
  done;
  {h=m ; w=n ; tab = Array.of_list (List.rev !res)}

let rec rule_to_call r =
  let get_caller= match r.pat with 
    | Pattern (_,_,v,lp) -> begin
                            try Some (lp,(snd (List.find (fun (x,_) -> x=(string_of_ident v)) !table)))
                            with Not_found -> None
                           end
    | p -> printf "ProblèmeCaller avec %a@." pp_pattern p ; None
  in
  let get_callee= match r.rhs with
    | Const (_,_,_) | DB (_,_,_) -> None
    | App (Const(_,_,v),t1,lt) -> Some (t1::lt,(snd (List.find (fun (x,_) -> x=(string_of_ident v)) !table)))
    | App (_,_,_) as p -> printf "ProblèmeAppeleeApp avec %a@." pp_term p ; None
    | Lam (_,_,_,_) as p -> printf "ProblèmeAppeleeLam avec %a@." pp_term p ; None
    | Pi (_,_,_,_) as p -> () ; None
    | p -> printf "ProblèmeAppelee avec %a@." pp_term p ; None
  in
  let term2rule t= {pat= r.pat ; rhs = t ; ctx= r.ctx ; name=r.name} in
  match get_callee,get_caller with
  | None, _ -> []
  | _, None -> []
  | Some (l1,a), Some (l2,b) ->  if List.length l1 <> (IMap.find a !(!initialize.symbols)).arity then failwith ("Appel partiel "^(fst (List.find (fun (_,x) -> x=a) !table))^" vers "^(fst (List.find (fun (_,x) -> x=b) !table))) else {callee=a ; caller = b ; matrix=matrice l1 l2 ; is_rec = false}::List.flatten (List.map rule_to_call (List.map term2rule  l1))
  
let add_rules vb l =
  let f _ = () in
  f (List.map add_call (List.flatten (List.map rule_to_call l)))

let print_array pp sep out v=Pp.print_list sep pp out (Array.to_list v)
                       
let print_call : formatter -> symbol IMap.t -> call -> unit = fun ff tbl c ->
  let caller_sym = IMap.find c.caller tbl in
  let callee_sym = IMap.find c.callee tbl in
  let print_args = print_array pp_print_string "," in
  fprintf ff "%s%d(%a%!) <- %s%d%!(" caller_sym.name c.caller
          print_args caller_sym.args callee_sym.name c.callee;
  let jj=Array.length c.matrix.tab in
  if jj>0 then
    let ii=Array.length c.matrix.tab.(0) in
    for i = 0 to ii - 1 do
      if i > 0 then fprintf ff ",";
      let some = ref false in
      for j = 0 to jj - 1 do
        let c = c.matrix.tab.(j).(i) in
        if c <> Infi then
          begin
            let sep = if !some then " " else "" in
            fprintf ff "%s%s%s" sep (cmp_to_string c) caller_sym.args.(j);
            some := true
          end
      done
    done;
  fprintf ff ")%!"
    
let latex_print_calls () =
  let ff= std_formatter in
  let tbl = !initialize in
  let arities = IMap.bindings !(tbl.symbols) in
  let calls = !(tbl.calls) in
  fprintf ff "\\begin{dot2tex}[dot,options=-tmath]\n  digraph G {\n";
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
  let print_call2 arities {callee = i; caller = j; matrix = m} =
    let {name = namej; arity = aj; args = prj} =
      try List.assoc j arities with Not_found -> assert false
    in
    let {name = namei; arity = ai; args = pri} =
      try List.assoc i arities with Not_found -> assert false
    in
    fprintf ff "    N%d -> N%d [label = \"\\\\left(\\\\begin{smallmatrix}"
      (index j) (index i);
    for i = 0 to ai - 1 do
      if i > 0 then fprintf ff "\\\\\\\\ ";
      for j = 0 to aj - 1 do
        if j > 0 then fprintf ff " & ";
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
  List.iter (print_call2 arities) calls;
  fprintf ff "  }\n\\end{dot2tex}\n"
    
    (** the main function, checking if calls are well-founded *)
let sct_only ()=
  let ftbl= !initialize in
  let num_fun = !(ftbl.next_index) in
  let arities = !(ftbl.symbols) in
  let tbl = Array.init num_fun (fun _ -> Array.make num_fun []) in
  let print_call ff= print_call ff arities in 
  (* counters to count added and composed edges *)
  let added = ref 0 and composed = ref 0 in
  (* function adding an edge, return a boolean indicating
     if the edge is new or not *)
  let add_edge i j m =
    (* test idempotent edges as soon as they are discovered *)
    if i = j && prod m m = m && not (decreasing m) then
      begin
        printf "edge %a idempotent and looping\n%!" print_call
           {callee = i; caller = j; matrix = m; is_rec = true};
        raise Exit
      end;
    let ti = tbl.(i) in
    let ms = ti.(j) in
    if List.exists (fun m' -> subsumes m' m) ms then
      false
    else (
      let ms = m :: List.filter (fun m' -> not (subsumes m m')) ms in
      ti.(j) <- ms;
      true)
  in
  (* adding initial edges *)
  try
    printf "initial edges to be added:\n%!";
    List.iter (fun c -> printf "\t%a\n%!" print_call c) !(ftbl.calls);
    let new_edges = ref !(ftbl.calls) in
    (* compute the transitive closure of the call graph *)
    printf "start completion\n%!";
    let rec fn () =
      match !new_edges with
      | [] -> ()
      | {callee = i; caller = j}::l when j < 0 -> new_edges := l; fn () (* ignore root *)
      | ({callee = i; caller = j; matrix = m} as c)::l ->
        assert (i >= 0);
        new_edges := l;
        if add_edge i j m then begin
          printf "\tedge %a added\n%!" print_call c;
          incr added;
          let t' = tbl.(j) in
          Array.iteri (fun k t -> List.iter (fun m' ->
            let c' = {callee = j; caller = k; matrix = m'; is_rec = true} in
            printf "\tcompose: %a * %a = %!" print_call c print_call c';
            let m'' = prod m' m in
            incr composed;
            let c'' = {callee = i; caller = k; matrix = m''; is_rec = true} in
            new_edges := c'' :: !new_edges;
          printf "%a\n%!" print_call c'';
          ) t) t'
        end else
        printf "\tedge %a is old\n%!" print_call c;
        fn ()
    in
    fn ();
    printf "SCT passed (%5d edges added, %6d composed)\n%!" !added !composed;
    true
  with Exit ->
    printf "SCT failed (%5d edges added, %6d composed)\n%!" !added !composed;
    false
