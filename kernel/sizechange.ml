(** Size change principle.
    This module implements a decision procedure based on the work of Chin Soon Lee, Neil D. Jones and Amir M. Ben-Amram (POPL 2001).
    Most of this module comes from an implementation of Rodolphe  Lepigre and Christophe Raffalli. *)
open Basic
open Term
open Rule
open Format

(** Index of a function symbol. *)
type index = int

(** Conversion to int. *)
let int_of_index : index -> int = fun i -> i

(** Index of the root. *)
let root : index = -1

exception Calling_unknown of int
exception NonLinearity of int
exception TypingError of name
exception NonPositivity of name
exception ModuleDependancy of name
exception PatternMatching of int
exception TypeLevelRewriteRule of (name * name)
exception TypeLevelWeird of (name * term)
exception TarjanError
            
type symb_status = Set_constructor | Elt_constructor | Def_function | Def_type
      
(** The most useful function in the world *)
let erase : 'a -> unit =
  fun _ -> ()
  
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
  { w   : int (* Number of argument of callee *)
  ; h   : int (* Number of argument of caller *)
  ; tab : cmp array array }
  
(** Matrix product. *)
let prod : matrix -> matrix -> matrix = fun m1 m2 ->
  assert (m1.w = m2.h);
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

(** A call [{callee; caller; matrix; is_rec}] represents a call to the function symbol with key [callee] by the function symbole with the key [caller].
    The [matrix] gives the relation between the parameters of the caller and the callee.
    The coefficient [matrix.(a).(b)] give the relation between the [a]-th parameter of the caller and the [b]-th argument of the callee.
    The boolean [is_rec] is true when the call is a reccursive call (i.e. a call to a generalised hypothesis lower in the tree.
    It is [false] for every call to subtyping in the typing algorithm and the same goes for rules introducing a new induction hypothesis.
    Every other call refers to a previously introduced induction hypothesis and its boolean is [true]. *)
    
type call =
    { callee : index  (** Key of the function symbol being called. *)
    ; caller : index  (** Key of the calling function symbol. *)
    ; matrix : matrix (** Size change matrix of the call. *)}

(** Representation of a function symbol. *)
type symbol =
    { name  : name (** Name of the symbol. *)
    ; arity : int             (** Arity of the symbol (number of args). *)}

(** Internal state of the SCT, including the representation of symbols and the call graph. *)
type call_graph =
    { next_index : index ref
    ; symbols    : symbol IMap.t ref
    ; calls      : call list ref }

let def_name = mk_name (mk_mident "") (mk_ident "")
      
(* Global variables *)
let graph : call_graph ref =
  let root = { name  = def_name ; arity = 0} in
  let syms = IMap.singleton (-1) root in
  ref { next_index = ref 0 ; symbols = ref syms ; calls = ref [] }

let vb : bool ref=ref false
let table : (name * int * index) list ref = ref []
let constructors : name list ref= ref []
let must_be_str_after : (name, name list) Hashtbl.t  = Hashtbl.create 5
let after : (name, name list) Hashtbl.t = Hashtbl.create 5

let initialize : bool -> unit =
  fun v->
  let root = { name  = def_name ; arity = 0} in
  let syms = IMap.singleton (-1) root in
  graph:={ next_index = ref 0 ; symbols = ref syms ; calls = ref [] };
  table:=[];
  constructors:=[];
  Hashtbl.clear must_be_str_after;
  Hashtbl.clear after;
  vb:=v
                                                                         
(** Printing functions *)
let printHT key_printer elt_printer ht=
  Hashtbl.iter (fun a b -> printf "%a : %a@." key_printer a elt_printer b) ht

let pp_index fmt x = pp_print_int fmt (int_of_index x)
    
let pp_couple pp_fst pp_snd fmt x = fprintf fmt "(%a, %a)" pp_fst (fst x) pp_snd (snd x)

let pp_triple pp_fst pp_snd pp_thd fmt (x,y,z) = fprintf fmt "(%a, %a, %a)" pp_fst x pp_snd y pp_thd z

 let pp_quat pp1 pp2 pp3 pp4 fmt (a,b,c,d)= fprintf fmt "%a,%a,%a,%a" pp1 a pp2 b pp3 c pp4 d

let pp_stat fmt s = fprintf fmt "%s" (if s=Signature.Static then "Static" else "Definable")
                                                         
let pp_option pp_arg fmt a=
  match a with
  | None -> fprintf fmt "%a" pp_print_string "None"
  | Some n -> fprintf fmt "%a" pp_arg n

let pp_array sep pp fmt v=pp_list sep pp fmt (Array.to_list v)

let pp_matrix fmt m=
  let res=ref [] in
  Array.iter (fun l -> let res2=ref [] in Array.iter (fun x -> res2:=(cmp_to_string x):: !res2) l; res:=(List.rev !res2):: !res) m.tab;
  fprintf fmt "w=%i, h=%i, tab=@.[[%a]]@." m.w m.h (pp_list "]\n[" (pp_list "," pp_print_string)) (List.rev !res)
                                  
let print_call : symbol IMap.t -> formatter -> call -> unit =
  fun tbl ff c->
  let caller_sym = IMap.find c.caller tbl in
  let callee_sym = IMap.find c.callee tbl in
  let res=ref "" in
  for i=0 to caller_sym.arity -1 do
    res:=!res^"x"^(string_of_int i)^" "
  done;
  fprintf ff "%a%d(%s%!) <- %a%d%!(" pp_name caller_sym.name c.caller
    !res pp_name callee_sym.name c.callee;
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
            fprintf ff "%s%s" sep (cmp_to_string c);
            some := true
          end
      done
    done;
    fprintf ff ")%!"
     
(** An enrichment of Basic with couple *)
let couple_id_eq : (ident * ident) -> (ident * ident) -> bool =
    fun (a,b) (c,d)->
      (ident_eq a c) && (ident_eq b d)

(** Adding the element a at the begining of the list accessed in ht with key id *)
let updateHT : ('a,'b list) Hashtbl.t -> 'a -> 'b -> unit =
  fun ht id x ->
  if Hashtbl.mem ht id
  then
    Hashtbl.replace ht id (x::Hashtbl.find ht id)
  else
    Hashtbl.add ht id [x]
                    
(** Creation of a new symbol.  *)
let create_symbol : name -> int -> unit =
  fun name arity ->
  let g= !graph in
  if !vb then printf "Ajout du symbole %a d'arité %i@." pp_name name arity; 
  let index = !(g.next_index) in
  let sym = {name=name ; arity} in
  table:=(name, arity, index)::!table;
  g.symbols := IMap.add index sym !(g.symbols);
  incr g.next_index

(** Add a new call to the call graph. *)
let add_call : call-> unit =
  fun cc ->
  let gr= !graph in
  if !vb then printf "%a@." (print_call !(gr.symbols)) cc;
  gr.calls := cc :: !(gr.calls)

let rec comparison : int -> term -> pattern -> cmp =
  fun nb t p ->
    let minus1 : cmp -> cmp =
      function
      | Zero -> Min1
      | n -> n
    in
    let rec mini : cmp -> cmp list -> cmp =
      fun cur ll ->
	match ll with
	| []-> cur
	| Min1::l -> Min1
	| Zero::l -> mini Zero l
	| Infi::l -> mini cur l
    in
    let rec comp_list : cmp -> pattern list -> term list -> cmp =
      fun cur lp lt ->
        if List.length lp=List.length lt
	then
          match lp,lt with
          | [],[] -> cur
          | a::l1,b::l2 ->
            begin
              match comparison nb b a,cur with
              | Infi, _ -> Infi
              | Min1,_ -> comp_list Min1 l1 l2
      	      | _, Min1 -> comp_list Min1 l1 l2
      	      | Zero,Zero -> comp_list Zero l1 l2
      	    end
	else
	  Infi
    in
    match p,t with
    | Var (_,_,n,[]), DB (_,_,m) -> if n+nb=m then Zero else Infi
    | Pattern (_,n,lp), App(Const(_,g),t1,lt) when (name_eq n g) ->
       begin
	 (* printf "%a et %a\n" pp_pattern p pp_term t; *)
	 comp_list Zero lp (t1::lt)
       end
    | Pattern (_,_,l),t -> minus1 (mini Infi (List.map (comparison nb t) l))
    | _ -> Infi
           
let matrix_of_lists : int -> term list -> pattern list -> matrix =
  fun nb l1 l2 ->
  (* printf "Matrix_of_list de@.patt:%a@.term:%a@." (pp_list " , " pp_pattern) l2 (pp_list " , " pp_term) l1; *)
  let n=List.length l1 in
  let m=List.length l2 in
  let res =ref [] in
  for i=0 to m-1 do
    let p=List.nth l2 i in
    let loc_res =ref [] in
    for j=0 to n-1 do
      let t=List.nth l1 j in
      (* printf "In matrix_of_lists, %a et %a@." pp_pattern p pp_term t; *)
      loc_res:=(comparison nb t p)::!loc_res
    done;
    res:=(Array.of_list (List.rev !loc_res))::!res
  done;
  {h=m ; w=n ; tab = Array.of_list (List.rev !res)}

let auto_call_matrix : index -> index -> matrix =
  fun a b ->
  let same x y=if x=y then Zero else Infi in
  let second (a,b,c) = b in
  let n=second (List.find (fun (_,_,x) -> x=a) !table) in
  let m=second (List.find (fun (_,_,x) -> x=b) !table) in
  let res =ref [] in
  for i=0 to m-1 do
    let loc_res =ref [] in
    for j=0 to n-1 do
      loc_res:=(same i j)::!loc_res
    done;
    res:=(Array.of_list (List.rev !loc_res))::!res
  done;
  {h=m ; w=n ; tab = Array.of_list (List.rev !res)}

let rec detect_wrong_pm : pattern -> unit=
  function
  | Pattern (l,n,ar) -> if not (List.mem n !constructors) then raise (PatternMatching (fst (of_loc l))); List.iter detect_wrong_pm ar
  | _ -> ()
    
let rec rule_to_call : int -> rule_infos -> call list =
  fun nb r ->
  let gr= !graph in
  (* if not (r.constraints=[]) then raise (NonLinearity (fst (of_loc r.l)))
  else *)
      let third (a,b,c) = c in
      let get_caller : pattern list * index =
	let lp=r.args in
	List.iter detect_wrong_pm lp;
	try 
	  (lp,third (List.find (fun (x,ar,_) -> (name_eq x r.cst && ar=List.length(lp))) !table))
	with Not_found ->
	  begin
            try
	      let old_index=third (List.find (fun (x,ar,_) -> (name_eq x r.cst && ar>List.length(lp))) !table) in
	      let new_index= !(gr.next_index) in
	      create_symbol r.cst (List.length lp);
	      add_call { callee = new_index; caller =old_index; matrix=auto_call_matrix old_index new_index};
	      (lp,new_index)
            with Not_found -> raise (Calling_unknown (fst (of_loc r.l)))
	  end
      in
      let term2rule t= {l=r.l; name=r.name; ctx=r.ctx; cst=r.cst; args=r.args; rhs=t; esize=r.esize; l_args=r.l_args; constraints=[]} in
      let get_callee : (term list * index) option =
	match r.rhs with
	| DB (_,_,_) | Kind | Type(_) -> None
	| Const (_,f) ->
	   begin
             try Some ([],third (List.find (fun (x,ar,_) -> (name_eq x f) && ar=0) !table))
             with Not_found ->
               begin
		 try
		   let old_index=third (List.find (fun (x,ar,_) -> (name_eq x f)) !table)
		   in
		   let new_index= !(gr.next_index)
		   in
		   create_symbol f 0;
		   add_call { callee = new_index; caller = old_index; matrix=auto_call_matrix new_index old_index};
		   Some ([],new_index)
		 with Not_found -> None
               end
	   end
	| App (Const(_,f),t1,lt) ->
	   begin
             try Some (t1::lt,third (List.find (fun (x,ar,_) -> (name_eq x f) && ar=List.length(lt)+1) !table))
             with Not_found ->
               begin
		 try
		   let old_index=third (List.find (fun (x,ar,_) -> (name_eq x f) && ar>List.length(lt)+1) !table)
		   in
		   let new_index= !(gr.next_index)
		   in
		   create_symbol f (List.length (t1::lt));
		   add_call { callee = new_index; caller = old_index; matrix=auto_call_matrix new_index old_index};
		   Some (t1::lt,new_index)
		 with Not_found ->
                   begin
		     try
		       let old_index=third (List.find (fun (x,ar,_) -> (name_eq x f) && ar<=List.length(lt)) !table)
		       in
		       let new_index= !(gr.next_index)
		       in
		       create_symbol f (List.length (t1::lt));
		       add_call { callee = old_index; caller = new_index; matrix=auto_call_matrix old_index new_index};
		       Some (t1::lt,new_index)
		     with Not_found ->
                       erase (List.map add_call ((rule_to_call nb (term2rule t1)) @List.flatten (List.map (rule_to_call nb) (List.map term2rule lt))));
                       None
                   end
               end
	   end
	| App (t1,t2,lt) ->
	   begin
             erase (List.map add_call  ((rule_to_call nb (term2rule t1)) @ (rule_to_call nb (term2rule t2)) @ List.flatten (List.map (rule_to_call nb) (List.map term2rule lt)))) ;
             None
	   end
	| Lam (_,_,_,t) ->
	   begin
             erase (List.map add_call (rule_to_call (nb+1) (term2rule t))) ;
             None
	   end
	| Pi (_,_,t1,t2) ->
	   begin
             erase (List.map add_call ((rule_to_call nb (term2rule t1)) @ (rule_to_call (nb+1) (term2rule t2)))) ;
             None
	   end
      in
      if !vb then printf "We are studying %a@.The caller is %a@.The callee is %a@." pp_rule_infos r (pp_couple (pp_list "," pp_pattern) pp_index) get_caller (pp_option (pp_couple (pp_list "," pp_term) pp_index)) get_callee;
      match get_callee,get_caller with
      | None, _ -> []
      | Some (l1,a), (l2,b) -> {callee=a ; caller = b ; matrix=matrix_of_lists nb l1 l2}::List.flatten (List.map (rule_to_call nb) (List.map term2rule l1))

let add_rules : rule_infos list -> unit =
  fun l ->
    if !vb then printf "Ajout des règles : @. - %a@." (pp_list "\n - " pp_rule_infos) l;
    let ll=List.flatten (List.map (rule_to_call 0) l) in
    if ll=[] then if !vb then printf "Liste de call vide générée@.";
    erase (List.map add_call ll)
    
let latex_print_calls : unit -> unit=
  fun () ->
  let gr= !graph in
  let ff= std_formatter in
  let arities = IMap.bindings !(gr.symbols) in
  let calls = !(gr.calls) in
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
      fprintf ff "    N%d [ label = \"%a_{%d}\" ];\n" (index j) pp_name sym.name (index j)
    else
      fprintf ff "    N%d [ label = \"%a\" ];\n" (index j) pp_name sym.name
  in
  List.iter f (List.filter (fun (i,_) ->
    List.exists (fun c -> i = c.caller || i = c.callee) calls) arities);
  let print_call2 arities {callee = i; caller = j; matrix = m} =
    let {name = namej; arity = aj} =
      try List.assoc j arities with Not_found -> assert false
    in
    let {name = namei; arity = ai} =
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
    
let tarjan graph=
  try 
    let num=ref 0 and p = ref [] and partition = ref []
    and numHT= Hashtbl.create 5 and numAcc = Hashtbl.create 5 in
    let rec parcours v=
      Hashtbl.add numHT v !num;
      Hashtbl.add numAcc v !num;
      incr num;
      p := v::!p;
      let parcours_intern w=
	if not (Hashtbl.mem numHT w)
	then
          (parcours w;
           Hashtbl.replace numAcc v (min (Hashtbl.find numAcc v) (Hashtbl.find numAcc w)))
	else
          (if List.mem w !p
           then Hashtbl.replace numAcc v (min (Hashtbl.find numAcc v) (Hashtbl.find numHT w)))
      in
      List.iter parcours_intern (Hashtbl.find graph v);
      if (Hashtbl.find numAcc v)=(Hashtbl.find numHT v)
      then
	begin
          let c=ref [] and w=ref def_name in
          while not (name_eq !w v)
          do
            w:= List.hd !p;
            p:= List.tl !p;
            c:= !w::!c;
          done;
          partition:=!c::!partition
	end
    in
    Hashtbl.iter (fun v -> fun _ -> if not (Hashtbl.mem numHT v) then parcours v) graph;
    !partition
  with
  | _ -> raise TarjanError
      
let are_equiv a b l=
  List.exists (fun x -> (List.mem a x) && (List.mem b x)) l

let print_constr : unit -> unit=
  fun () ->
  printf "@.Constructors :@.%a@." (pp_list ";" pp_name) !constructors;
  printf "@.After :@."; printHT pp_name (pp_list "," pp_name) after;
  printf "@;"
    
let str_positive l ht=
  Hashtbl.iter (fun a b -> if List.exists (fun x -> are_equiv a x l) b then raise (NonPositivity a)) ht
	
    
(** the main function, checking if calls are well-founded *)
let sct_only : unit -> bool =
  fun ()->
    let ftbl= !graph in
    let num_fun = !(ftbl.next_index) in
    let arities = !(ftbl.symbols) in
    let tbl = Array.init num_fun (fun _ -> Array.make num_fun []) in
    let print_call ff= print_call arities ff in 
  (* counters to count added and composed edges *)
    let added = ref 0 and composed = ref 0 in
  (* function adding an edge, return a boolean indicating
     if the edge is new or not *)
    let add_edge i j m =
    (* test idempotent edges as soon as they are discovered *)
      if i = j && prod m m = m && not (decreasing m) then
	begin
          if !vb then 
            printf "edge %a idempotent and looping\n%!" print_call
              {callee = i; caller = j; matrix = m};
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
    (* Test positivity of the signature *)
    str_positive (tarjan after) must_be_str_after;
  (* adding initial edges *)
    try
      if !vb then
	(printf "initial edges to be added:\n%!";
	 List.iter (fun c -> printf "\t%a\n%!" print_call c) !(ftbl.calls));
      let new_edges = ref !(ftbl.calls) in
    (* compute the transitive closure of the call graph *)
      if !vb then printf "start completion\n%!";
      let rec fn () =
	match !new_edges with
	| [] -> ()
	| {callee = i; caller = j}::l when j < 0 -> new_edges := l; fn () (* ignore root *)
	| ({callee = i; caller = j; matrix = m} as c)::l ->
           assert (i >= 0);
          new_edges := l;
          if add_edge i j m then begin
            if !vb then printf "\tedge %a added\n%!" print_call c;
            incr added;
            let t' = tbl.(j) in
            Array.iteri (fun k t -> List.iter (fun m' ->
              let c' = {callee = j; caller = k; matrix = m'} in
              if !vb then printf "\tcompose: %a * %a = %!" print_call c print_call c';
              let m'' = prod m' m in
              incr composed;
              let c'' = {callee = i; caller = k; matrix = m''} in
              new_edges := c'' :: !new_edges;
              if !vb then printf "%a\n%!" print_call c'';
            ) t) t'
          end else
            if !vb then printf "\tedge %a is old\n%!" print_call c;
          fn ()
      in
      fn ();
      if !vb then printf "SCT passed (%5d edges added, %6d composed)\n%!" !added !composed;
      true
    with
    | Exit -> if !vb then printf "SCT failed (%5d edges added, %6d composed)\n%!" !added !composed; false

type position =  Global | Argument | Negative

let under : position -> position =
  function
  | Global -> Argument
  | _ -> Negative

let rec right_most : term -> term =
  function
  | Pi(_,_,_,a) -> right_most a
  | App(a,_,_) -> right_most a
  | Lam(_,_,_,a) -> right_most a
  | t -> t

let find_status : name -> (name * Signature.staticity * term * (rule_infos list*int*Dtree.dtree) option) list -> symb_status=
  fun f sign ->
  try 
    let (_,stat,typ,_) = List.find (fun (g,_,_,_) -> (name_eq f g)) sign in
    match stat,(right_most typ) with
    | Signature.Static,Type _ -> Set_constructor
    | Signature.Static,_ -> Elt_constructor
    | Signature.Definable,Type _ -> Def_type
    | Signature.Definable,_ -> Def_function
  with
  | _ -> raise (ModuleDependancy f)
      
let rec constructors_infos : position -> name -> term -> term -> (name * Signature.staticity * term * (rule_infos list*int*Dtree.dtree) option) list -> unit =
  fun posit f typ rm sign->
    match rm with
    | Type _ -> if not (Hashtbl.mem after f) then (Hashtbl.add must_be_str_after f []; Hashtbl.add after f [])
    | _ -> ();
    match typ with
    | Kind -> raise (TypingError f)
    | DB(_,_,_) | Type _ -> ()
    | App(a,_,_) | Lam(_,_,_,a) -> constructors_infos posit f a rm sign
    | Pi(_,_,lhs,rhs) ->
       begin
	 constructors_infos posit f rhs rm sign;
	 constructors_infos (under posit) f lhs rm sign
       end
    | Const(_,g) ->
       begin
	 match find_status g sign with
	 | Set_constructor ->
	    begin
	        match rm with
	        | Type _ ->  updateHT after f g; updateHT must_be_str_after f g
                | Const(_,g2) ->
                   begin
                     updateHT after g2 g;
                     match posit with
                     | Negative -> updateHT must_be_str_after g2 g
                     | _ -> ()
                   end
	        | _ -> raise (TypeLevelRewriteRule (f,g))
	    end
         |_ -> ()
       end
    | _ -> raise (TypeLevelWeird (f,typ))

let print_sig sg=
  printf "La signature est:@. * %a@." (pp_list "\n * " (pp_quat pp_name pp_stat pp_term (pp_option (pp_triple (pp_list ";" pp_rule_infos) pp_print_int Dtree.pp_dtree)))) sg

let print_ext_ru er=
  if er=[] then () else
    printf "Ext_ru contient:@. + %a@." (pp_list "\n + " (pp_list " ; " pp_rule_infos)) er
    
(** Initialize the SCT-checker *)	
let termination_check vb szgraph mod_name ext_ru whole_sig =
  initialize vb;
  if vb then (print_sig whole_sig; print_ext_ru ext_ru);
  List.iter
    (fun (fct,_,_,rules_opt) ->
      match rules_opt with
      | None -> ()
      | Some (_,arit,_) -> create_symbol fct arit
    ) whole_sig;
  List.iter
    (fun (fct,st,typ,_) ->
      match st with
      | Signature.Definable -> ()
      | Signature.Static ->  constructors:= fct:: !constructors; constructors_infos Global fct typ (right_most typ) whole_sig
    ) whole_sig;
  List.iter
    (fun (fct,st,typ,rules_opt) ->
      match rules_opt with
      | None -> ()
      | Some (rul,arit,dec_tree) -> add_rules rul
    ) whole_sig;
  List.iter add_rules ext_ru;
  if vb then printf "Table :@. ' %a@." (pp_list "\n ' " (pp_triple pp_name pp_print_int pp_index)) !table;
  if vb
  then (print_constr ();
    printf "%a@." (pp_list ";" (pp_list "," pp_name)) (tarjan after));
  if szgraph then latex_print_calls ();
  sct_only ()
