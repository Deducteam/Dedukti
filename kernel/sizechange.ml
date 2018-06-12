(** Size change principle.
    This module implements a decision procedure based on the work of
Chin Soon Lee, Neil D. Jones and Amir M. Ben-Amram (POPL 2001).
    Most of this module comes from an implementation of Rodolphe Lepigre
and Christophe Raffalli. *)
open Basic
open Term
open Rule



(** Index of a function symbol. *)
type index = int

(** Conversion to int. *)
let int_of_index : index -> int =
  fun i -> i

(** The pretty printer for the type [index] *)
let pp_index fmt x =
  Format.pp_print_int fmt (int_of_index x)



(** Representation of the set {-1, 0, ∞} *)
type cmp = Min1 | Zero | Infi

(** String representation. *)
let cmp_to_string : cmp -> string =
  function
  | Min1 -> "<"
  | Zero -> "="
  | Infi -> "?"

(** The pretty printer for the type [cmp] *)
let pp_comp fmt c=
  Format.fprintf fmt "%s" (cmp_to_string c)

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
  { w   : int             ; (* Number of argument of callee *)
    h   : int             ; (* Number of argument of caller *)
    tab : cmp array array   (* The matrix of size h*w *)
  }

(** The pretty printer for the type [matrix] *)
let pp_matrix fmt m=
  Format.fprintf fmt "w=%i, h=%i@.tab=[[%a]]@." m.w m.h
    (pp_arr "]@.     [" (pp_arr "," pp_comp)) m.tab
    
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



(** The status expresses if a symbol a constructor or not*)
type symb_status = Set_constructor | Elt_constructor | Def_function | Def_type

(** The pretty printer for the type [symb_status] *)
let pp_status fmt s =
  Format.fprintf fmt "%s"
    (
      if s=Set_constructor || s=Elt_constructor
      then "Static"
      else "Definable"
    )



(** The local result express the result of the termination checker for this symbol *)
type local_result = SelfLooping of (index list) | CriticalPair
                  | UsingBrackets | NonPositive | NotHandledRewritingTypeLevel



(** Representation of a function symbol. *)
type symbol =
  {
    identifier     : name              ; (** Name of the symbol. *)
    arity          : int               ; (** Arity of the symbol (number of args). *)
    mutable status : symb_status       ; (** The status of the symbol *)
    mutable result : local_result list ; (** The information about non termination for this symbol, initially empty *)
  }

(** Map with index as keys. *)
module IMap =
  struct
    include Map.Make(
      struct
        type t = index
        let compare = compare
      end)
      
    (** [find k m] will not raise [Not_found] because it will always be used
        when we are sure that the given key [k] is mapped in [m]. *)
    let find : key -> 'a t -> 'a = fun k m ->
      try find k m
      with Not_found -> assert false
  end



(** A call [{callee; caller; matrix; is_rec}] represents a call to the function symbol with key [callee] by the function symbole with the key [caller].
    The [matrix] gives the relation between the parameters of the caller and the callee.
    The coefficient [matrix.(a).(b)] give the relation between the [a]-th parameter of the caller and the [b]-th argument of the callee.
    The boolean [is_rec] is true when the call is a reccursive call (i.e. a call to a generalised hypothesis lower in the tree.
    It is [false] for every call to subtyping in the typing algorithm and the same goes for rules introducing a new induction hypothesis.
    Every other call refers to a previously introduced induction hypothesis and its boolean is [true]. *)
type call =
  { callee      : index      ; (** Key of the function symbol being called. *)
    caller      : index      ; (** Key of the calling function symbol. *)
    matrix      : matrix     ; (** Size change matrix of the call. *)
    rules       : index list ; (** The list of rules leading to this call *)
  }



(** Internal state of the SCT, including the representation of symbols and the call graph. *)
type call_graph =
  {
    next_index      : index ref             ; (** Just an index not mapped to any symbol yet, in order to easily add a new symbol *)
    next_rule_index : index ref             ; (** Same here *)
    symbols         : symbol IMap.t ref     ; (** A map containing every symbols studied *)
    all_rules       : rule_infos IMap.t ref ; (** A map containing every rules, in order to trace the succession of rule leading to non-termination *)
    calls           : call list ref         ; (** The list of every call *)
  }



(* Global variables *)

(** The call graph which will be studied *)
let graph : call_graph ref =
  let syms = IMap.empty in
  let ruls = IMap.empty in
  ref { next_index = ref 0 ; next_rule_index = ref 0; symbols = ref syms ;
        all_rules = ref ruls ; calls = ref []
      }

(** Saying if the size-change module must be verbose *)
let vb : bool ref=ref false

(** A table linking each symbol with the list of symbols which must be strictly after *)
let must_be_str_after : (name, (name * name) list) Hashtbl.t  =
  Hashtbl.create 5
(* Here 5 is perfectly arbitrary *)

(** A table linking each symbol with the list of symbols which effectively are after (after is a pre-order, it is possible that [a] is after [b] and [b] is after [a] *)
let after : (name, name list) Hashtbl.t =
  Hashtbl.create 5
(* Here again 5 is arbitrary *)

(** The name of the module we are studying *)
let mod_name = ref (mk_mident "")
  
(** This function clean all the global variables, in order to study another file *)
let initialize : bool -> mident -> unit =
  fun v mod_n->
  let syms = IMap.empty in
  let ruls = IMap.empty in
  graph:={ next_index = ref 0 ; next_rule_index = ref 0; symbols = ref syms ;
           all_rules = ref ruls ; calls = ref [] };
  Hashtbl.clear must_be_str_after;
  Hashtbl.clear after;
  vb := v;
  mod_name := mod_n
    
let pp_call fmt c =
  let tbl= !(!graph.symbols) in
  let caller_sym = IMap.find c.caller tbl in
  let callee_sym = IMap.find c.callee tbl in
  let res=ref "" in
  for i=0 to caller_sym.arity -1 do
    res:=!res^"x"^(string_of_int i)^" "
  done;
  Format.fprintf fmt "%a%d(%s%!) <- %a%d%!("
    pp_name caller_sym.identifier c.caller
    !res pp_name callee_sym.identifier c.callee;
  let jj=Array.length c.matrix.tab in
  if jj>0 then
    let ii=Array.length c.matrix.tab.(0) in
    for i = 0 to ii - 1 do
      if i > 0 then Format.fprintf fmt ",";
      let some = ref false in
      for j = 0 to jj - 1 do
        let c = c.matrix.tab.(j).(i) in
        if c <> Infi then
          begin
            let sep = if !some then " " else "" in
            Format.fprintf fmt "%s%s" sep (cmp_to_string c);
            some := true
          end
      done
    done;
    Format.fprintf fmt ")%!";
      

(* The main code of this module *)

(* These exceptions do not have any meaning, but is used to interrupt the iteration on maps *)
exception Success_index  of index
exception Success_status of symb_status

(** [find_key f] will return the key [k] which is mapped to the symbol named [f] *)
let find_key : name ->  index = fun f ->
  try
    IMap.iter
      (
	fun k x -> if name_eq x.identifier f then raise (Success_index k)
      ) !(!graph.symbols);
    raise Not_found
  with Success_index k -> k

(** [find_rule_key r] will return the key [k] which is mapped to the rule [r] *)
let find_rule_key : rule_infos ->  index = fun r ->
  try
    IMap.iter
      (
	fun k x -> if rule_name_eq x.name r.name then raise (Success_index k)
      ) !(!graph.all_rules);
    raise Not_found
  with Success_index k -> k

(** [find_stat f] will return the status [s] of the symbol named [f] *)
let find_stat : name -> symb_status = fun f ->
  try
    IMap.iter
      (
	fun _ x ->
          if name_eq x.identifier f
          then raise (Success_status x.status)
      ) !(!(graph).symbols);
    raise Not_found
with Success_status s -> s

(** Those functions modify the mutable fields in the symbol records *)
let update_result : index -> local_result -> unit = fun i res ->
  let tbl= !(!graph.symbols) in
  let sy=(IMap.find i tbl) in
  sy.result <- res::sy.result

let update_status : index -> symb_status -> unit = fun i res ->
  let tbl= !(!graph.symbols) in
  let sy=(IMap.find i tbl) in
  sy.status <- res

(** Adding the element a at the begining of the list accessed in ht with key id *)
let updateHT : ('a,'b list) Hashtbl.t -> 'a -> 'b -> unit =
  fun ht id x ->
    let add_in_list x l=
      if List.mem x l
      then l
      else x::l
    in
    if Hashtbl.mem ht id
    then
      Hashtbl.replace ht id (add_in_list x (Hashtbl.find ht id))
    else
      Hashtbl.add ht id [x]

(** Creation of a new symbol.  *)
let create_symbol : name -> int -> symb_status -> unit =
  fun identifier arity status ->
    let g= !graph in
    let index = !(g.next_index) in
    if !vb
    then
      Format.printf "Adding the %a symbol %a of arity %i at index %a@."
        pp_status status pp_name identifier arity pp_index index; 
    let sym = {identifier ; arity ; status; result=[]} in
    g.symbols := IMap.add index sym !(g.symbols);
    incr g.next_index

(** Creation of a new rule.  *)
let create_rule : rule_infos -> unit =
  fun r ->
    let g= !graph in
    if !vb
    then
      Format.printf "Adding the rule %a@."
        pp_rule_infos r; 
    let index = !(g.next_rule_index) in
    g.all_rules := IMap.add index r !(g.all_rules);
    incr g.next_rule_index

(** Add a new call to the call graph. *)
let add_call : call-> unit =
  fun cc ->
    let gr= !graph in
    if !vb then Format.printf "%a@." pp_call cc;
    gr.calls := cc :: !(gr.calls)

(** Compare a term and a pattern, using an int indicating under how many lambdas the comparison occurs *)
let rec comparison :  int -> term -> pattern -> cmp =
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
        if (List.length lp) <= (List.length lt)
	then
          match lp,lt with
          | []   , _     -> cur
          | a::l1, b::l2 ->
            begin
              match (comparison nb b a), cur with
	      | _   , Infi -> assert false
	      (* We are sure, that the current state [cur] cannot contain a Infi, else the Infi would be the result of the function and no recursive call would be needed *)
              | Infi, _    -> Infi
              | Min1, _    -> comp_list Min1 l1 l2
      	      | _   , Min1 -> comp_list Min1 l1 l2
      	      | Zero, Zero -> comp_list Zero l1 l2
      	    end
	  | _,_          -> assert false
	(* Since we are studying the case where [lp] is shorter than [lt], this case cannot occur *) 
	else
	  Infi
    in
    match p,t with
    | Var (_,_,n,[]), DB (_,_,m) -> if n+nb=m then Zero else Infi
    | Var (_,_,n,[]), App(DB(_,_,m),_,_) -> if n+nb=m then Zero else Infi
    | Lambda(_,_,Var(_,_,n,_)), App(DB(_,_,m),_,_) -> if n+nb=m+1 then Zero else Infi
    | Pattern (_,f,lp), App(Const(_,g),t1,lt) when (name_eq f g) ->
       begin
	 comp_list Zero lp (t1::lt)
       end
    | Pattern (_,_,l),t -> minus1 (mini Infi (List.map (comparison nb t) l))
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

(** Study the form of the pattern in order to detect the function on which we are patterm matching and certify that no bracket contains more than a variable) *)
let rec study_pm : name -> pattern -> unit =
  fun f ->
    function
    | Pattern  (l,n,ar) ->
      begin
        let stat= find_stat n in
        if stat=Def_function
        then update_status (find_key n) Elt_constructor;
        if stat=Def_type
        then assert false;
        List.iter (study_pm f) ar
      end
    | Var      (_,_,_,l) -> List.iter (study_pm f) l
    | Lambda   (_,_,p)   -> study_pm f p
    | Brackets (DB(_))   -> ()
    | Brackets (_)       -> update_result (find_key f) UsingBrackets

(** Find the index of the caller of a rule *)
let get_caller : rule_infos -> index = fun r ->
  find_key r.cst
	
(** Replace the right hand side of a rule with the term chosen *)
let term2rule : rule_infos -> term -> rule_infos = fun r t ->
  {l=r.l;
   name=r.name;
   ctx=r.ctx;
   cst=r.cst;
   args=r.args;
   rhs=t;
   esize=r.esize;
   pats=r.pats;
   constraints=[]}

(** Find the index of the callee of a rule and the list of its arguments *)
let rec get_callee : int -> rule_infos -> (term list * index) option =
  fun nb r ->
    match r.rhs with
    | DB (_,_,_) | Kind | Type(_) -> None
    | Const (_,f) -> Some ([], find_key f)
    | App (Const(l,f),t1,lt) -> Some (t1::lt, find_key f)
    | App (t1,t2,lt) ->
      begin
        ignore (
          List.map
            add_call
            (List.flatten 
              (List.map
                 (rule_to_call nb)
                 (List.map (term2rule r) (t1::t2::lt))
              )
            )
        );
        None
      end
    | Lam (_,_,_,t) ->
      begin
        ignore (List.map add_call (rule_to_call (nb+1) (term2rule r t))) ;
        None
      end
    | Pi (_,_,t1,t2) ->
      begin
        ignore (
          List.map
            add_call
            (
              (rule_to_call nb (term2rule r t1)) @
              (rule_to_call (nb+1) (term2rule r t2))
            )
        );
        None
      end
          
(** Generate the list of calls associated to a rule. An int is used to specified under how many lambdas are the call *)
and rule_to_call : int -> rule_infos -> call list = fun nb r ->
  let gr = !graph in
  let lp = r.args in
  List.iter (study_pm r.cst) lp;
  if !vb
  then
    Format.printf "We are studying %a@.The caller is %a@.The callee is %a@."
      pp_rule_infos r
      (pp_couple (pp_list "," pp_pattern) pp_index) (lp, get_caller r)
      (pp_option "None" (pp_couple (pp_list "," pp_term) pp_index))
        (get_callee nb r);
   match get_caller r, get_callee nb r with
   | _, None        -> []
   | a, Some (lt,b) ->
     begin
       let m= (IMap.find a !(gr.symbols)).arity in
       let n= (IMap.find b !(gr.symbols)).arity in
       {
         callee=a ; caller = b ;
         matrix=matrix_of_lists m lp n lt nb; rules=[find_rule_key r]
       }::
         (List.flatten (List.map (rule_to_call nb) (List.map (term2rule r) lt)))
     end

(** Take all the rules headed by a symbol and add them to the call graph *)
let add_rules : rule_infos list -> unit =
  fun l ->
    if !vb
    then
      Format.printf "Ajout des règles : @. - %a@."
        (pp_list "\n - " pp_rule_infos) l;
    let ll=List.flatten (List.map (rule_to_call 0) l) in
    if ll=[] then if !vb then Format.printf "Liste de call vide générée@.";
    ignore (List.map add_call ll)
    
let tarjan graph=
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
	let default_name= mk_name (mk_mident "") (mk_ident "") in
        let c=ref [] and w=ref default_name in
        while not (name_eq !w v)
        do
          w:= List.hd !p;
          p:= List.tl !p;
          c:= !w::!c;
        done;
        partition:=!c::!partition
      end
  in
  Hashtbl.iter
    (fun v -> fun _ -> if not (Hashtbl.mem numHT v) then parcours v)
    graph;
  !partition
      
let are_equiv : name -> name -> name list list -> bool = fun a b l ->
  List.exists (fun x -> (List.mem a x) && (List.mem b x)) l

let pp_HT pp_key pp_elt fmt ht=
  Hashtbl.iter (fun a b -> Format.fprintf fmt "%a : %a@." pp_key a pp_elt b) ht
                
let print_after : unit -> unit=
  fun () ->
  Format.printf "@.After :@.%a@." (pp_HT pp_name (pp_list "," pp_name)) after;
  Format.printf "@;"
    
let str_positive :
  name list list -> (name, (name * name) list) Hashtbl.t -> unit =
  fun l ht ->
    Hashtbl.iter (
      fun a b ->
        List.iter (
          fun (x,y) ->
            if (are_equiv a x l)
            then update_result (find_key y) NonPositive) b
    ) ht
	
    
(** the main function, checking if calls are well-founded *)
let sct_only : unit -> unit =
  fun ()->
    let ftbl= !graph in
    let num_fun = !(ftbl.next_index) in
    (* tbl is a num_fun x num_fun Array in which each element is the list of all matrices between the two symbols with the rules which generated this matrix *)
    let tbl = Array.init num_fun (fun _ -> Array.make num_fun []) in
    let print_call ff= pp_call ff in 
  (* counters to count added and composed edges *)
    let added = ref 0 and composed = ref 0 in
  (* function adding an edge, return a boolean indicating
     if the edge is new or not *)
    let add_edge i j m r =
      let ti = tbl.(i) in
      let ms = ti.(j) in
      if List.exists (fun m' -> subsumes (fst m') m) ms
      then
	false
      else
        begin
          (* test idempotent edges as soon as they are discovered *)
          if i = j && prod m m = m && not (decreasing m) then
	    begin
              if !vb
              then 
                Format.printf "edge %a idempotent and looping\n%!" print_call
                  {callee = i; caller = j; matrix = m; rules = r};
              update_result i (SelfLooping r)
	    end;
	  let ms = (m, r) ::
            List.filter (fun m' -> not (subsumes m (fst m'))) ms in
          ti.(j) <- ms;
          true
        end
    in
    (* Test positivity of the signature *)
    str_positive (tarjan after) must_be_str_after;
    (* adding initial edges *)
    if !vb then
      (
        Format.printf "initial edges to be added:\n%!";
        List.iter
          (fun c -> Format.printf "\t%a\n%!" print_call c)
          !(ftbl.calls)
      );
    let new_edges = ref !(ftbl.calls) in
    (* compute the transitive closure of the call graph *)
    if !vb then Format.printf "start completion\n%!";
    let rec fn () =
      match !new_edges with
      | [] -> ()
      | ({callee = i; caller = j; matrix = m; rules=r} as c)::l ->
        new_edges := l;
        if add_edge i j m r
        then
          begin
            if !vb
            then Format.printf "\tedge %a added\n%!" print_call c;
            incr added;
            let t' = tbl.(j) in
            Array.iteri
              (fun k t ->
                 List.iter
                   (fun (m',r') ->
                      let c' = {callee = j; caller = k; matrix = m'; rules=r'}
                      in
                      if !vb
                      then
                        Format.printf "\tcompose: %a * %a = %!"
                          print_call c
                          print_call c';
                      let m'' = prod m m' in
                      let r'' = r @ r' in
                      incr composed;
                      let c'' =
                        {callee = i; caller = k; matrix = m''; rules = r''}
                      in
                      new_edges := c'' :: !new_edges;
                      if !vb then Format.printf "%a\n%!" print_call c'';
                   ) t
              ) t'
        end else
        if !vb then Format.printf "\tedge %a is old\n%!" print_call c;
        fn ()
    in
    fn ();
    if !vb
    then
      Format.printf "SCT passed (%5d edges added, %6d composed)\n%!"
        !added !composed

type position =  Global | Argument | Negative

let under : position -> position =
  function
  | Global -> Argument
  | _ -> Negative

let rec right_most : name -> term -> term = fun f ->
  function
  | Kind                 -> assert false
  | Pi(_,_,_,a)          -> right_most f a
  | App(Lam(_),_,_) as t ->
    update_result (find_key f) NotHandledRewritingTypeLevel; t
  | App(a,_,_)           -> right_most f a
  | Lam(_,_,_,a)         -> assert false
  | DB(_)                -> assert false
  | t                    -> t

let infer_arity_from_type : term -> int = fun t ->
  let rec arity_bis : int -> term -> int = fun n ->
    function
    | Pi (_,_,_,a)-> arity_bis (n+1) a
    | _ -> n
  in arity_bis 0 t
      
let rec constructors_infos : position -> name -> term -> term -> unit =
  fun posit f typ rm ->
    match rm with
    | Type _ ->
      begin
        if not (Hashtbl.mem after f)
        then (Hashtbl.add must_be_str_after f []; Hashtbl.add after f [])
      end
    | _ -> ();
    match typ with
    | Kind -> assert false
    | DB(_,_,_) | Type _ -> ()
    | App(a,_,_) | Lam(_,_,_,a) -> constructors_infos posit f a rm
    | Pi(_,_,lhs,rhs) ->
       begin
	 constructors_infos posit f rhs rm;
	 constructors_infos (under posit) f lhs rm
       end
    | Const(_,g) ->
      begin
        match find_stat g  with
        | Set_constructor ->
          begin
            match rm with
            | Type _ ->
              updateHT after f g; updateHT must_be_str_after f (g,f)
            | Const(_,g2) ->
              begin
                updateHT after g2 g;
                match posit with
                | Negative -> updateHT must_be_str_after g2 (g,f)
                | _ -> ()
              end
            (* TODO : Understand when this case can occur *)
	    | _ -> assert false
	  end
        | _ -> if posit= Negative then update_result (find_key f) NonPositive 
      end

let pp_quat pp1 pp2 pp3 pp4 fmt (a,b,c,d) =
  Format.fprintf fmt "%a,%a,%a,%a" pp1 a pp2 b pp3 c pp4 d
          
let print_sig sg=
  Format.printf "The signature is:@. * %a@."
    (pp_list "\n * "
       (pp_quat
          pp_name
          Signature.pp_staticity
          pp_term
          (pp_option "None"
             (pp_triple
                (pp_list ";" pp_rule_infos)
                Format.pp_print_int
                Dtree.pp_dtree
             )
          )
       )
    ) sg

let print_ext_ru er=
  if er=[] then () else
    Format.printf "Ext_ru contains:@. + %a@."
      (pp_list "\n + " (pp_list " ; " pp_rule_infos)) er

let rec name_var : name -> int -> int -> pattern -> pattern =
  fun f i n ->
    function
    | Var(loc, _, k, ll) when k>=n      ->
      Var(
        loc,
        mk_ident ("x"^(string_of_int i)^"_"^(string_of_int (k-n))),
        k,
        List.map (name_var f i n) ll
      )
    | Var(loc, _, k, ll)                ->
      Var(loc, mk_ident "bound", k, List.map (name_var f i n) ll)
    | Pattern(loc, g, ll)               ->
      Pattern(loc, g, List.map (name_var f i n) ll)
    | Lambda(loc, _, pat)               ->
      Lambda(loc, mk_ident "bound", name_var f i (n+1) pat)
    | Brackets(DB(loc, _, k)) when k>=n ->
      Var(
        loc,
        mk_ident ("x"^(string_of_int i)^"_"^(string_of_int (k-n))),
        k,
        []
      )
    | Brackets(DB(loc, _, k))           ->
      Var(loc, mk_ident "bound", k, [])
    | Brackets(_)                       ->
      update_result (find_key f) UsingBrackets; raise Exit

let rec occurs : ident -> pattern -> bool =
  fun nk ->
    function
    | Var(_, nn, _, _ ) when nk=nn -> true
    | Var(_, _ , _, ll)            -> List.exists (occurs nk) ll
    | Pattern(_, _, ll)            -> List.exists (occurs nk) ll
    | Lambda(_, _, pat)            -> occurs nk pat
    | Brackets(_)                  -> assert false
      
let rec zip_with_int : 'a list -> 'b list -> int -> ('a * 'b * int) list=
  fun l1 l2 n->
    match l1,l2 with
    | []   , []    -> []
    | a::t1, b::t2 -> (a,b,n)::(zip_with_int t1 t2 n)
    | _            -> assert false
       
let rec solve : name -> (pattern * pattern * int) list -> bool =
  fun f l ->
    match l with
    | []                                                                ->
      true
    | (Var(_, n1, _, l1), Var(_, n2, _, l2), n)::tl
      when (n1=n2 && l1=[] && l2=[]) ->
      solve f tl
    | (Var(_, nk, k, ll)  , t2             , n)::tl when (k>=n && ll=[]) ->
      elim f nk t2 tl n
    | (t1             , Var(_, nk, k, ll)  , n)::tl when (k>=n && ll=[]) ->
      elim f nk t1 tl n
    | (Pattern(_, f, l1), Pattern(_, g, l2), n)::tl when f=g             ->
      solve f ((zip_with_int l1 l2 n)@tl)
    | (Lambda(_, _, t1) , Lambda(_, _, t2) , n)::tl                      ->
      solve f ((t1, t2, n+1)::tl)
    | (Var(loc, _, k, ll), t2             , n)::tl when ll!= []          ->
      update_result (find_key f) NotHandledRewritingTypeLevel; true
    | (t1             , Var(loc, _, k, ll), n)::tl when ll!= []          ->
      update_result (find_key f) NotHandledRewritingTypeLevel; true
    | _ -> false
and elim : name -> ident -> pattern -> (pattern * pattern * int) list ->
  int -> bool
  =fun f nk t l n ->
    let rec lift : ident -> pattern -> pattern -> pattern
      = fun nk t ->
        function
        | Var(_,nn,_,_)               when nk=nn -> t
        | Var(_)                      as   t1    -> t1
        | Pattern(loc,f,lt)                      ->
          Pattern(loc,f,List.map (lift nk t) lt)
        | Lambda(loc,id,t1)                      ->
          Lambda(loc,id,lift nk t t1)
        | Brackets(t)                            ->
          assert false
    in
    if occurs nk t then false
    else
      solve f
        (List.map
           (fun (t1,t2,n) ->
              (lift nk t t1, lift nk t t2, n)
           ) l
        )
     
let unifiable : name -> pattern list -> pattern list -> bool = fun f t1 t2->
  try
    solve f
      (zip_with_int
         (List.map (name_var f 1 0) t1) (List.map (name_var f 2 0) t2) 0
      )
  with _ -> true

let rec cp_at_root : rule_infos list -> (rule_infos * rule_infos) option =
  function
  | []   -> None
  | a::l ->
     try
       Some (a, List.find (fun r -> unifiable a.cst a.args r.args) l)
     with
       Not_found -> cp_at_root l
    
(** Initialize the SCT-checker *)	
let termination_check vb mod_name ext_ru whole_sig =
  initialize vb mod_name;
  if vb then (print_sig whole_sig; print_ext_ru ext_ru);
  List.iter
    (fun (fct,stat,typ,rules_opt) ->
      let ar = infer_arity_from_type typ in
      let stat =
	(
	  match (right_most fct typ),stat with
	  | Type _, Signature.Definable -> Def_type
	  | Type _, Signature.Static    -> Set_constructor
	  | _     , _                   -> Def_function
	)
      in
      create_symbol fct ar stat;
      match rules_opt with
      | None -> ()
      | Some (rul,_,_) -> List.iter create_rule rul
    ) whole_sig;
  List.iter
    (fun (fct,_,typ,rules_opt) ->
      match rules_opt with
      | None -> ()
      | Some (rul,_,_) ->
         begin
           add_rules rul;
           match right_most fct typ with
           | Type _ ->
              begin
                match cp_at_root rul with
                | None          -> ()
                | Some (l1, l2) -> update_result (find_key fct) CriticalPair
              end
           | _ -> ()
         end
    ) whole_sig;
  List.iter add_rules ext_ru;
  List.iter
    (fun (fct,_,typ,_) ->
       if
         (IMap.find (find_key fct) !(!graph.symbols)).status=Elt_constructor
       || (IMap.find (find_key fct) !(!graph.symbols)).status=Set_constructor
       then constructors_infos Global fct typ (right_most fct typ)
    ) whole_sig;
  if vb
  then (print_after ();
    Format.printf "%a@." (pp_list ";" (pp_list "," pp_name)) (tarjan after));
  sct_only ();
  let tbl= !(!graph.symbols) in
  IMap.for_all
    (fun _ s-> s.result = []) tbl

type global_result=Terminating | G_SelfLooping
                  | G_UsingBrackets | G_NonPositive | G_CriticalPair
                  | G_NotHandledRewritingTypeLevel

let corresp_loc_glob = function
  | UsingBrackets -> G_UsingBrackets
  | NonPositive -> G_NonPositive
  | CriticalPair -> G_CriticalPair
  | NotHandledRewritingTypeLevel -> G_NotHandledRewritingTypeLevel
  | _ -> assert false

let print_res : bool -> bool -> bool -> unit =
  fun color st res ->
  let tbl= !(!graph.symbols) in
  let ht= Hashtbl.create 5 in
  let list_SL=ref [] in
  let rec fill_res_HT b id =
    let modify_ht x tl=
      updateHT ht (corresp_loc_glob x) id;
      fill_res_HT false id tl
    in
    function
    | []                 -> if b then updateHT ht Terminating id
    | SelfLooping(l)::tl ->
       begin
         list_SL:= (id,l):: !list_SL;
         fill_res_HT false id tl
       end
    | x ::tl -> modify_ht x tl
  in
  IMap.iter
    (fun _ s -> fill_res_HT (s.status = Def_function || s.status = Def_type)
                  s.identifier s.result
    ) tbl;
  let good_module=fun n -> md n = !mod_name in
  begin
    try
      if st
      then Format.eprintf "\027[32m Respect SCP\027[m %a@."
        (pp_list " , " pp_name)
        (List.filter good_module (Hashtbl.find ht Terminating))
    with Not_found -> ()
  end;
  begin
    if !list_SL = [] then ()
    else
      let pp_local= fun fmt (x,y) ->
        Format.fprintf fmt "%a\n%a" pp_name x
          (pp_list "\n"
             (fun fmt ind ->
                let r=IMap.find ind !(!graph.all_rules) in
                Format.fprintf fmt "{%a} %a %a --> %a"
                  pp_rule_name r.name
                  pp_name r.cst
                  (pp_list " " pp_pattern) r.args
                  pp_term r.rhs
             )
          ) y
      in
      Format.eprintf "\027[31m Is self-looping according to SCP\027[m@. - %a@."
        (pp_list "\n - " pp_local)
        (if st
         then (List.filter (fun (n,ll) -> md n= !mod_name) !list_SL)
         else !list_SL
        )
  end;
  let rep g_res s=
    try
      let l=Hashtbl.find ht g_res
      in
      Format.eprintf "%s %a@." s
        (pp_list " , " pp_name)
        (if st then (List.filter (fun n -> md n= !mod_name) l) else l)
    with Not_found -> ()
  in
  let colored n s =
    if color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s
  in
  rep G_UsingBrackets (colored 1 " Use brackets");
  rep G_NonPositive (colored 1 " Not strictly positive");
  rep G_CriticalPair (colored 1 " Critical pair");
  rep G_NotHandledRewritingTypeLevel (colored 1 " Not handled rewriting at type level");
  if res
  then
    Format.eprintf "%s The file %a was proved terminating using SCP@."
      (colored 2 "TERMINATING") pp_mident !mod_name
  else
    Format.eprintf "%s The file %a was not proved terminating@."
      (colored 1 "TERMINATION ERROR") pp_mident !mod_name
