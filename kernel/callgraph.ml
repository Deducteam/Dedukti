open Basic
open Term
open Rule
open Sizematrix


(** Index of a function symbol. *)
type index = int

(** Conversion to int. *)
let int_of_index : index -> int =
  fun i -> i

(** The pretty printer for the type [index] *)
let pp_index fmt x =
  Format.pp_print_int fmt (int_of_index x)


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
type local_result = SelfLooping of (index list)
                  | UsingBrackets | NonPositive | NotHandledRewritingTypeLevel

let pp_local_result : local_result printer =
  fun fmt lr ->
    let st =
      match lr with
      | SelfLooping _ -> "Self Looping"
      | UsingBrackets -> "Using Brackets"
      | NonPositive -> "Non positive type"
      | NotHandledRewritingTypeLevel -> "Rewriting at type level"
    in
    Format.fprintf fmt "%s" st
  


(** Representation of a function symbol. *)
type symbol =
  {
    ind            : index             ; (** The index of the symbol *)
    arity          : int               ; (** Arity of the symbol (number of args). *)
    typ            : term              ; (** Type of the symbol *)
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

module NMap =
  struct
    include Map.Make(
      struct
        type t = name
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
    The coefficient [matrix.(a).(b)] give the relation between the [a]-th parameter of the caller and the [b]-th argument of the callee. *)
type call =
  { callee      : name      ; (** Key of the function symbol being called. *)
    caller      : name      ; (** Key of the calling function symbol. *)
    matrix      : matrix     ; (** Size change matrix of the call. *)
    rules       : index list ; (** The list of rules leading to this call *)
  }



(** Internal state of the SCT, including the representation of symbols and the call graph. *)
type call_graph =
  {
    next_index      : index ref             ; (** The index of the next function symbol to be added *)
    next_rule_index : index ref             ; (** Same here *)
    symbols         : symbol NMap.t ref     ; (** A map containing every symbols studied *)
    all_rules       : rule_infos IMap.t ref ; (** A map containing every rules, in order to trace the succession of rule leading to non-termination *)
    calls           : call list ref         ; (** The list of every call *)
  }


(** The call graph which will be studied *)
let graph : call_graph ref =
  let syms = NMap.empty in
  let ruls = IMap.empty in
  ref { next_index = ref 0; next_rule_index = ref 0; symbols = ref syms ;
        all_rules = ref ruls ; calls = ref []
      }


let pp_call fmt c =
  let tbl= !(!graph.symbols) in
  let caller_sym = NMap.find c.caller tbl in
  let res=ref "" in
  for i=0 to caller_sym.arity -1 do
    res:=!res^"x"^(string_of_int i)^" "
  done;
  Format.fprintf fmt "%a(%s%!) <- %a%!("
    pp_name c.caller 
    !res pp_name c.callee;
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
     

(* These exceptions do not have any meaning, but is used to interrupt the iteration on maps *)
exception Success_index  of index
exception Success_status of symb_status

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
  (NMap.find f !(!graph.symbols)).status

(** Those functions modify the mutable fields in the symbol records *)
let update_result : name -> local_result -> unit = fun i res ->
  let tbl= !(!graph.symbols) in
  let sy=(NMap.find i tbl) in
  sy.result <- res::sy.result

let update_status : name -> symb_status -> unit = fun i res ->
  let tbl= !(!graph.symbols) in
  let sy=(NMap.find i tbl) in
  sy.status <- res

(** Add a new call to the call graph. *)
let add_call : call-> unit =
  fun cc ->
    let gr= !graph in
    Debug.(debug d_sizechange "%a" pp_call cc);
    gr.calls := cc :: !(gr.calls)



(** Study the form of the pattern in order to detect the function on which we are pattern matching and certify that no bracket contains more than a variable) *)
let rec study_pm : name -> pattern -> unit =
  fun f ->
    function
    | Pattern  (l,n,ar) ->
      begin
        let stat= find_stat n in
        if stat=Def_function
        then update_status n Elt_constructor;
        if stat=Def_type
        then assert false;
        List.iter (study_pm f) ar
      end
    | Var      (_,_,_,l) -> List.iter (study_pm f) l
    | Lambda   (_,_,p)   -> study_pm f p
    | Brackets (DB(_))   -> ()
    | Brackets (_)       -> update_result f UsingBrackets

(** Find the index of the callee of a rule and the list of its arguments *)
let rec get_callee : int -> rule_infos -> (term list * name) option =
  fun nb r ->
    match r.rhs with
    | DB (_,_,_) | Kind | Type(_) -> None
    | Const (_,f) -> Some ([], f)
    | App (Const(l,f),t1,lt) -> Some (t1::lt, f)
    | App (t1,t2,lt) ->
      begin
          List.iter
            add_call
            (List.flatten 
              (List.map
                 (rule_to_call nb)
                 (List.map (term2rule r) (t1::t2::lt))
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
          List.iter
            add_call
            (
              (rule_to_call nb (term2rule r t1)) @
              (rule_to_call (nb+1) (term2rule r t2))
            );
        None
      end


(** Generate the list of calls associated to a rule. An int is used to specified under how many lambdas are the call *)
and rule_to_call : int -> rule_infos -> call list = fun nb r ->
  let gr = !graph in
  let lp = r.args in
  List.iter (study_pm r.cst) lp;
  let callee = get_callee nb r in
   Debug.(debug d_sizechange "We are studying %a@.The caller is %a@.The callee is %a"
      pp_rule_infos r
      (pp_pair (pp_list "," pp_pattern) pp_name) (lp, r.cst)
      (pp_option "None" (pp_pair (pp_list "," pp_term) pp_name))
        callee);
   match r.cst, callee with
   | _, None        -> []
   | a, Some (lt,b) ->
     begin
       let m= (NMap.find a !(gr.symbols)).arity in
       let n= (NMap.find b !(gr.symbols)).arity in
       {
         callee=a ; caller = b ;
         matrix=matrix_of_lists m lp n lt nb; rules=[find_rule_key r]
       }::
         (List.flatten (List.map (rule_to_call nb) (List.map (term2rule r) lt)))
     end




let infer_arity_from_type : term -> int = fun t ->
  let rec arity_bis : int -> term -> int = fun n ->
    function
    | Pi (_,_,_,a)-> arity_bis (n+1) a
    | _ -> n
  in arity_bis 0 t
