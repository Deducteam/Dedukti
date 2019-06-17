open Basic
open Format
open Rule
open Term
open Dtree
open Matching
open Ac


type Debug.flag += D_reduce
let _ = Debug.register_flag D_reduce "Reduce"

type red_target   = Snf | Whnf
type red_strategy = ByName | ByValue | ByStrongValue

type red_cfg = {
  select   : (Rule.rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  target   : red_target;
  strat    : red_strategy;
  beta     : bool;
  logger   : position -> Rule.rule_name -> term Lazy.t -> term Lazy.t -> unit;
}

let pp_red_cfg fmt cfg =
  let args =
    (match cfg.target   with Snf     -> ["SNF"]                             | _ -> []) @
    (match cfg.strat    with ByValue -> ["CBV"] | ByStrongValue -> ["CBSV"] | _ -> []) @
    (match cfg.nb_steps with Some i  -> [string_of_int i]                   | _ -> []) in
  Format.fprintf fmt "[%a]" (pp_list "," Format.pp_print_string) args

let default_cfg =
  {select=None; nb_steps=None; target=Snf; strat=ByName; beta=true; logger=fun _ _ _ _ -> () }

exception NotConvertible

let rec zip_lists l1 l2 lst =
  match l1, l2 with
  | [], [] -> lst
  | s1::l1, s2::l2 -> zip_lists l1 l2 ((s1,s2)::lst)
  | _,_ -> raise NotConvertible

(* State *)

type env = term Lazy.t LList.t

(* A state {ctx; term; stack} is the state of an abstract machine that
represents a term where [ctx] is a ctx that contains the free variables
of [term] and [stack] represents the terms that [term] is applied to. *)
type state = {
  ctx   : env;    (* context *)
  term  : term;   (* term to reduce *)
  stack : stack;  (* stack *)
  reduc : (bool * state) ref;
  (* Pointer to a state in a more reduced form representing the same term. *)
}
and stack = state list

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx term ) in
  mk_App2 t (List.map term_of_state stack)


(** Creates a fresh state with reduc pointing to itself. *)
let mk_state ctx term stack =
  let rec t = { ctx; term; stack; reduc = ref (false, t) } in t

let state_of_term t = mk_state LList.nil t []

(* Pretty Printing *)

let pp_env fmt (env:env) = pp_list ", " pp_term fmt (List.map Lazy.force (LList.lst env))

let pp_stack fmt (st:stack) =
  fprintf fmt "[ %a ]\n" (pp_list "\n | " pp_term) (List.map term_of_state st)

let pp_stack_no_endline fmt (st:stack) =
  fprintf fmt "[ %a ]" (pp_list " | " pp_term) (List.map term_of_state st)

let pp_state ?(if_ctx=true) ?(if_stack=true) fmt st =
  let { ctx; term; stack; reduc } = st in
  if if_ctx
  then fprintf fmt "{ctx=[%a]; " pp_env ctx
  else fprintf fmt "{ctx=[...](%i);@ " (LList.len ctx);
  fprintf fmt "term=%a; " pp_term term;
  if if_stack
  then fprintf fmt "stack=%a}" pp_stack_no_endline stack
  else fprintf fmt "stack=[...]}"

let simpl_pp_state = pp_state ~if_ctx:true ~if_stack:true


(** Creates a fresh state using the same reduc pointer as [st].
    This pointer now points to the fresh state. *)
let mk_reduc_state st ctx term stack =
  let st' = { ctx; term; stack; reduc = st.reduc } in
  st.reduc := (false, st');
  st'

(** Creates a (fresh) final state from given state and redirect pointer to it. *)
let rec set_final st =
  assert (snd !(st.reduc) == st);
  st.reduc := (true, st)

let as_final st = set_final st; st

exception Not_convertible

(* ********************* *)

type rw_strategy         = Signature.t -> term -> term
type rw_state_strategy   = Signature.t -> state -> state

type matching_test = Rule.constr -> Rule.rule_name -> Signature.t -> term -> term -> bool
type convertibility_test = Signature.t -> term -> term -> bool


module type ConvChecker = sig
  val are_convertible : convertibility_test
  val matching_test   : matching_test
  val conversion_step : Signature.t -> term * term -> (term * term) list -> (term * term) list
end

module type S = sig
  include ConvChecker
  val reduction       : red_cfg -> Signature.t -> term -> term
  val whnf            : Signature.t -> term -> term
  val snf             : Signature.t -> term -> term
end


(* Should eta expansion be allowed at conversion check ? *)
let eta = ref false

(* Should beta steps be allowed at reduction ? *)
let beta = ref true

(* Rule filter *)
let selection  = ref None

module Make(C : ConvChecker) : S =
struct

(*******      AC manipulating functions   *******)

let filter_neutral conv sg l cst terms =
  match Signature.get_algebra sg l cst with
  | ACU neu ->
    (match List.filter (fun x -> not (conv neu x)) terms with
     | [] -> [neu] | s -> s)
  | _ -> terms

(* Unfolds all occurences of the AC(U) symbol in the stack
 * Removes occurence of neutral element.  *)
let flatten_AC_stack sg strategy
    (l:loc) (cst:name) (stack:stack) : stack =
  let rec flatten acc = function
    | [] -> acc
    | { term=Const (l',cst'); stack=[st1;st2] } :: tl when name_eq cst cst' ->
      flatten acc (st1 :: st2 :: tl)
    | st :: tl ->
      match strategy sg st with
      | { term=Const (l',cst'); stack=[st1;st2] } when name_eq cst cst' ->
        flatten acc (st1 :: st2 :: tl)
      | whnf_st -> flatten (whnf_st::acc) tl
  in
  let stack = flatten [] stack in
  match Signature.get_algebra sg l cst with
  | ACU neu -> List.filter (fun st -> not (C.are_convertible sg (term_of_state st) neu)) stack
  | _ -> stack



(** Builds a comb-shaped AC term from a list of arguments. *)
let to_comb sg l cst ctx stack =
  let rec f = function
    | []         -> state_of_term (Signature.get_neutral sg l cst)
    | [t]        -> t
    | t1::t2::tl -> f ((mk_state ctx (mk_Const l cst) [t1;t2]) :: tl)
  in
  f stack

let comb_state_shape_if_AC sg strategy st =
  match st with
  | { ctx; term=Const (l,cst); stack=(s1::s2::rstack) } when Signature.is_AC sg l cst ->
    let nstack = flatten_AC_stack sg strategy l cst [s1;s2] in
    let s = to_comb sg l cst ctx nstack in
    let rstack = (match rstack with [] -> s.stack | l -> s.stack@l) in
    mk_reduc_state st s.ctx s.term rstack
  | st -> st

let comb_term_shape_if_AC sg : term -> term = function
  | App(Const (l,cst), a1, a2::remain_args) when Signature.is_AC sg l cst ->
     let id_comp = Signature.get_id_comparator sg in
     let args = flatten_AC_terms cst [a1;a2] in
     let args = filter_neutral (C.are_convertible sg) sg l cst args in
     let args = List.sort (compare_term id_comp) args in
     let _ = assert (List.length args > 0) in
     mk_App2
       (unflatten_AC (cst, Signature.get_algebra sg l cst) args)
       remain_args
  (* mk_App2 (unflatten sg l cst args) remain_args *)
  | t -> t

let rec find_case (flattenner:loc->name->stack->stack)
    (st:state) (case:case) : stack option =
  match st, case with
  (** This case is a bit tricky:
      [] C (+ f g 1) --> plus (f 1) (g 1).
      C (h 1) matche avec  +{f,g} ~ +{h}
      Maybe + is acu, maybe h reduces to (+ i j), who knows...
      TODO: check that this case is used properly !
  *)
  | { ctx; term=t; stack } , CConst (nargs,cst,true)
    when List.length stack == nargs - 2 -> (* should we also check for the type of t ? *)
    let new_st = mk_state ctx t [] in
    let new_stack = flattenner dloc cst [new_st] in
    Some ( (mk_state ctx (mk_Const dloc cst) new_stack) :: stack)

  | { ctx; term=Const (l,cst); stack=t1::t2::s } , CConst (nargs,cst',true)
    when name_eq cst cst' && nargs == List.length s + 2 ->
    Some ( (mk_state st.ctx st.term (flattenner l cst [t1;t2]))::s)

  | { ctx; term=Const (l,cst); stack } , CConst (nargs,cst',false)
    when name_eq cst cst' && List.length stack == nargs ->
    Some stack

  | { ctx; term=DB (l,x,n); stack }, CDB(nargs,n') ->
      assert ( ctx = LList.nil ); (* no beta in patterns *)
      if n==n' && List.length stack == nargs then Some stack else None

  | { ctx; term=Lam (_,_,_,_) }, CLam ->
    begin
      match term_of_state st with (*TODO could be optimized*)
      | Lam (_,_,_,te) -> Some [state_of_term te]
      | _ -> assert false
    end
  | _ -> None

let rec fetch_case sg (flattenner:loc->name->stack->stack)
                   (state:state) (case:case)
                   (dt_suc:dtree) (dt_def:dtree option) : (dtree*state*stack) list =
  let def_s = match dt_def with None -> [] | Some g -> [(g,state,[])] in
  let stack = state.stack in
  match state.term with
  | Const(l,_) ->
     let rec f acc stack_acc st = match st, case with
       | [], _ -> acc
       | hd::tl, _ ->
          let new_stack_acc = (hd::stack_acc) in
          let new_acc = match find_case flattenner hd case with
            | None   -> acc
            | Some s ->
              let new_stack = List.rev_append stack_acc tl in (* Remove hd from stack *)
              let new_state = mk_state state.ctx state.term new_stack in
              (dt_suc,new_state,s)::acc
          in
          f new_acc new_stack_acc tl
     in
     List.rev_append (f [] [] stack) def_s
   | _ -> assert false


let find_cases (flattenner:loc->name->stack->stack)
    (st:state) (cases:(case * dtree) list)
    (default:dtree option) : (dtree*stack) list =
  List.fold_left
    (fun acc (case, tr) ->
       match find_case flattenner st case with
       | None -> acc
       | Some stack -> (tr,stack) :: acc
    )
    (match default with None -> [] | Some g -> [(g,[])])
    cases


(* Problem conversion *)

let convert_problem stack problem =
  let lazy_stack = List.map (fun s -> lazy (term_of_state s)) stack in
  let lazy_array = Array.of_list lazy_stack in
  let convert i = lazy_array.(i) in
  let convert_ac_sets = function
    | [i] ->
      begin
        match List.nth stack i with
        | {ctx; term=Const(l,cst'); stack=st} ->
          List.map (fun s -> lazy (term_of_state s)) st
        | _ -> assert false
      end
    | _ -> assert false in
  mk_matching_problem convert convert_ac_sets problem


let rec gamma_rw_list (sg:Signature.t) (filter:(Rule.rule_name -> bool) option)
              : (stack*dtree) list -> (rule_name*env*term) option =
  function
  | [] -> None
  | (stack, tree) :: tl ->
     match gamma_rw sg filter stack tree with
     | None -> gamma_rw_list sg filter tl
     | Some _ as x -> x

(* TODO implement the stack as an array ? (the size is known in advance). *)
and gamma_rw (sg:Signature.t) (filter:(Rule.rule_name -> bool) option)
             (stack:stack) : dtree -> (rule_name*env*term) option =
  function
  | Fetch (i,case,dt_suc,dt_def) -> (* Fetch case from AC-headed i-th state *)
    let rec split_ith acc i l = match i,l with
      | 0, h::t -> (acc,h,t)
      | i, h::t -> split_ith (h::acc) (i-1) t
      | _ -> assert false
    in
    let (stack_h, arg_i, stack_t) = split_ith [] i stack in
    assert (match arg_i.term with Const(l,cst) -> Signature.is_AC sg l cst | _ -> false);
    let process (g, new_s, s) =
      List.rev_append stack_h
        (new_s :: match s with [] -> stack_t | s  -> stack_t@s), g in
    let cases =  (* Generate all possible pick for the fetch... *)
      fetch_case sg (flatten_AC_stack sg state_whnf) arg_i case dt_suc dt_def in
    let new_cases = List.map process cases in
    gamma_rw_list sg filter new_cases (* ... try them all *)
  | ACEmpty (i, dt_suc, dt_def) ->
    begin
      match List.nth stack i with
      | {ctx; term=Const(l,cst); stack=st} ->
        begin
          assert (Signature.is_AC sg l cst);
          if st = []
          then gamma_rw sg filter stack dt_suc
          else bind_opt (gamma_rw sg filter stack) dt_def
        end
      | _ -> assert false
    end
  | Switch (i,cases,def) ->
    let arg_i = state_whnf sg (List.nth stack i) in
    let new_cases =
      List.map
        (fun (g,l) -> ( (match l with |[] -> stack | s -> stack@s), g))
        (find_cases (flatten_AC_stack sg state_whnf) arg_i cases def) in
    gamma_rw_list sg filter new_cases
  | Test (rule_name, problem, cstr, right, def) ->
    let pb = convert_problem stack problem in (* Convert problem with the stack *)
    begin
      match solve_problem (snf sg) (C.are_convertible sg) (snf sg) pb with
      | None ->
        bind_opt (gamma_rw sg filter stack) def
      | Some subst ->
        begin
          let aux e acc = match e with None -> assert false | Some e -> e :: acc in
          let ctx_list = Array.fold_right aux subst [] in
          let ctx = LList.of_list ctx_list in
          List.iter
            (fun (i,t2) ->
               let t1 = Lazy.force (LList.nth ctx i) in
               let t2 = term_of_state (mk_state ctx t2 []) in
               if not (C.are_convertible sg t1 t2)
               then raise (Signature.SignatureError
                             (Signature.GuardNotSatisfied(get_loc t1, t1, t2))))
              cstr;
          Some (rule_name, ctx, right)
        end
    end

(* ************************************************************** *)

(* This function reduces a state to a weak-head-normal form.
 * This means that the term [term_of_state (state_whnf sg state)] is a
 * weak-head-normal reduct of [term_of_state state].
 *
 * Moreover the returned state verifies the following properties:
 * - state.term is not an application
 * - state.term can only be a variable if term.ctx is empty
 *    (and therefore this variable is free in the corresponding term)
 * - when state.term is an AC constant, then state.stack contains no application
 *     of that same constant
 * *)
and state_whnf (sg:Signature.t) (st:state) : state =
  match !(st.reduc) with
  | (true, st') -> st'
  | (false, st') when st' != st ->
    let _ = Debug.(debug D_reduce "Jumping %a ---> %a" simpl_pp_state st simpl_pp_state st') in
    state_whnf sg st'
  | _ ->
    let _ = Debug.(debug D_reduce "Reducing %a" simpl_pp_state st) in
    let rec_call c t s = state_whnf sg (mk_reduc_state st c t s) in
  match st with
  (* Weak heah beta normal terms *)
  | { term=Type _ } | { term=Kind }
  | { term=Pi _   } | { term=Lam _; stack=[] } -> as_final st
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if LList.is_empty ctx then as_final st
    else if n < LList.len ctx
    then rec_call LList.nil (Lazy.force (LList.nth ctx n)) stack
    else as_final (mk_reduc_state st LList.nil (mk_DB l x (n-LList.len ctx)) stack)
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    if not !beta then as_final st
    else rec_call (LList.cons (lazy (term_of_state p)) ctx) t s
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map (fun t -> mk_state ctx t []) (a::lst) in
    rec_call ctx f (List.rev_append tl' s)
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,n); stack } ->
    let trees = Signature.get_dtree sg l n in
    match find_dtree (List.length stack) trees with
    | None -> as_final (comb_state_shape_if_AC sg state_whnf st)
    | Some (ar, tree) ->
      let s1, s2 = split ar stack in
      let s1 =
        if Signature.is_AC sg l n && ar > 1
        then
          match s1 with
          | t1 :: t2 :: tl ->
            let flat = flatten_AC_stack sg state_whnf l n [t1;t2] in
            (mk_state ctx (mk_Const l n) flat):: tl
          | _ -> assert false
        else s1 in
      match gamma_rw sg !selection s1 tree with
      | None -> as_final (comb_state_shape_if_AC sg state_whnf st)
      | Some (_,ctx,term) -> rec_call ctx term s2

(* ************************************************************** *)

(* Weak Head Normal Form *)
and whnf sg term = term_of_state ( state_whnf sg (state_of_term term) )

(* Strong Normal Form *)
and snf sg (t:term) : term =
  match whnf sg t with
  | Kind | Const _ | DB _ | Type _ as t' -> t'
  | App (f,a,lst) ->
     let res = mk_App (snf sg f) (snf sg a) (List.map (snf sg) lst) in
     comb_term_shape_if_AC sg res
  | Pi (_,x,a,b) -> mk_Pi dloc x (snf sg a) (snf sg b)
  | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (snf sg) a) (snf sg b)

and conversion_step sg : (term * term) -> (term * term) list -> (term * term) list = fun (l,r) lst ->
  match l,r with
  | Kind, Kind | Type _, Type _                 -> lst
  | Const (_,n), Const (_,n') when name_eq n n' -> lst
  | DB (_,_,n) , DB (_,_,n')  when n == n'      -> lst
  | App (Const(lc,cst), _, _), App (Const(lc',cst'), _, _)
    when Signature.is_AC sg lc cst && name_eq cst cst' ->
    begin
      (* TODO: Replace this with less hardcore criteria: put all terms in whnf
               * then look at the heads to match arguments with one another. *)
      match snf sg l, snf sg r with
      | App (Const(l ,cst2 ), a , args ),
        App (Const(l',cst2'), a', args')
        when name_eq cst2 cst  && name_eq cst2' cst &&
             name_eq cst2 cst' && name_eq cst2' cst' ->
        (a,a') :: (zip_lists args args' lst)
      | p -> p :: lst
    end
  | App (f,a,args), App (f',a',args') ->
     (f,f') :: (a,a') :: (zip_lists args args' lst)
  | Lam (_,_,_,b), Lam (_,_,_ ,b') -> (b,b')::lst
  (* Potentially eta-equivalent terms *)
  | Lam (_,i,_,b), a when !eta ->
    let b' = mk_App (Subst.shift 1 a) (mk_DB dloc i 0) [] in
    (b,b')::lst
  | a, Lam (_,i,_,b) when !eta ->
    let b' = mk_App (Subst.shift 1 a) (mk_DB dloc i 0) [] in
    (b,b')::lst
  | Pi  (_,_,a,b), Pi  (_,_,a',b') -> (a,a') :: (b,b') :: lst
  | t1, t2 -> begin
      Debug.(debug D_reduce "Not convertible: %a / %a" pp_term t1 pp_term t2 );
      raise Not_convertible end

let rec are_convertible_lst sg : (term*term) list -> bool =
  function
  | [] -> true
  | (t1,t2)::lst -> are_convertible_lst sg
    (if term_eq t1 t2 then lst
     else conversion_step sg (whnf sg t1, whnf sg t2) lst)

(* Convertibility Test *)
let are_convertible sg t1 t2 =
  try are_convertible_lst sg [(t1,t2)]
  with Not_convertible | Invalid_argument _ -> false

let matching_test _ _ = are_convertible

(* ************************************************************** *)

type state_reducer = position -> state -> state
type  term_reducer = position -> term  -> term

let logged_state_whnf log stop (strat:red_strategy) (sg:Signature.t) : state_reducer =
  let rec aux : state_reducer = fun (pos:position) (st:state) ->
    if stop () then st else
      match st, strat with
      (* Weak heah beta normal terms *)
      | { term=Type _ }, _
      | { term=Kind   }, _ -> st

      | { term=Pi _ }  , ByName
      | { term=Pi _ }  , ByValue -> st
      | { ctx=ctx; term=Pi(l,x,a,b) }, ByStrongValue ->
        let a' = term_of_state (aux (0::pos) (mk_state ctx a[])) in
        (* Should we also reduce b ? *)
        {st with term=mk_Pi l x a' b }

      (* Reducing type annotation *)
      | { ctx; term=Lam (l,x,Some ty,t); stack=[] }, ByStrongValue ->
        let ty' = term_of_state (aux (0::pos) (mk_state ctx ty [])) in
        {st with term=mk_Lam l x (Some ty') t}
      (* Empty stack *)
      | { term=Lam _; stack=[] }, _ -> st
      (* Beta redex with type annotation *)
      | { ctx; term=Lam (l,x,Some ty,t); stack=p::s }, ByStrongValue ->
        let ty' = term_of_state (aux (0::pos) (mk_state ctx ty [])) in
        if stop () || not !beta then {st with term=mk_Lam l x (Some ty') t}
        else
          let st' = mk_state (LList.cons (lazy (term_of_state p)) ctx) t s in
          let _ = log pos Rule.Beta st st' in
          aux pos st'
      (* Beta redex *)
      | { ctx; term=Lam (_,_,_,t); stack=p::s }, _ ->
        if not !beta then st
        else
          let st' = mk_state (LList.cons (lazy (term_of_state p)) ctx) t s in
          let _ = log pos Rule.Beta st st' in
          aux pos st'

      (* DeBruijn index: environment lookup *)
      | { ctx; term=DB (l,x,n); stack }, _ ->
        if n < LList.len ctx
        then aux pos (mk_state LList.nil (Lazy.force (LList.nth ctx n)) stack)
        else mk_state LList.nil (mk_DB l x (n-LList.len ctx)) stack

      (* Application: arguments go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s }, ByName ->
        (* rev_map + rev_append to avoid map + append *)
        let tl' = List.rev_map (fun t -> mk_state ctx t []) (a::lst) in
        aux pos (mk_state ctx f (List.rev_append tl' s))

      (* Application: arguments are reduced to values then go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s }, _ ->
        let tl' = rev_mapi (fun i t -> aux (i::pos) (mk_state ctx t []) ) (a::lst) in
        aux pos (mk_state ctx f (List.rev_append tl' s))

      (* Potential Gamma redex *)
      | { ctx; term=Const (l,n); stack }, _ ->
        let trees = Signature.get_dtree sg l n in
        match find_dtree (List.length stack) trees with
        | None -> st
        | Some (ar, tree) ->
          let s1, s2 = split ar stack in
          match gamma_rw sg !selection s1 tree with
          | None -> st
          | Some (rn,ctx,term) ->
            let st' = mk_state ctx term s2 in
            log pos rn st st';
            aux pos st'
  in aux

let term_whnf (st_reducer:state_reducer) : term_reducer =
  fun pos t -> term_of_state (st_reducer pos (state_of_term t))

let term_snf (st_reducer:state_reducer) : term_reducer =
  let rec aux pos t =
    match term_whnf st_reducer pos t with
    | Kind | Const _ | DB _ | Type _ as t' -> t'
    | App (f,a,lst) ->
      mk_App (aux (0::pos) f) (aux (1::pos) a)
        (List.mapi (fun p arg -> aux (p::pos) arg) lst)
    | Pi  (_,x,a,b) -> mk_Pi dloc x (aux (0::pos) a) (aux (1::pos) b)
    | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (aux (0::pos)) a) (aux (1::pos) b)
  in aux

let reduction cfg sg te =
  let log, stop =
    match cfg.nb_steps with
    | None   -> (fun _ _ _ _ -> ()), (fun () -> false)
    | Some n ->
      let aux = ref n in
      (fun _ _ _ _-> decr aux), (fun () -> !aux <= 0)
  in
  let st_logger = fun p rn stb sta -> log p rn stb sta;
    cfg.logger p rn (lazy (term_of_state stb)) (lazy (term_of_state sta)) in
  let st_red = logged_state_whnf st_logger stop cfg.strat sg in
  let term_red = match cfg.target with Snf -> term_snf | Whnf -> term_whnf in
  selection := cfg.select;
  beta      := cfg.beta;
  let te' = term_red st_red [] te in
  selection := default_cfg.select;
  beta      := default_cfg.beta;
  te'

  let are_convertible = are_convertible
  let matching_test _ _ = are_convertible
end

module rec Default : S = Make(Default)
