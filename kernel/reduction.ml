open Basic
open Rule
open Term
open Dtree
open Ac
open Format

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
type state =
  {
    ctx   : env;    (* context *)
    term  : term;   (* term to reduce *)
    stack : stack;  (* stack *)
  }
and stack = state ref list
(* TODO: implement  constant time random access / in place mutable value.  *)

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx term ) in
  mk_App2 t (List.map term_of_state_ref stack)

and term_of_state_ref r = term_of_state !r

let mk_state ctx term stack = { ctx; term; stack }

let mk_state_ref ctx term stack = ref (mk_state ctx term stack)

let state_of_term t = mk_state LList.nil t []

let state_ref_of_term t = mk_state_ref LList.nil t []

(**************** Pretty Printing ****************)

let pp_env fmt (env:env) = pp_list ", " pp_term fmt (List.map Lazy.force (LList.lst env))

let pp_stack fmt (st:stack) =
  fprintf fmt "[ %a ]\n" (pp_list "\n | " pp_term) (List.map term_of_state_ref st)

(* let pp_stack_oneline fmt (st:stack) =
 *   fprintf fmt "[ %a ]" (pp_list " | " pp_term) (List.map term_of_state_ref st) *)

let pp_state ?(if_ctx=true) ?(if_stack=true) fmt { ctx; term; stack ; _ } =
  if if_ctx
  then fprintf fmt "{ctx=[%a];@." pp_env ctx
  else fprintf fmt "{ctx=[...](%i);@." (LList.len ctx);
  fprintf fmt "term=%a;@." pp_term term;
  if if_stack
  then fprintf fmt "stack=%a}@." pp_stack stack
  else fprintf fmt "stack=[...](%i)}@." (List.length stack)

let pp_state_oneline = pp_state ~if_ctx:true ~if_stack:true

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

let filter_neutral sg l cst terms =
  match Signature.get_algebra sg l cst with
  | ACU neu ->
    (match List.filter (fun x -> not (C.are_convertible sg neu x)) terms with
     | [] -> [neu] | s -> s)
  | _ -> terms

(** Builds a comb-shaped AC term from a list of arguments. *)
let to_comb sg l cst ctx stack =
  let rec f = function
    | []         -> { ctx=LList.nil; term=Signature.get_neutral sg l cst; stack=[] }
    | [t]        -> !t
    | t1::t2::tl -> f (ref {ctx;term=mk_Const l cst;stack=[t1;t2]} :: tl)
  in
  f stack

(* Unfolds all occurences of the AC(U) symbol in the stack
 * Removes occurence of neutral element.  *)
let rec flatten_AC_stack sg (l:loc) (cst:name) (stack:stack) : stack =
  let rec flatten acc = function
    | [] -> acc
    | st :: tl ->
      match !st with
      | { term=Const (_,cst'); stack=[st1;st2] ; _ } when name_eq cst cst' ->
        flatten acc (st1 :: st2 :: tl)
      | _ ->
        st := state_whnf sg !st;
        match !st with
        | { term=Const (_,cst'); stack=[st1;st2] ; _ } when name_eq cst cst' ->
          flatten acc (st1 :: st2 :: tl)
        | _ -> flatten (st::acc) tl
  in
  let stack = flatten [] stack in
  match Signature.get_algebra sg l cst with
  | ACU neu -> List.filter (fun st -> not (C.are_convertible sg (term_of_state_ref st) neu)) stack
  | _ -> stack

and comb_state_shape_if_AC sg st =
  match st with
  | { ctx; term=Const (l,cst); stack=(s1::s2::rstack) ; _ } when Signature.is_AC sg l cst ->
    let nstack = flatten_AC_stack sg l cst [s1;s2] in
    let combed = to_comb sg l cst ctx nstack in
    let fstack = match rstack with [] -> combed.stack | l -> combed.stack@l in
    { combed with stack = fstack }
  | st -> st

and comb_term_shape_if_AC sg : term -> term = function
  | App(Const (l,cst), a1, a2::remain_args) when Signature.is_AC sg l cst ->
     let id_comp = Signature.get_id_comparator sg in
     let args = flatten_AC_terms cst [a1;a2] in
     let args = filter_neutral sg l cst args in
     let args = List.sort (compare_term id_comp) args in
     let _ = assert (List.length args > 0) in
     mk_App2
       (unflatten_AC (cst, Signature.get_algebra sg l cst) args)
       remain_args
  (* mk_App2 (unflatten sg l cst args) remain_args *)
  | t -> t

and find_case sg
    (st:state) (case:case) : stack option =
  match st, case with
  (* This case is a bit tricky: when + is AC,
     C (+ f g 1) can match C (h 1)
     The corresponding matching problem is  +{f,g} = +{h}
     which is not necessarily unsolvable in general:
     maybe + is acu and a solution is {f = u, g = h}
     TODO: check that this case is used properly !
  *)
  | { ctx; term; stack } , CConst (nargs,cst,true)
    when List.length stack == nargs - 2 -> (* should we also check for the type of t ? *)
    let new_st = ref {ctx;term;stack=[]} in
    let new_stack = flatten_AC_stack sg dloc cst [new_st] in
    Some ( (mk_state_ref ctx (mk_Const dloc cst) new_stack) :: stack)

  | { term=Const (l,cst); stack=t1::t2::s ; _ } , CConst (nargs,cst',true)
    when name_eq cst cst' && nargs == List.length s + 2 ->
    Some ( (mk_state_ref st.ctx st.term (flatten_AC_stack sg l cst [t1;t2]))::s)

  | { term=Const (_,cst); stack ; _ } , CConst (nargs,cst',false) ->
    if name_eq cst cst' && List.length stack == nargs
    then Some stack
    else None

  | { ctx; term=DB (_,_,n); stack ; _ }, CDB(nargs,n') ->
      assert ( ctx = LList.nil ); (* no beta in patterns *)
      if n==n' && List.length stack == nargs then Some stack else None

  | { term=Lam (_,_,_,_) ; _ }, CLam ->
    begin
      match term_of_state st with (*TODO could be optimized*)
      | Lam (_,_,_,te) -> Some [state_ref_of_term te]
      | _ -> assert false
    end
  | _ -> None

and fetch_case sg (state:state ref) (case:case)
               (dt_suc:dtree) (dt_def:dtree option) : (dtree*state ref*stack) list =
  let def_s = match dt_def with None -> [] | Some g -> [(g,state,[])] in
  let stack = !state.stack in
  match !state.term with
  | Const _ ->
     let rec f acc (stack_acc:state ref list)  st = match st, case with
       | [], _ -> acc
       | hd::tl, _ ->
          let new_stack_acc = (hd::stack_acc) in
          let new_acc = match find_case sg !hd case with
            | None   -> acc
            | Some s ->
              let new_stack = List.rev_append stack_acc tl in (* Remove hd from stack *)
              let new_state = mk_state_ref !state.ctx !state.term new_stack in
              (dt_suc,new_state,s)::acc
          in
          f new_acc new_stack_acc tl
     in
     List.rev_append (f [] [] stack) def_s
   | _ -> assert false

and find_cases sg
    (st:state) (cases:(case * dtree) list)
    (default:dtree option) : (dtree*stack) list =
  List.fold_left
    (fun acc (case, tr) ->
       match find_case sg st case with
       | None -> acc
       | Some stack -> (tr,stack) :: acc
    )
    (match default with None -> [] | Some g -> [(g,[])])
    cases


(* Problem conversion *)

and convert_problem (stack:state ref list) problem =
  let lazy_stack = List.map (fun s -> lazy (term_of_state_ref s)) stack in
  let lazy_array = Array.of_list lazy_stack in
  let convert i = lazy_array.(i) in
  let convert_ac_sets = function
    | [i] ->
      begin
        let arg_i = List.nth stack i in
        match !arg_i with
        | { term=Const _; stack=st ; _ } ->
          List.map (fun s -> lazy (term_of_state_ref s)) st
        | _ -> assert false
      end
    | _ -> assert false in
  Matching.mk_matching_problem convert convert_ac_sets problem



(* TODO implement the stack as an array ? (the size is known in advance). *)
and gamma_rw (sg:Signature.t) (filter:(Rule.rule_name -> bool) option)
  : stack -> dtree -> (rule_name*env*term) option =
  let rec rw_list : (stack*dtree) list -> (rule_name*env*term) option =
    function
    | [] -> None
    | [(stack, tree)] -> rw stack tree
    | (stack, tree) :: tl ->
      match rw stack tree with
      | None -> rw_list tl | x -> x
  and rw (stack:stack) : dtree -> (rule_name*env*term) option =
    function
    (* Fetch case from AC-headed i-th state
       This may branch and generate many case, one for each possible term to fetch
    *)
    | Fetch (i,case,dt_suc,dt_def) ->
      let rec split_ith acc i l = match i,l with
        | 0, h::t -> (acc,h,t)
        | i, h::t -> split_ith (h::acc) (i-1) t
        | _ -> assert false
      in
      let (stack_h, arg_i, stack_t) = split_ith [] i stack in
      assert (match !arg_i.term with Const(l,cst) -> Signature.is_AC sg l cst | _ -> false);
      let process (g, new_s, s) =
        List.rev_append stack_h
          (new_s :: match s with [] -> stack_t | s  -> stack_t@s), g in
      let cases = (* Generate all possible picks for the fetch *)
        fetch_case sg arg_i case dt_suc dt_def in
      let new_cases = List.map process cases in
      rw_list new_cases (* ... try them all *)
    | ACEmpty (i, dt_suc, dt_def) ->
      begin
        match !(List.nth stack i) with
        | { term=Const(l,cst); stack=st ; _ } ->
          begin
            assert (Signature.is_AC sg l cst);
            if st = []
            then rw stack dt_suc
            else bind_opt (rw stack) dt_def
          end
        | _ -> assert false
      end
    | Switch (i,cases,def) ->
      let arg_i = List.nth stack i in
      arg_i := state_whnf sg !arg_i;
      let new_cases =
        List.map
          (fun (g,l) -> (concat stack l, g))
          (find_cases sg !arg_i cases def) in
      rw_list new_cases
    | Test (rule_name, matching_pb, cstr, right, def) ->
      let keep_rule =
        match filter with
        | None -> true
        | Some f -> f rule_name
      in
      if keep_rule then
        let pb = convert_problem stack matching_pb in
        (* Convert problem on stack indices to a problem on terms *)
        begin
          match Matching.solve_problem (snf sg) (C.are_convertible sg) (snf sg) pb with
          | None -> bind_opt (rw stack) def
          | Some subst ->
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
      else bind_opt (rw stack) def
  in rw

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
 *)
and state_whnf (sg:Signature.t) (st:state) : state =
  let _ = Debug.(debug D_reduce "Reducing %a" pp_state_oneline st) in
  let rec_call c t s = state_whnf sg (mk_state c t s) in
  match st with
  (* Weak heah beta normal terms *)
  | { term=Type _ ; _ } | { term=Kind ; _ }
  | { term=Pi   _ ; _ } | { term=Lam _; stack=[] ; _ } -> st
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack ; _ } ->
    if LList.is_empty ctx then st
    else if n < LList.len ctx
    then rec_call LList.nil (Lazy.force (LList.nth ctx n)) stack
    else (mk_state LList.nil (mk_DB l x (n-LList.len ctx)) stack)
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s ; _ } ->
    if not !beta then st
    else rec_call (LList.cons (lazy (term_of_state_ref p)) ctx) t s
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s ; _ } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map (fun t -> mk_state_ref ctx t []) (a::lst) in
    rec_call ctx f (List.rev_append tl' s)
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,n); stack ; _} ->
    let trees = Signature.get_dtree sg l n in
    match find_dtree (List.length stack) trees with
    | None -> comb_state_shape_if_AC sg st
    | Some (ar, tree) ->
      let s1, s2 = split ar stack in
      let s1 =
        if Signature.is_AC sg l n && ar > 1
        then
          match s1 with
          | t1 :: t2 :: tl ->
            let flat = flatten_AC_stack sg l n [t1;t2] in
            (mk_state_ref ctx (mk_Const l n) flat):: tl
          | _ -> assert false
        else s1 in
      match gamma_rw sg !selection s1 tree with
      | None -> (comb_state_shape_if_AC sg st)
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

and conversion_step sg : (term * term) -> (term * term) list -> (term * term) list =
  fun (l,r) lst ->
  match l,r with
  | Kind, Kind | Type _, Type _                 -> lst
  | Const (_,n), Const (_,n') when name_eq n n' -> lst
  | DB (_,_,n) , DB (_,_,n')  when n == n'      -> lst
  | App (Const(lc,cst), _, _), App (Const(_,cst'), _, _)
    when Signature.is_AC sg lc cst && name_eq cst cst' ->
    begin
      (* TODO: Eventually replace this with less hardcore criteria: put all terms in whnf
       * then look at the heads to match arguments with one another.
       * Careful, this is tricky:
       * The whnf would need here to make sure that no reduction may occur at the AC-head.
       * Whenever  max n n --> n,   the whnf of "max a (max a b)" should be "max a b"
       * If not all head reduction are exhausted, then comparing AC argument sets is not enough
       *)
      match snf sg l, snf sg r with
      | App (Const(_, cst2 ), a , args ),
        App (Const(_, cst2'), a', args')
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
      raise NotConvertible end

let rec are_convertible_lst sg : (term*term) list -> bool =
  function
  | [] -> true
  | (t1,t2)::lst -> are_convertible_lst sg
    (if term_eq t1 t2 then lst
     else conversion_step sg (whnf sg t1, whnf sg t2) lst)

(* Convertibility Test *)
let are_convertible sg t1 t2 =
  try are_convertible_lst sg [(t1,t2)]
  with NotConvertible | Invalid_argument _ -> false

(* ************************************************************** *)

type state_reducer = position -> state -> state
type  term_reducer = position -> term  -> term

let logged_state_whnf log stop (strat:red_strategy) (sg:Signature.t) : state_reducer =
  let rec aux : state_reducer = fun (pos:position) (st:state) ->
    if stop () then st else
      match st, strat with
      (* Weak heah beta normal terms *)
      | { term=Type _ ; _ }, _
      | { term=Kind   ; _ }, _ -> st

      | { term=Pi _ ; _ }  , ByName
      | { term=Pi _ ; _ }  , ByValue -> st
      | { ctx=ctx; term=Pi(l,x,a,b) ; _ }, ByStrongValue ->
        let a' = term_of_state (aux (0::pos) (mk_state ctx a[])) in
        (* Should we also reduce b ? *)
        {st with term=mk_Pi l x a' b }

      (* Reducing type annotation *)
      | { ctx; term=Lam (l,x,Some ty,t); stack=[] ; _ }, ByStrongValue ->
        let ty' = term_of_state (aux (0::pos) (mk_state ctx ty [])) in
        {st with term=mk_Lam l x (Some ty') t}
      (* Empty stack *)
      | { term=Lam _; stack=[] ; _ }, _ -> st
      (* Beta redex with type annotation *)
      | { ctx; term=Lam (l,x,Some ty,t); stack=p::s ; _ }, ByStrongValue ->
        let ty' = term_of_state (aux (0::pos) (mk_state ctx ty [])) in
        if stop () || not !beta then {st with term=mk_Lam l x (Some ty') t}
        else
          let st' = mk_state (LList.cons (lazy (term_of_state_ref p)) ctx) t s in
          let _ = log pos Rule.Beta st st' in
          aux pos st'
      (* Beta redex *)
      | { ctx; term=Lam (_,_,_,t); stack=p::s ; _ }, _ ->
        if not !beta then st
        else
          let st' = mk_state (LList.cons (lazy (term_of_state_ref p)) ctx) t s in
          let _ = log pos Rule.Beta st st' in
          aux pos st'

      (* DeBruijn index: environment lookup *)
      | { ctx; term=DB (l,x,n); stack ; _ }, _ ->
        if n < LList.len ctx
        then aux pos (mk_state LList.nil (Lazy.force (LList.nth ctx n)) stack)
        else mk_state LList.nil (mk_DB l x (n-LList.len ctx)) stack

      (* Application: arguments go on the stack *)
      | { ctx; term=App (f,a,lst); stack; _ }, ByName ->
        (* rev_map + rev_append to avoid map + append *)
        let tl' = List.rev_map (fun t -> mk_state_ref ctx t []) (a::lst) in
        aux pos (mk_state ctx f (List.rev_append tl' stack))

      (* Application: arguments are reduced to values then go on the stack *)
      | { ctx; term=App (f,a,lst); stack ; _ }, _ ->
        let tl' = rev_mapi (fun i t -> ref (aux (i::pos) (mk_state ctx t []) )) (a::lst) in
        aux pos (mk_state ctx f (List.rev_append tl' stack))

      (* Potential Gamma redex *)
      | { ctx; term=Const (l,n); stack ; _ }, _ ->
        let trees = Signature.get_dtree sg l n in
        match find_dtree (List.length stack) trees with
        | None -> comb_state_shape_if_AC sg st
        | Some (ar, tree) ->
          let s1, s2 = split ar stack in
          let s1 =
            if Signature.is_AC sg l n && ar > 1
            then
              match s1 with
              | t1 :: t2 :: tl ->
                let flat = flatten_AC_stack sg l n [t1;t2] in
                (mk_state_ref ctx (mk_Const l n) flat):: tl
              | _ -> assert false
            else s1 in
          match gamma_rw sg !selection s1 tree with
          | None -> comb_state_shape_if_AC sg st
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
