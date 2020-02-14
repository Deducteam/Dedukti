open Basic
open Rule
open Term
open Dtree

let d_reduce = Debug.register_flag "Reduce"

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


(**************** Pretty Printing ****************)

(*
open Format

let pp_env fmt (env:env) = pp_list ", " pp_term fmt (List.map Lazy.force (LList.lst env))
let pp_stack fmt (st:stack) =
  fprintf fmt "[ %a ]\n" (pp_list "\n | " pp_term) (List.map term_of_state_ref st)

let pp_stack_oneline fmt (st:stack) =
  fprintf fmt "[ %a ]" (pp_list " | " pp_term) (List.map term_of_state_ref st)

let pp_state ?(if_ctx=true) ?(if_stack=true) fmt { ctx; term; stack } =
  if if_ctx
  then fprintf fmt "{ctx=[%a];@." pp_env ctx
  else fprintf fmt "{ctx=[...](%i);@." (LList.len ctx);
  fprintf fmt "term=%a;@." pp_term term;
  if if_stack
  then fprintf fmt "stack=%a}@." pp_stack stack
  else fprintf fmt "stack=[...](%i)}@." (List.length stack);
  fprintf fmt "@.%a@." pp_term (term_of_state {ctx; term; stack})

let pp_state_oneline = pp_state ~if_ctx:true ~if_stack:true
*)

type matching_test = Rule.constr -> Rule.rule_name -> Signature.t -> term -> term -> bool
type convertibility_test = Signature.t -> term -> term -> bool

module type ConvChecker = sig
  val are_convertible : convertibility_test
  val matching_test   : matching_test
  val conversion_step : term * term -> (term * term) list -> (term * term) list
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

let rec get_context (sg:Signature.t) (stack:stack)
                    (mp:matching_problem) : env option =
  let aux ({pos;depth;args_db}:atomic_problem) : term Lazy.t =
    let st = List.nth stack pos in
    if depth = 0 then lazy (term_of_state_ref st) (* First order matching *)
    else
      let te = term_of_state_ref st in
      Lazy.from_val
        (try Matching.solve depth args_db te
         with Matching.NotUnifiable | Subst.UnshiftExn ->
             Matching.solve depth args_db (snf sg te))
  in
  try Some (LList.map aux mp)
  with Matching.NotUnifiable | Subst.UnshiftExn -> None

and test (rn:Rule.rule_name) (sg:Signature.t)
         (ctx:env) (constrs: constr list) : bool  =
  match constrs with
  | [] -> true
  | Linearity (i,j)::tl ->
     let t1 = Lazy.force (LList.nth ctx i) in
     let t2 = Lazy.force (LList.nth ctx j) in
     if C.matching_test (Linearity (i,j)) rn sg t1 t2
     then test rn sg ctx tl
     else false
  | Bracket (i,t)::tl ->
     let t1 = Lazy.force (LList.nth ctx i) in
     let t2 = term_of_state { ctx; term=t; stack=[] } in
     if C.matching_test (Bracket (i,t)) rn sg t1 t2
     then test rn sg ctx tl
     else raise (Signature.Signature_error(Signature.GuardNotSatisfied(get_loc t1, t1, t2)))

and find_case (st:state) (cases:(case*dtree) list)
    (default:dtree option) : (dtree * stack) option =
  match st, cases with
  | _, [] -> map_opt (fun g -> (g,[])) default
  | { term=Const (_,cst); stack ; _ } , (CConst (nargs,cst'),tr)::tl ->
     (* The case doesn't match if the identifiers differ or the stack is not
      * of the expected size. *)
     if name_eq cst cst' && List.length stack == nargs
     then Some (tr,stack)
     else find_case st tl default
  | { ctx; term=DB (_,_,n); stack } , (CDB (nargs,n'),tr)::tl ->
    assert ( ctx = LList.nil ); (* no beta in patterns *)
    (* The case doesn't match if the DB indices differ or the stack is not
     * of the expected size. *)
    if n == n' && List.length stack == nargs
    then Some (tr,stack)
    else find_case st tl default
  | { term=Lam _; _ } , ( CLam , tr )::_ ->
    begin
      match term_of_state st with (*TODO could be optimized*)
      | Lam (_,_,_,te) ->
        Some ( tr , [ ref { ctx=LList.nil; term=te; stack=[] }] )
      | _ -> assert false
    end
  | _, _::tl -> find_case st tl default



(* TODO: implement the stack as an array ? (the size is known in advance).*)
and gamma_rw (sg:Signature.t) (filter:(Rule.rule_name -> bool) option)
                  : stack -> dtree -> (rule_name*env*term) option =
  let rec rw stack = function
    | Switch (i,cases,def) ->
      let arg_i = List.nth stack i in
      arg_i := state_whnf sg !arg_i;
      bind_opt (fun (g,s) -> rw (concat stack s) g) (find_case !arg_i cases def)
      (* This line highly depends on how the module dtree works.
       * When a column is specialized, the dtree makes the assumption
       * that new columns are pushed at the end of the stack
       * which is why s is added at the end. *)
    | Test (rule_name, matching_pb, eqs, right, def) ->
      let keep_rule =
        match filter with
        | None -> true
        | Some f -> f rule_name
      in
      if keep_rule then
        match get_context sg stack matching_pb with
        | None -> bind_opt (rw stack) def
        | Some ctx ->
          if test rule_name sg ctx eqs
          then Some (rule_name, ctx, right)
          else bind_opt (rw stack) def
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
 *)
and state_whnf (sg:Signature.t) (st:state) : state =
  (*
  Debug.(debug D_reduce "Reducing %a" pp_state_oneline st);
  *)
  match st with
  (* Weak head beta normal terms *)
  | { term=Type _ ; _ } | { term=Kind ; _ }
  | { term=Pi   _ ; _ } | { term=Lam _; stack=[] ; _ } -> st
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if LList.is_empty ctx then st
    else if n < LList.len ctx
    then state_whnf sg { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    else { ctx=LList.nil; term=mk_DB l x (n-LList.len ctx); stack }
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    if not !beta then st
    else state_whnf sg { ctx=LList.cons (lazy (term_of_state_ref p)) ctx; term=t; stack=s }
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map (fun t -> ref {ctx;term=t;stack=[]}) (a::lst) in
    state_whnf sg { ctx; term=f; stack=List.rev_append tl' s }
  (* Potential Gamma redex *)
  | { term=Const (l,n); stack ; _} ->
    let trees = Signature.get_dtree sg l n in
    match find_dtree (List.length stack) trees with
    | None -> st
    | Some (ar, tree) ->
      let s1, s2 = split ar stack in
      match gamma_rw sg !selection s1 tree with
      | None -> st
      | Some (_,ctx,term) -> state_whnf sg { ctx; term; stack=s2 }

(* ************************************************************** *)

(* Weak Head Normal Form *)
and whnf sg term = term_of_state ( state_whnf sg { ctx=LList.nil; term; stack=[] } )

(* Strong Normal Form *)
and snf sg (t:term) : term =
  match whnf sg t with
  | Kind | Const _ | DB _ | Type _ as t' -> t'
  | App (f,a,lst) -> mk_App (snf sg f) (snf sg a) (List.map (snf sg) lst)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (snf sg a) (snf sg b)
  | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (snf sg) a) (snf sg b)

let conversion_step : (term * term) -> (term * term) list -> (term * term) list =
  fun (l,r) lst ->
  match l,r with
  | Kind, Kind | Type _, Type _                 -> lst
  | Const (_,n), Const (_,n') when name_eq n n' -> lst
  | DB (_,_,n) , DB (_,_,n')  when n == n'      -> lst
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
  | t1, t2 ->
    Debug.(debug d_reduce "Not convertible: %a / %a" pp_term t1 pp_term t2 );
    raise NotConvertible

let rec are_convertible_lst sg : (term*term) list -> bool =
  function
  | [] -> true
  | (t1,t2)::lst -> are_convertible_lst sg
    (if term_eq t1 t2 then lst
     else conversion_step (whnf sg t1, whnf sg t2) lst)

(* Convertibility Test *)
let are_convertible sg t1 t2 =
  try are_convertible_lst sg [(t1,t2)]
  with NotConvertible -> false

(* ************************************************************** *)

type state_reducer = position -> state -> state
type  term_reducer = position -> term  -> term

let logged_state_whnf log stop (strat:red_strategy) (sg:Signature.t) : state_reducer =
  let rec aux : state_reducer = fun (pos:position) (st:state) ->
    if stop () then st else
      match st, strat with
      (* Weak head beta normal terms *)
      | { term=Type _ ; _ }, _
      | { term=Kind   ; _ }, _ -> st

      | { term=Pi _   ; _ }, ByName
      | { term=Pi _   ; _ }, ByValue -> st
      | { ctx=ctx; term=Pi(l,x,a,b) ; _ }, ByStrongValue ->
        let a' = term_of_state (aux (0::pos) {ctx=ctx; term=a; stack=[]}) in
        (* Should we also reduce b ? *)
        {st with term=mk_Pi l x a' b }

      (* Reducing type annotation *)
      | { ctx; term=Lam (l,x,Some ty,t); stack=[] }, ByStrongValue ->
        let ty' = term_of_state (aux (0::pos) {ctx=ctx; term=ty; stack=[]}) in
        {st with term=mk_Lam l x (Some ty') t}
      (* Empty stack *)
      | { term=Lam _; stack=[] ; _ }, _ -> st
      (* Beta redex with type annotation *)
      | { ctx; term=Lam (l,x,Some ty,t); stack=p::s }, ByStrongValue ->
        let ty' = term_of_state (aux (0::pos) {ctx=ctx; term=ty; stack=[]}) in
        if stop () || not !beta then {st with term=mk_Lam l x (Some ty') t}
        else
          let st' = { ctx=LList.cons (lazy (term_of_state_ref p)) ctx; term=t; stack=s } in
          let _ = log pos Rule.Beta st st' in
          aux pos st'
      (* Beta redex *)
      | { ctx; term=Lam (_,_,_,t); stack=p::s }, _ ->
        if not !beta then st
        else
          let st' = { ctx=LList.cons (lazy (term_of_state_ref p)) ctx; term=t; stack=s } in
          let _ = log pos Rule.Beta st st' in
          aux pos st'

      (* DeBruijn index: environment lookup *)
      | { ctx; term=DB (l,x,n); stack }, _ ->
        if n < LList.len ctx
        then aux pos { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
        else { ctx=LList.nil; term=(mk_DB l x (n-LList.len ctx)); stack }

      (* Application: arguments go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s }, ByName ->
        (* rev_map + rev_append to avoid map + append *)
        let tl' = List.rev_map ( fun t -> ref {ctx;term=t;stack=[]} ) (a::lst) in
        aux pos { ctx; term=f; stack=List.rev_append tl' s }

      (* Application: arguments are reduced to values then go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s }, _ ->
        let tl' = rev_mapi ( fun i t -> ref (aux (i::pos) {ctx;term=t;stack=[]}) ) (a::lst) in
        aux pos { ctx; term=f; stack=List.rev_append tl' s }

      (* Potential Gamma redex *)
      | { term=Const (l,n); stack ; _ }, _ ->
        let trees = Signature.get_dtree sg l n in
        match find_dtree (List.length stack) trees with
        | None -> st
        | Some (ar, tree) ->
           let s1, s2 = split ar stack in
           match gamma_rw sg !selection s1 tree with
           | None -> st
           | Some (rn,ctx,term) ->
              let st' = { ctx; term; stack=s2 } in
              log pos rn st st';
              aux pos st'
  in aux

let term_whnf (st_reducer:state_reducer) : term_reducer =
  fun pos t -> term_of_state (st_reducer pos { ctx=LList.nil; term=t; stack=[] } )

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
