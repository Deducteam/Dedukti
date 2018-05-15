open Basic
open Format
open Rule
open Term
open Dtree

type red_strategy = Hnf | Snf | Whnf

type red_cfg = {
  select : (rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  strategy : red_strategy;
  beta : bool
}

let pp_red_cfg fmt strat =
  match strat with
  | {strategy=Snf ;nb_steps=None   } -> Format.fprintf fmt "[SNF]"
  | {strategy=Snf ;nb_steps=Some i } -> Format.fprintf fmt "[SNF,%i]" i
  | {strategy=Hnf ;nb_steps=None   } -> Format.fprintf fmt "[HNF]"
  | {strategy=Hnf ;nb_steps=Some i } -> Format.fprintf fmt "[HNF,%i]" i
  | {strategy=Whnf;nb_steps=None   } -> ()
  | {strategy=Whnf;nb_steps=Some i } -> Format.fprintf fmt "[%i]" i

let default_cfg = { select = None ; nb_steps = None ; strategy = Snf ; beta = true }

let selection  = ref None

let beta = ref true

let select f b : unit =
  selection := f;
  beta := b

(* State *)

type env = term Lazy.t LList.t

(* A state {ctx; term; stack} is the state of an abstract machine that
represents a term where [ctx] is a ctx that contains the free variables
of [term] and [stack] represents the terms that [term] is applied to. *)
type state = {
  ctx:env;        (*context*)
  term : term;    (*term to reduce*)
  stack : stack;  (*stack*)
}
and stack = state list

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx term ) in
  match stack with
  | [] -> t
  | a::lst -> mk_App t (term_of_state a) (List.map term_of_state lst)

(* Pretty Printing *)

let pp_state fmt st =
  fprintf fmt "{ctx} {%a} {stack[%i]}\n" pp_term st.term (List.length st.stack)

let pp_stack fmt stck =
  fprintf fmt "[\n";
  List.iter (pp_state fmt) stck;
  fprintf fmt "]\n"

let pp_env fmt (ctx:env) =
  let pp_lazy_term out lt = pp_term fmt (Lazy.force lt) in
    pp_list ", " pp_lazy_term fmt (LList.lst ctx)

let pp_stack fmt (st:stack) =
  let aux fmt state =
    pp_term fmt (term_of_state state)
  in
    fprintf fmt "[ %a ]\n" (pp_list "\n | " aux) st

let pp_state ?(if_ctx=true) ?(if_stack=true) fmt { ctx; term; stack } =
  if if_ctx
  then fprintf fmt "{ctx=[%a];@." pp_env ctx
  else fprintf fmt "{ctx=[...](%i);@." (LList.len ctx);
  fprintf fmt "term=%a;@." pp_term term;
  if if_stack
  then fprintf fmt "stack=%a}@." pp_stack stack
  else fprintf fmt "stack=[...]}@.";
  fprintf fmt "@.%a@." pp_term (term_of_state {ctx; term; stack})

(* ********************* *)

type rw_strategy = Signature.t -> term -> term

type rw_state_strategy = Signature.t -> state -> state

type convertibility_test = Signature.t -> term -> term -> bool


module type Controller =
sig
  (** This allows/forbids the use of given rule on given state. *)
  val use_rule : Signature.t -> state -> rule_name -> bool
  val use_beta : Signature.t -> state -> bool
  val rule_filter : unit -> (rule_name -> bool) option
  val beta_filter : unit -> bool
end

module type GammaRW =
sig
  val gamma_rw : Signature.t -> stack -> dtree -> (rule_name*env*term) option
end

module type StateReducer =
sig
  val state_whnf : Signature.t -> state -> state
end

module type Reducer =
sig
  val whnf : Signature.t -> term -> term
  val snf  : Signature.t -> term -> term
  val hnf  : Signature.t -> term -> term
  val are_convertible : Signature.t -> term -> term -> bool
end

(******************************************************************************)

module MkRewriter (SR : StateReducer) (R : Reducer) : GammaRW  = struct

let solve (sg:Signature.t) (depth:int) (pbs:int LList.t) (te:term) : term =
  try Matching.solve depth pbs te
  with Matching.NotUnifiable ->
    Matching.solve depth pbs (R.snf sg te)

let unshift (sg:Signature.t) (q:int) (te:term) =
  try Subst.unshift q te
  with Subst.UnshiftExn ->
    Subst.unshift q (R.snf sg te)

let get_context_syn (sg:Signature.t) (stack:stack) (ord:arg_pos LList.t) : env option =
  try Some (LList.map (
      fun p ->
        if ( p.depth = 0 )
        then lazy (term_of_state (List.nth stack p.position) )
        else
          Lazy.from_val
            (unshift sg p.depth (term_of_state (List.nth stack p.position) ))
    ) ord )
  with Subst.UnshiftExn -> ( None )

let get_context_mp (sg:Signature.t) (stack:stack)
                   (pb_lst:abstract_problem LList.t) : env option =
  let aux ((pos,dbs):abstract_problem) : term Lazy.t =
    let res = solve sg pos.depth dbs (term_of_state (List.nth stack pos.position)) in
    Lazy.from_val (Subst.unshift pos.depth res)
  in
  try Some (LList.map aux pb_lst)
  with Matching.NotUnifiable -> None
     | Subst.UnshiftExn -> assert false

let rec test (sg:Signature.t) (ctx:env) (constrs: constr list) : bool =
  match constrs with
  | [] -> true
  | (Linearity (i,j))::tl ->
    let t1 = mk_DB dloc dmark i in
    let t2 = mk_DB dloc dmark j in
    if R.are_convertible sg (term_of_state { ctx; term=t1; stack=[] })
                      (term_of_state { ctx; term=t2; stack=[] })
    then test sg ctx tl
    else false
  | (Bracket (i,t))::tl ->
    let t1 = Lazy.force (LList.nth ctx i) in
    let t2 = term_of_state { ctx; term=t; stack=[] } in
    if R.are_convertible sg t1 t2
    then test sg ctx tl
    else
      (*FIXME: if a guard is not satisfied should we fail or only warn the user? *)
      raise (Signature.SignatureError( Signature.GuardNotSatisfied(get_loc t1, t1, t2) ))

let rec find_case (st:state) (cases:(case * dtree) list)
                  (default:dtree option) : (dtree*state list) option =
  match st, cases with
  | _, [] -> map_opt (fun g -> (g,[])) default
  | { term=Const (_,cst); stack } , (CConst (nargs,cst'),tr)::tl ->
     (* The case doesn't match if the identifiers differ or the stack is not
      * of the expected size. *)
     if name_eq cst cst' && List.length stack == nargs
     then Some (tr,stack)
     else find_case st tl default
  | { ctx; term=DB (l,x,n); stack } , (CDB (nargs,n'),tr)::tl ->
    begin
      assert ( ctx = LList.nil ); (* no beta in patterns *)
     (* The case doesn't match if the DB indices differ or the stack is not
      * of the expected size. *)
      if n == n' && List.length stack == nargs
      then Some (tr,stack)
      else find_case st tl default
    end
  | { ctx; term=Lam _; stack } , ( CLam , tr )::tl ->
    begin
      match term_of_state st with (*TODO could be optimized*)
      | Lam (_,_,_,te) ->
        Some ( tr , [{ ctx=LList.nil; term=te; stack=[] }] )
      | _ -> assert false
    end
  | _, _::tl -> find_case st tl default


(*TODO implement the stack as an array ? (the size is known in advance).*)
let gamma_rw (sg:Signature.t) : stack -> dtree -> (rule_name*env*term) option =
  let rec rw stack = function
    | Switch (i,cases,def) ->
       begin
         let arg_i = SR.state_whnf sg (List.nth stack i) in
         match find_case arg_i cases def with
         | Some (g,[]) -> rw stack g
         | Some (g,s ) -> rw (stack@s) g
         (* This line highly depends on how the module dtree works.
         When a column is specialized, new columns are added at the end
         This is the reason why s is added at the end. *)
         | None -> None
       end
    | Test (rname,Syntactic ord, eqs, right, def) ->
       begin
         match get_context_syn sg stack ord with
         | None -> bind_opt (rw stack) def
         | Some ctx ->
            if test sg ctx eqs then Some (rname, ctx, right)
            else bind_opt (rw stack) def
       end
    | Test (rname,MillerPattern lst, eqs, right, def) ->
       begin
         match get_context_mp sg stack lst with
         | None -> bind_opt (rw stack) def
         | Some ctx ->
            if test sg ctx eqs then Some (rname, ctx, right)
            else bind_opt (rw stack) def
       end
  in
  rw

end (* module MkRewriter (SR : StateReducer) (R : Reducer) : GammaRW *)

(******************************************************************************)

module MkStateReducer (GR : GammaRW) (C:Controller) : StateReducer = struct

(* Definition: a term is in weak-head-normal form if all its reducts
 * (including itself) have same 'shape' at the root.
 * The shape of a term could be computed like this:
 *
 * let rec shape = function
 *  | Type -> Type
 *  | Kind -> Kind
 *  | Pi _ -> Pi
 *  | Lam _ -> Lam
 *  | DB (_,_,n) -> DB n
 *  | Const (_,m,v) -> Const m v
 *  | App(f,a0,args) -> App (shape f,List.lenght (a0::args))

 * Property:
 * A (strongly normalizing) non weak-head-normal term can only have the form:
 * - (x:A => b) a c_1..c_n, this is a beta-redex potentially with extra arguments.
 * - or c a_1 .. a_n b_1 ..b_n with c a constant and c a'_1 .. a'_n is a gamma-redex
 *   where the (a'_i)s are reducts of (a_i)s.
 *)

(* This function reduces a state to a weak-head-normal form.
 * This means that the term [term_of_state (state_whnf sg state)] is a
 * weak-head-normal reduct of [term_of_state state].
 *
 * Moreover the returned state verifies the following properties:
 * - state.term is not an application
 * - state.term can only be a variable if term.ctx is empty
 *    (and therefore this variable is free in the corresponding term)
 * *)
let rec state_whnf (sg:Signature.t) (st:state) : state =
  match st with
  (* Weak heah beta normal terms *)
  | { term=Type _ }
  | { term=Kind }
  | { term=Pi _ }
  | { term=Lam _; stack=[] } -> st
  | { term=Lam _ } when not (C.beta_filter ()) -> st
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if n < LList.len ctx
    then state_whnf sg { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    else { ctx=LList.nil; term=(mk_DB l x (n-LList.len ctx)); stack }
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    if C.use_beta sg st
    then state_whnf sg
        { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s }
    else st
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
    state_whnf sg { ctx; term=f; stack=List.rev_append tl' s }
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,n); stack } ->
    let trees = Signature.get_dtree sg (C.rule_filter ()) l n in
    match find_dtree (List.length stack) trees with
    | None -> st
    | Some (ar, tree) ->
      let s1, s2 = split_list ar stack in
      match GR.gamma_rw sg s1 tree with
      | None -> st
      | Some (rname,ctx,term) ->
        if C.use_rule sg st rname
        then state_whnf sg { ctx; term; stack=s2 }
        else st

end (* module MkStateReducer (GR : GammaRW) (C:Controller) : StateReducer *)

(******************************************************************************)

module MkReducer (SR : StateReducer) : Reducer = struct

(* Weak Head Normal Form *)
let whnf sg term = term_of_state ( SR.state_whnf sg { ctx=LList.nil; term; stack=[] } )

(* Strong Normal Form *)
let rec snf sg (t:term) : term =
  match whnf sg t with
  | Kind | Const _
  | DB _ | Type _ as t' -> t'
  | App (f,a,lst) -> mk_App (snf sg f) (snf sg a) (List.map (snf sg) lst)
  | Pi (_,x,a,b) -> mk_Pi dloc x (snf sg a) (snf sg b)
  | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (snf sg) a) (snf sg b)

let rec are_convertible_lst sg : (term*term) list -> bool =
  function
  | [] -> true
  | (t1,t2)::lst ->
    begin
      match (
        if term_eq t1 t2 then Some lst
        else
          match whnf sg t1, whnf sg t2 with
          | Kind, Kind | Type _, Type _ -> Some lst
          | Const (_,n), Const (_,n') when ( name_eq n n' ) -> Some lst
          | DB (_,_,n), DB (_,_,n') when ( n==n' ) -> Some lst
          | App (f,a,args), App (f',a',args') ->
            add_to_list2 args args' ((f,f')::(a,a')::lst)
          | Lam (_,_,_,b), Lam (_,_,_,b') -> Some ((b,b')::lst)
          | Pi (_,_,a,b), Pi (_,_,a',b') -> Some ((a,a')::(b,b')::lst)
          | t1, t2 -> None
      ) with
      | None -> false
      | Some lst2 -> are_convertible_lst sg lst2
    end

(* Convertibility Test *)
let are_convertible sg t1 t2 = are_convertible_lst sg [(t1,t2)]

(* Head Normal Form *)
let rec hnf sg t =
  match whnf sg t with
  | Kind | Const _ | DB _ | Type _ | Pi (_,_,_,_) | Lam (_,_,_,_) as t' -> t'
  | App (f,a,lst) -> mk_App (hnf sg f) (hnf sg a) (List.map (hnf sg) lst)

end (* module MkReducer (SR : StateReducer) : Reducer *)

(******************************************************************************)

(** Standard controller *)
module StdController : Controller = struct
  let use_rule sg s r = true
  let use_beta sg s = true
  let rule_filter () = None
  let beta_filter () = true
end

module
  rec StdGammaRW      : GammaRW      = MkRewriter(StdStateReducer)(StdReducer)
  and StdStateReducer : StateReducer = MkStateReducer(StdGammaRW)(StdController)
  and StdReducer      : Reducer      = MkReducer(StdStateReducer)

(******************************************************************************)

let beta = ref true
let steps_limit = ref None
let rule_filter  = ref None
let count = ref 0
let reset () = count := 0

module ParamController : Controller =
struct
  let try_reduce () = match !steps_limit with
    | None -> true
    | Some limit -> if !count < limit then (incr count; true) else false
  let use_rule sg s r = (Debug.(debug d_warn) "Test %i" !count; try_reduce ())
  let use_beta sg s = !beta && try_reduce ()
  let rule_filter () = !rule_filter
  let beta_filter () = !beta
end


module rec ParamReducer :
sig
  include Reducer
  val set_strategy : red_cfg -> unit
  val reset : unit -> unit
end =
struct
  include MkReducer(ParamStateReducer)
  let set_strategy s =
    rule_filter := s.select;
    beta        := s.beta;
    steps_limit := s.nb_steps
  let reset = reset
end
and ParamStateReducer : StateReducer = MkStateReducer(ParamGammaRW)(ParamController)
and ParamGammaRW      : GammaRW      = MkRewriter(ParamStateReducer)(ParamReducer)
