open Basic
open Format
open Rule
open Term
open Dtree

type red_strategy = Hnf | Snf | Whnf

type red_cfg = {
  select : (Rule.rule_name -> bool) option;
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

let solve (sg:Signature.t) (reduce:rw_strategy) (depth:int) (pbs:int LList.t) (te:term) : term =
  try Matching.solve depth pbs te
  with Matching.NotUnifiable ->
    Matching.solve depth pbs (reduce sg te)

let unshift (sg:Signature.t) (reduce:rw_strategy) (q:int) (te:term) =
  try Subst.unshift q te
  with Subst.UnshiftExn ->
    Subst.unshift q (reduce sg te)

let get_context_syn (sg:Signature.t) (forcing:rw_strategy) (stack:stack) (ord:arg_pos LList.t) : env option =
  try Some (LList.map (
      fun p ->
        if ( p.depth = 0 )
        then lazy (term_of_state (List.nth stack p.position) )
        else
          Lazy.from_val
            (unshift sg forcing p.depth (term_of_state (List.nth stack p.position) ))
    ) ord )
  with Subst.UnshiftExn -> ( None )

let get_context_mp (sg:Signature.t) (forcing:rw_strategy) (stack:stack)
                   (pb_lst:abstract_problem LList.t) : env option =
  let aux ((pos,dbs):abstract_problem) : term Lazy.t =
    let res = solve sg forcing pos.depth dbs (term_of_state (List.nth stack pos.position)) in
    Lazy.from_val (Subst.unshift pos.depth res)
  in
  try Some (LList.map aux pb_lst)
  with Matching.NotUnifiable -> None
     | Subst.UnshiftExn -> assert false

let rec test (sg:Signature.t) (convertible:convertibility_test)
             (ctx:env) (constrs: constr list) : bool  =
  match constrs with
  | [] -> true
  | (Linearity (i,j))::tl ->
    let t1 = mk_DB dloc dmark i in
    let t2 = mk_DB dloc dmark j in
    if convertible sg (term_of_state { ctx; term=t1; stack=[] })
                      (term_of_state { ctx; term=t2; stack=[] })
    then test sg convertible ctx tl
    else false
  | (Bracket (i,t))::tl ->
    let t1 = Lazy.force (LList.nth ctx i) in
    let t2 = term_of_state { ctx; term=t; stack=[] } in
    if convertible sg t1 t2
    then test sg convertible ctx tl
    else
      (*FIXME: if a guard is not satisfied should we fail or only warn the user? *)
      raise (Signature.SignatureError( Signature.GuardNotSatisfied(get_loc t1, t1, t2) ))
  | (Condition(l,r))::tl ->
    let l' = term_of_state {ctx;term=l; stack=[] } in
    let r' = term_of_state {ctx;term=r; stack=[] } in
    if convertible sg l' r' then
      test sg convertible ctx tl
    else false

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
let gamma_rw (sg:Signature.t)
             (convertible:convertibility_test)
             (forcing:rw_strategy)
             (strategy:rw_state_strategy) : stack -> dtree -> (env*term) option =
  let rec rw stack = function
    | Switch (i,cases,def) ->
       begin
         let arg_i = strategy sg (List.nth stack i) in
         match find_case arg_i cases def with
         | Some (g,[]) -> rw stack g
         | Some (g,s ) -> rw (stack@s) g
         (* This line highly depends on how the module dtree works.
         When a column is specialized, new columns are added at the end
         This is the reason why s is added at the end. *)
         | None -> None
       end
    | Test (_,Syntactic ord, eqs, right, def) ->
       begin
         match get_context_syn sg forcing stack ord with
         | None -> bind_opt (rw stack) def
         | Some ctx ->
            if test sg convertible ctx eqs then Some (ctx, right)
            else bind_opt (rw stack) def
       end
    | Test (_,MillerPattern lst, eqs, right, def) ->
       begin
         match get_context_mp sg forcing stack lst with
         | None -> bind_opt (rw stack) def
         | Some ctx ->
            if test sg convertible ctx eqs then Some (ctx, right)
            else bind_opt (rw stack) def
       end
  in
  rw


(* ********************* *)

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
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if n < LList.len ctx
    then state_whnf sg { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    else { ctx=LList.nil; term=(mk_DB l x (n-LList.len ctx)); stack }
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    if not !beta then st
    else state_whnf sg { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s }
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
    state_whnf sg { ctx; term=f; stack=List.rev_append tl' s }
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,n); stack } ->
    let trees = Signature.get_dtree sg !selection l n in
    match find_dtree (List.length stack) trees with
    | None -> st
    | Some (ar, tree) ->
      let s1, s2 = split_list ar stack in
      match gamma_rw sg are_convertible snf state_whnf s1 tree with
      | None -> st
      | Some (ctx,term) -> state_whnf sg { ctx; term; stack=s2 }

(* ********************* *)

(* Weak Head Normal Form *)
and whnf sg term = term_of_state ( state_whnf sg { ctx=LList.nil; term; stack=[] } )

(* Strong Normal Form *)
and snf sg (t:term) : term =
  match whnf sg t with
  | Kind | Const _
  | DB _ | Type _ as t' -> t'
  | App (f,a,lst) -> mk_App (snf sg f) (snf sg a) (List.map (snf sg) lst)
  | Pi (_,x,a,b) -> mk_Pi dloc x (snf sg a) (snf sg b)
  | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (snf sg) a) (snf sg b)

and are_convertible_lst sg : (term*term) list -> bool =
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
and are_convertible sg t1 t2 = are_convertible_lst sg [(t1,t2)]

(* Head Normal Form *)
let rec hnf sg t =
  match whnf sg t with
  | Kind | Const _ | DB _ | Type _ | Pi (_,_,_,_) | Lam (_,_,_,_) as t' -> t'
  | App (f,a,lst) -> mk_App (hnf sg f) (hnf sg a) (List.map (hnf sg) lst)

let reduction = function
  | Hnf  -> hnf
  | Snf  -> snf
  | Whnf -> whnf

(* n-steps reduction on state *)
let state_nsteps (sg:Signature.t) (strat:red_strategy)
    (steps:int) (state:state) =
  let rec aux (red,st:(int*state)) : int*state =
    if red <= 0 then (0, st)
    else match st with
      (* Normal terms *)
      | { term=Type _ }  | { term=Kind } -> (red, st)
      (* Pi types are head normal terms *)
      | { term=Pi _ } when strat <> Snf  -> (red, st)
      (* Strongly normalizing Pi types *)
      | { ctx=ctx; term=Pi(l,x,a,b) } ->
        let (red, a') = aux (red , {st with term=a} ) in
        let snf_a = term_of_state a' in
        let state_b = {ctx=LList.cons (lazy snf_a) ctx; term=b; stack=[]} in
        let (red, b') = aux (red, state_b) in
        (red, {st with term=mk_Pi l x snf_a (term_of_state b') } )

      (* Beta redex *)
      | { ctx; term=Lam (_,_,_,t); stack=p::s } when !beta ->
        aux (red-1, { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s })
      (* Not a beta redex (or beta disabled) *)
      | { term=Lam _ } when strat == Whnf -> (red, st)
      (* Not a beta redex (or beta disabled) but keep looking for normal form *)
      | { ctx; term=Lam(l,x,ty_opt,t); stack } ->
        begin
          match term_of_state st with
          | Lam(_,_,_,t') ->
            let (red, st_t) = aux (red, {ctx=LList.nil; term=t'; stack=[]}) in
            let t' = term_of_state st_t in
            begin
              match strat, ty_opt with
              | Snf, Some ty ->
                let red, ty = aux (red, {ctx; term=ty; stack=[]}) in
                (red, {st with term=mk_Lam l x (Some (term_of_state ty)) t' })
              | _ -> (red, {st with term=mk_Lam l x ty_opt t' })
            end
          | _ -> assert false
        end

      (* DeBruijn index: environment lookup *)
      | { ctx; term=DB (_,_,n); stack } when n < LList.len ctx ->
        aux (red, { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack })
      (* DeBruijn index: out of environment *)
      | { term=DB _ } -> (red, st)

      (* Application: arguments go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s } when strat <> Snf ->
        let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
        aux (red, { ctx; term=f; stack=List.rev_append tl' s })
      (* Application: arguments are reduced then go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s } ->
        let redc = ref red in
        let reduce t =
          let new_redc, st = aux (!redc, {ctx;term=t;stack=[]}) in
          redc := new_redc;
          st in
        let new_stack = List.rev_append (List.rev_map reduce (a::lst)) s in
        aux (!redc, {ctx; term=f; stack=new_stack })

      (* Potential Gamma redex *)
      | { ctx; term=Const (l,n); stack } ->
        let trees = Signature.get_dtree sg !selection l n in
        match find_dtree (List.length stack) trees with
        | None -> (red,st)
        | Some (ar, tree) ->
          let s1, s2 = split_list ar stack in
          match gamma_rw sg are_convertible snf state_whnf s1 tree with
          | None -> (red,st)
          | Some (ctx,term) -> aux (red-1, { ctx; term; stack=s2 })
  in
  aux (steps,state)

let reduction_steps n strat sg t =
  let st = { ctx=LList.nil; term=t; stack=[] } in
  let (_,st') = state_nsteps sg strat n st in
  term_of_state st'

let reduction strat sg te =
  select strat.select strat.beta;
  let te' =
    match strat with
    | { nb_steps = Some n; _} -> reduction_steps n strat.strategy sg te
    | _ -> reduction strat.strategy sg te
  in
  select default_cfg.select default_cfg.beta;
  te'
