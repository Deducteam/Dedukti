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

exception NotConvertible

let rec zip_lists l1 l2 lst =
  match l1, l2 with
  | [], [] -> lst
  | s1::l1, s2::l2 -> zip_lists l1 l2 ((false,s1,s2)::lst)
  | _,_ -> raise NotConvertible

(* State *)


(* A state {ctx; term; stack} is the state of an abstract machine that
represents a term where [ctx] is a ctx that contains the free variables
of [term] and [stack] represents the terms that [term] is applied to. *)
type state = {
  ctx   : env;    (*context*)
  term  : term;   (*term to reduce*)
  stack : stack;  (*stack*)
  mutable reduc : reduct; (* Pointer to reduct. *)
}
and stack = state list
and env = state LList.t
and reduct = WHNF | Unknown | Reduct of state
(* whenever st.reduc = Reduct whnf then whnf.reduc <> Unknown *)

let mk_state      ctx term stack = { ctx; term; stack; reduc=Unknown}
let mk_whnf_state ctx term stack = { ctx; term; stack; reduc=WHNF   }

let state_of_term t = mk_state LList.nil t []

let rec find_reduct st = match st.reduc with
  | Unknown -> (false, st)
  | WHNF    -> (true , st)
  | Reduct red -> match red.reduc with
  | Unknown -> (false, red)
  | WHNF    -> (true , red)
    | Reduct red' as r -> st.reduc <- r; find_reduct red

let rec term_of_state {ctx;term;stack} : term =
  let t =
    if LList.is_empty ctx then term
    else Subst.psubst_l (LList.map (fun x -> lazy (term_of_state x)) ctx) term
  in
  match stack with
  | [] -> t
  | a::lst -> mk_App t (term_of_state a) (List.map term_of_state lst)

(* Pretty Printing *)

let pp_state fmt st =
  fprintf fmt "{ctx} {%a} {stack[%i]}\n" pp_term st.term (List.length st.stack)

let pp_state2 fmt st = pp_term fmt (term_of_state st)

let pp_stack fmt stck =
  fprintf fmt "[\n";
  List.iter (pp_state fmt) stck;
  fprintf fmt "]\n"

let pp_env fmt (ctx:env) =
  pp_list ", " pp_state2 fmt (LList.lst ctx)

let pp_stack fmt (st:stack) =
  let aux fmt state = pp_term fmt (term_of_state state) in
  fprintf fmt "[ %a ]\n" (pp_list "\n | " aux) st

let pp_state ?(if_ctx=true) ?(if_stack=true) fmt { ctx; term; stack } =
  if if_ctx
  then fprintf fmt "{ctx=[%a];@." pp_env ctx
  else fprintf fmt "{ctx=[...](%i);@." (LList.len ctx);
  fprintf fmt "term=%a;@." pp_term term;
  if if_stack
  then fprintf fmt "stack=%a}@." pp_stack stack
  else fprintf fmt "stack=[...]}@.";
  fprintf fmt "@.%a@." pp_term (term_of_state (mk_state ctx term stack))

let simpl_pp_state = pp_state ~if_ctx:true ~if_stack:true


let rec set_reduct st red =
  assert (red.reduc = WHNF);
  match st.reduc with
  | Reduct stred -> (
      assert (stred != st);
      set_reduct stred red)
  | _ -> if red != st then st.reduc <- Reduct red

let as_whnf st r =
  let whnf = match find_reduct r with
    | false, red -> mk_whnf_state red.ctx red.term red.stack
    | true , red -> red in
  set_reduct st whnf; whnf

(* ********************* *)

type rw_strategy = Signature.t -> term -> term

type rw_state_strategy = Signature.t -> state -> state

type convertibility_test = Signature.t -> term -> term -> bool
type st_convertibility_test = Signature.t -> state -> state -> bool


let solve (sg:Signature.t) (reduce:rw_strategy) (depth:int) (pbs:int LList.t) (te:term) : term =
  try Matching.solve depth pbs te
  with Matching.NotUnifiable ->
    Matching.solve depth pbs (reduce sg te)

let rec unshift_st (sg:Signature.t) (reduce:rw_strategy) (q:int) (st:state) =
  let _, st = find_reduct st in
  if q = 0 then st
  else let t =
         try  Subst.unshift q (term_of_state st)
         with Subst.UnshiftExn -> Subst.unshift q (reduce sg (term_of_state st))
    in state_of_term t

let unshift (sg:Signature.t) (reduce:rw_strategy) (q:int) (te:term) =
  try Subst.unshift q te
  with Subst.UnshiftExn -> Subst.unshift q (reduce sg te)

let get_context_syn (sg:Signature.t) (forcing:rw_strategy) (stack:stack)
    (ord:arg_pos LList.t) : env option =
  let solve p =
    let st = List.nth stack p.position in
    if p.depth = 0 then st
    else unshift_st sg forcing p.depth st in
  try Some (LList.map solve ord)
  with Subst.UnshiftExn -> None

let get_context_mp (sg:Signature.t) (forcing:rw_strategy) (stack:stack)
                   (pb_lst:abstract_problem LList.t) : env option =
  let aux ((pos,dbs):abstract_problem) : state =
    let t = term_of_state (List.nth stack pos.position) in
    state_of_term (solve sg forcing pos.depth dbs t)
  in
  try Some (LList.map aux pb_lst)
  with Matching.NotUnifiable -> None
     | Subst.UnshiftExn -> assert false

let rec test (sg:Signature.t) (convertible:st_convertibility_test)
             (ctx:env) (constrs: constr list) : bool  =
  match constrs with
  | [] -> true
  | (Linearity (i,j))::tl ->
    convertible sg (LList.nth ctx i) (LList.nth ctx j)
    && test sg convertible ctx tl
  | (Bracket (i,t))::tl ->
    let t1 = LList.nth ctx i in
    let t2 = mk_state ctx t [] in
    if convertible sg t1 t2
    then test sg convertible ctx tl
    else
      (*FIXME: if a guard is not satisfied should we fail or simply warn the user? *)
      raise (Signature.SignatureError
               (Signature.GuardNotSatisfied
                  (get_loc t1.term, term_of_state t1, term_of_state t2)))

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
        Some ( tr , [ state_of_term te ] )
      | _ -> assert false
    end
  | _, _::tl -> find_case st tl default


(*TODO implement the stack as an array ? (the size is known in advance).*)
let gamma_rw (sg:Signature.t)
             (convertible:st_convertibility_test)
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
  state_whnf_aux sg st st

and state_whnf_aux (sg:Signature.t) (root:state) (state:state) : state =
  let rec_call c t s = state_whnf_aux sg root (mk_state c t s) in
  let return red = as_whnf root red in
  match find_reduct state with
  | (true, whnf) -> return whnf
  | (false, st) ->
    let _ = Debug.(debug d_reduce "Reducing %a" simpl_pp_state st) in
  match st with
  (* Weak heah beta normal terms *)
  | { term=Type _ }
  | { term=Kind }
  | { term=Pi _ }
  | { term=Lam _; stack=[] } -> return st
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if LList.is_empty ctx then return st
    else if n < LList.len ctx
    then
      let ctx_st = LList.nth ctx n in
      if stack == [] then state_whnf_aux sg root ctx_st
      else rec_call ctx_st.ctx ctx_st.term (ctx_st.stack @ stack)
    else return (mk_state LList.nil (mk_DB l x (n-LList.len ctx)) stack)
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    if not !beta then return st
    else rec_call (LList.cons p ctx) t s
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let aux = function
      | DB(l,x,n) when n < LList.len ctx -> LList.nth ctx n
      | t -> mk_state ctx t [] in
    let tl' = List.rev_map aux (a::lst) in
    rec_call ctx f (List.rev_append tl' s)
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,n); stack } ->
    let trees = Signature.get_dtree sg !selection l n in
    match find_dtree (List.length stack) trees with
    | None -> return st
    | Some (ar, tree) ->
      let s1, s2 = split_list ar stack in
      match gamma_rw sg are_convertible_st snf state_whnf s1 tree with
      | None -> return st
      | Some (ctx,term) -> rec_call ctx term s2

(* ********************* *)

(* Weak Head Normal Form *)
and whnf sg term = term_of_state (state_whnf sg (state_of_term term))

(* Strong Normal Form *)
and snf sg (t:term) : term =
  match whnf sg t with
  | Kind | Const _
  | DB _ | Type _ as t' -> t'
  | App (f,a,lst) -> mk_App (snf sg f) (snf sg a) (List.map (snf sg) lst)
  | Pi (_,x,a,b) -> mk_Pi dloc x (snf sg a) (snf sg b)
  | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (snf sg) a) (snf sg b)


(* TODO: When checking st1 ~ st2: st1's WHNF could be set to st2 (even though
   it's already a WHNF) so that later converitiblity check between these
   states is a simple physical equality check.  *)
and check_convertible_lst sg : (bool * state * state) list -> unit = function
  | [] -> ()
  | (flag, st1, st2)::lst ->
    check_convertible_lst sg
      (
        if flag then (set_reduct st1 st2; lst)
        else if st1 == st2 then lst else
        let st1f, st1 = find_reduct st1 in
        let st2f, st2 = find_reduct st2 in
        if st1 == st2 then lst
        else
        let lst' = (true, st1, st2)::lst in
        if not (st1f && st2f) then
          if term_eq st1.term st2.term && term_eq (term_of_state st1) (term_of_state st2)
          then lst
          else
            (false,
             (if st1f then st1 else state_whnf sg st1),
             (if st2f then st1 else state_whnf sg st2)) :: lst
        else
          let {ctx=ctx1; term=t1; stack=s1} = st1 in
          let {ctx=ctx2; term=t2; stack=s2} = st2 in
          match t1, t2 with
          | Kind, Kind | Type _, Type _ -> assert (s1 = [] && s2 = []); lst'
          | Const(_,n1), Const(_,n2) ->
            if name_eq n1 n2 then zip_lists s1 s2 lst'
            else raise NotConvertible
          | DB (_,_,n1), DB (_,_,n2) ->
            assert (LList.is_empty ctx1);
            assert (LList.is_empty ctx2);
            if n1 == n2 then zip_lists s1 s2 lst'
            else raise NotConvertible
          | Lam _, Lam _ ->
            assert (s1 = [] && s2 = []);
            begin
              match (term_of_state st1, term_of_state st2) with
              | Lam (_,_,_,b1), Lam (_,_,_,b2) ->
                (false, state_of_term b1, state_of_term b2)::lst'
              | _ -> assert false
            end
          | Pi _, Pi _ ->
            assert (s1 = [] && s2 = []);
            begin
              match (term_of_state st1, term_of_state st2) with
              | Pi (_,_,a,b), Pi (_,_,a',b') ->
                (false, state_of_term a, state_of_term a')::
                (false, state_of_term b, state_of_term b')::lst'
              | _ -> assert false
            end
          | t1, t2 -> raise NotConvertible
      )
      

(* Convertibility tests *)

and are_convertible_st sg st1 st2 =
  try check_convertible_lst sg [(false,st1,st2)]; true
  with NotConvertible -> false


let are_convertible sg t1 t2 =
  are_convertible_st sg (state_of_term t1) (state_of_term t2)

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
  let rec aux (nred,st:(int*state)) : int*state =
    if nred <= 0 then (0, st)
    else
      match find_reduct st with
      | (true, whnf) -> (nred, whnf)
      | (false,st) -> match st with
      (* Normal terms *)
      | { term=Type _ }  | { term=Kind } -> (nred, st)
      (* Pi types are head normal terms *)
      | { term=Pi _ } when strat <> Snf  -> (nred, st)
      (* Strongly normalizing Pi types *)
      | { ctx=ctx; term=Pi(l,x,a,b) } ->
        let (nred, a') = aux (nred, mk_state ctx                 a []) in
        let (nred, b') = aux (nred, mk_state (LList.cons a' ctx) b []) in
        (nred, mk_state ctx (mk_Pi l x (term_of_state a') (term_of_state b')) [])

      (* Beta redex *)
      | { ctx; term=Lam (_,_,_,t); stack=p::s } when !beta ->
        aux (nred-1, mk_state (LList.cons p ctx) t s)
      (* Not a beta redex (or beta disabled) *)
      | { term=Lam _ } when strat == Whnf -> (nred, st)
      (* Not a beta redex (or beta disabled) but keep looking for normal form *)
      | { ctx; term=Lam(l,x,ty_opt,t); stack=[] } ->
        begin
          match term_of_state st with
          | Lam(_,_,_,t') ->
            let (nred, st_t) = aux (nred, state_of_term t') in
            let t' = term_of_state st_t in
            begin
              match strat, ty_opt with
              | Snf, Some ty ->
                let nred, ty = aux (nred, mk_state ctx ty []) in
                (nred, mk_state ctx (mk_Lam l x (Some (term_of_state ty)) t') [])
              | _ -> (nred, mk_state ctx (mk_Lam l x ty_opt t') [])
            end
          | _ -> assert false
        end
      | { ctx; term=Lam(l,x,ty_opt,t); stack=a::args } ->
        begin
          match term_of_state st with
          | App(Lam(_,_,_,t'),_,_) ->
            let (nred, st_t) = aux (nred, state_of_term t') in
            let t' = term_of_state st_t in
            begin
              match strat with
              | Snf ->
                let nred, args = List.fold_right (fun a (nred,args) ->
                  let nred, a' = aux (nred,a) in
                  nred,a::args) (a::args)  (nred,[])
                in
                (nred, mk_state ctx (mk_Lam l x ty_opt t') args)
              | _ -> (nred, mk_state ctx (mk_Lam l x ty_opt t') (a::args))
            end
          | _ -> assert false
        end
      (* DeBruijn index: environment lookup *)
      | { ctx; term=DB (_,_,n); stack } when n < LList.len ctx ->
        let ctx_st = LList.nth ctx n in
        let nst =
          if stack == [] then ctx_st
          else mk_state ctx_st.ctx ctx_st.term (ctx_st.stack @ stack)
        in aux (nred, nst)
      (* DeBruijn index: out of environment *)
      | { term=DB _ } -> (nred, st)
      (* Application: arguments go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s } when strat <> Snf ->
        let aux' = function
          | DB(l,x,n) when n < LList.len ctx -> LList.nth ctx n
          | t -> mk_state ctx t [] in
        let tl' = List.rev_map aux' (a::lst) in
        aux (nred, mk_state ctx f (List.rev_append tl' s) )
      (* Application: arguments are reduced then go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s } ->
        let nredc = ref nred in
        let reduce t =
          let new_nredc, st = aux (!nredc, mk_state ctx t []) in
          nredc := new_nredc;
          st in
        let new_stack = List.rev_append (List.rev_map reduce (a::lst)) s in
        aux (!nredc, mk_state ctx f new_stack)
      (* Potential Gamma redex *)
      | { ctx; term=Const (l,n); stack } ->
        let trees = Signature.get_dtree sg !selection l n in
        match find_dtree (List.length stack) trees with
        | None -> (nred,st)
        | Some (ar, tree) ->
          let s1, s2 = split_list ar stack in
          match gamma_rw sg are_convertible_st snf state_whnf s1 tree with
          | None -> (nred,st)
          | Some (ctx,term) -> aux (nred-1, mk_state ctx term s2)
  in
  aux (steps,state)

let reduction_steps n strat sg t =
  let st = state_of_term t in
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
