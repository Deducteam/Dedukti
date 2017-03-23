open Basic
open Format
open Rule
open Term
open Dtree

let selection = ref None

let select l = selection := l

(* State *)

type env = term Lazy.t LList.t

(* A state {ctx; term; stack} is the state of an abstract machine that represents a term where [ctx] is a ctx that contains the free variable of [term] and [stack] represents the terms that [term] is applied to. *)
type state = {
  ctx:env;              (*context*)
  term : term;          (*term to reduce*)
  stack : stack;        (*stack*)
}
and stack = state list

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx 0 term ) in
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
  begin
    if if_ctx then
      fprintf fmt "{ctx=[%a];@." pp_env ctx
    else
      fprintf fmt "{ctx=[...](%i);@." (LList.len ctx)
  end;
  fprintf fmt "term=%a;@." pp_term term;
  begin
    if if_stack then
      fprintf fmt "stack=%a}@." pp_stack stack
    else
      fprintf fmt "stack=[...]}@."
  end;
  fprintf fmt "@.%a@." pp_term (term_of_state {ctx; term; stack})

(* Misc *)
(* FIXME: only used once in are_convertible_list, should it be declared at top level? *)
let rec add_to_list2 l1 l2 lst =
  match l1, l2 with
    | [], [] -> Some lst
    | s1::l1, s2::l2 -> add_to_list2 l1 l2 ((s1,s2)::lst)
    | _,_ -> None

let rec split_stack (i:int) : stack -> (stack*stack) option = function
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> map_opt (fun (s1,s2) -> (x::s1,s2) ) (split_stack (i-1) l)

let rec safe_find m v = function
  | []                  -> None
  | (_,m',v',tr)::tl       ->
      if ident_eq v v' && ident_eq m m' then Some tr
      else safe_find m v tl

let rec add_to_list lst (s:stack) (s':stack) =
  match s,s' with
    | [] , []           -> Some lst
    | x::s1 , y::s2     -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> None

(* ********************* *)

type rw_strategy = Signature.t -> term -> term

type rw_state_strategy = Signature.t -> state -> state

type convertibility_test = Signature.t -> term -> term -> bool

let solve (sg:Signature.t) (reduce:rw_strategy) (depth:int) (pbs:int LList.t) (te:term) : term =
(*   let res = *)
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
        if ( p.depth = 0 ) then
          lazy (term_of_state (List.nth stack p.position) )
        else
          Lazy.from_val
            (unshift sg forcing p.depth (term_of_state (List.nth stack p.position) ))
    ) ord )
  with Subst.UnshiftExn -> ( None )

let get_context_mp (sg:Signature.t) (forcing:rw_strategy) (stack:stack) (pb_lst:abstract_problem LList.t) : env option =
  let aux ((pos,dbs):abstract_problem) : term Lazy.t =
    let res = solve sg forcing pos.depth dbs (term_of_state (List.nth stack pos.position)) in
    Lazy.from_val (Subst.unshift pos.depth res)
  in
  try Some (LList.map aux pb_lst)
  with Matching.NotUnifiable -> None
     | Subst.UnshiftExn -> assert false

let rec test (sg:Signature.t) (convertible:convertibility_test) (ctx:env) (constrs: constr list) : bool  =
  match constrs with
  | [] -> true
  | (Linearity (i,j))::tl ->
    let t1 = mk_DB dloc dmark i in
    let t2 = mk_DB dloc dmark j in
    if convertible sg (term_of_state { ctx; term=t1; stack=[] })
        (term_of_state { ctx; term=t2; stack=[] })
    then test sg convertible ctx tl
    else false
  | (Bracket (i,t2))::tl ->
    let t1 = mk_DB dloc dmark i in
    if convertible sg (term_of_state { ctx; term=t1; stack=[] })
                                (term_of_state { ctx; term=t2; stack=[] })
    then test sg convertible ctx tl
    else
      (*FIXME: if a guard is not satisfied should we fail or only warn the user? *)
      failwith "Error while reducing a term: a guard was not satisfied."

let rec find_case (st:state) (cases:(case * dtree) list) (default:dtree option) : (dtree*state list) option =
  match st, cases with
  | _, [] -> map_opt (fun g -> (g,[])) default
  | { term=Const (_,m,v); stack } , (CConst (nargs,m',v'),tr)::tl ->
    if ident_eq v v' && ident_eq m m' then
      begin
        assert (List.length stack >= nargs);
        Some (tr,stack)
      end
    else find_case st tl default
  | { ctx; term=DB (l,x,n); stack } , (CDB (nargs,n'),tr)::tl ->
    begin
      assert ( ctx = LList.nil ); (* no beta in patterns *)
      if n==n' && (List.length stack >= nargs) then
        Some (tr,stack)
      else
        find_case st tl default
    end
  | { ctx; term=Lam (_,_,_,_) } , ( CLam , tr )::tl ->
    begin
      match term_of_state st with (*TODO could be optimized*)
      | Lam (_,_,_,te) ->
        Some ( tr , [{ ctx=LList.nil; term=te; stack=[] }] )
      | _ -> assert false
    end
  | _, _::tl -> find_case st tl default


(*TODO implement the stack as an array ? (the size is known in advance).*)
let rec gamma_rw (sg:Signature.t) (convertible:convertibility_test) (forcing:rw_strategy) (strategy:rw_state_strategy) (stack:stack) : dtree -> (env*term) option = function
  | Switch (i,cases,def) ->
    begin
      let arg_i = strategy sg (List.nth stack i) in
      match find_case arg_i cases def with
      | Some (g,[]) -> gamma_rw sg convertible forcing strategy stack g
      | Some (g,s) -> gamma_rw sg convertible forcing strategy (stack@s) g (* this line highly depends on how the module dtree works. When a column is specialized, new columns are added at the end this explains why s is added at the end. *)
      | None -> None
    end
  | Test (Syntactic ord, eqs, right, def) ->
    begin
      match get_context_syn sg forcing stack ord with
      | None -> bind_opt (gamma_rw sg convertible forcing strategy stack) def
      | Some ctx ->
        if test sg convertible ctx eqs then Some (ctx, right)
        else bind_opt (gamma_rw sg convertible forcing strategy stack) def
    end
  | Test (MillerPattern lst, eqs, right, def) ->
    begin
      match get_context_mp sg forcing stack lst with
      | None -> bind_opt (gamma_rw sg convertible forcing strategy stack) def
      | Some ctx ->
        if test sg convertible ctx eqs then Some (ctx, right)
        else bind_opt (gamma_rw sg convertible forcing strategy stack) def
    end


(* ********************* *)

(* Definition: a term is in weak-head-normal form if all its reducts (including itself)
 * have same 'shape' at the root.
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
  | { term=Lam _; stack=[] } as state -> state
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if n < LList.len ctx then
      state_whnf sg { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    else
      { ctx=LList.nil; term=(mk_DB l x (n-LList.len ctx)); stack }
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    state_whnf sg { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s }
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
    state_whnf sg { ctx; term=f; stack=List.rev_append tl' s }
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,m,v); stack } as state ->
    begin
      let dtree =
        match !selection with
        | None -> Signature.get_dtree sg l m v
        | Some selection -> Signature.get_dtree sg ~select:selection l m v
      in
      match dtree with
      | None -> state
      | Some (i,g) ->
        begin
          match split_stack i stack with
          | None -> state
          | Some (s1,s2) ->
            ( match gamma_rw sg are_convertible snf state_whnf s1 g with
              | None -> state
              | Some (ctx,term) -> state_whnf sg { ctx; term; stack=s2 }
            )
        end
    end

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

and are_convertible_lst sg : (term*term) list -> bool = function
  | [] -> true
  | (t1,t2)::lst ->
    begin
      match (
        if term_eq t1 t2 then Some lst
        else
          match whnf sg t1, whnf sg t2 with
          | Kind, Kind | Type _, Type _ -> Some lst
          | Const (_,m,v), Const (_,m',v') when ( ident_eq v v' && ident_eq m m' ) -> Some lst
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

(* One-Step Reduction *)
let rec state_one_step (sg:Signature.t) : state -> state option = function
  (* Weak heah beta normal terms *)
  | { term=Type _ }
  | { term=Kind }
  | { term=Pi _ }
  | { term=Lam _; stack=[] } -> None
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (_,_,n); stack } ->
    if n < LList.len ctx then
      state_one_step sg { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    else
      None
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    Some { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s }
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
    state_one_step sg { ctx; term=f; stack=List.rev_append tl' s }
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,m,v); stack } ->
    begin
      match Signature.get_dtree sg l m v with
      | None -> None
      | Some (i,g) ->
        begin
          match split_stack i stack with
          | None -> None
          | Some (s1,s2) ->
            ( match gamma_rw sg are_convertible snf state_whnf s1 g with
              | None -> None
              | Some (ctx,term) -> Some { ctx; term; stack=s2 }
            )
        end
    end

let one_step sg t =
  map_opt term_of_state (state_one_step sg { ctx=LList.nil; term=t; stack=[] })
