open Basic
open Format
open Rule
open Term
open Dtree

type red = {
  select : (Rule.rule_name -> bool) option;
  beta : bool
}

let default = { select = None ; beta = true }

let selection  = ref None

let beta = ref true

let select (red:red) : unit =
  selection := red.select;
  beta := red.beta

type red_strategy = Hnf | Snf | Whnf | NSteps of int

(* State *)

type env = term Lazy.t LList.t

(* A state {ctx; term; stack} is the state of an abstract machine that
represents a term where [ctx] is a ctx that contains the free variables
of [term] and [stack] represents the terms that [term] is applied to. *)
type state = {
  ctx   : env;    (*context*)
  term  : term;   (*term to reduce*)
  stack : stack;  (*stack*)
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
  | (Bracket (i,t2))::tl ->
    let t1 = mk_DB dloc dmark i in
    if convertible sg (term_of_state { ctx; term=t1; stack=[] })
                      (term_of_state { ctx; term=t2; stack=[] })
    then test sg convertible ctx tl
    else
      (*FIXME: if a guard is not satisfied should we fail or only warn the user? *)
      failwith "Error while reducing a term: a guard was not satisfied."


(** Unfolds all occurence of the AC(U) symbol in the stack
  * Removes occurence of neutral element.  *)
let flatten_AC_stack (sg:Signature.t) (strategy:rw_state_strategy)
                     (convertible:convertibility_test)
                     ?not_empty:(not_empty=false)
                     (l:loc) (m:ident) (v:ident) (stack:stack) : stack =
  let rec flatten acc = function
    | [] -> acc
    | st :: tl ->
       let whnf_st = (strategy sg) st in
       match whnf_st with
       | { ctx; term=Const (l',m',v'); stack=(st1::st2::[]) }
            when ident_eq m' m && ident_eq v' v ->
          flatten acc (st1 :: st2 :: tl)
       | _ -> flatten (whnf_st::acc) tl
  in
  let stack = flatten [] stack in
  match Signature.get_staticity sg l m v with
  | Signature.DefinableACU neu ->
     List.filter (fun st -> not (convertible sg (term_of_state st) neu)) stack
  | _ -> stack
  

let rec to_comb sg = function
  | { ctx; term=Const (l,m,v); stack } as st ->
     let rec f = function
       | []     -> {ctx=ctx;term=Signature.get_neutral sg l m v;stack=[]}
       | [t]    -> t
       | t1::tl -> {st with stack=[t1;(f tl)]}
     in
     f stack
  | _ -> assert false


let flatten_AC (sg:Signature.t)
               (strategy:rw_state_strategy)
               (convertible:convertibility_test) : state -> state = function
  | { ctx; term=Const (l,m,v); stack=(s1::s2::rstack) } as st when Signature.is_AC sg l m v ->
     let s = to_comb sg { st with stack=(flatten_AC_stack sg strategy convertible l m v [s1;s2]) } in
     (match rstack with [] -> s | l ->  {s with stack=(s.stack@l)})
  | st -> st

let flatten_SNF_AC_term (sg:Signature.t)
                        (convertible:convertibility_test) : term -> term = function
  | App(Const (l,m,v), a1, a2::remain_args) when Signature.is_AC sg l m v ->
     begin
       let rec flatten acc = function
         | [] -> acc
         | App(Const (_,m',v'), a1', a2'::[]) :: tl
              when ident_eq m' m && ident_eq v' v ->
            flatten acc (a1'::a2'::tl)
         | arg::tl -> flatten (arg::acc) tl
       in
       let args = flatten [] [a1;a2] in
       let args =
         match Signature.get_staticity sg l m v with
         | Signature.DefinableACU neu ->
            (match List.filter (fun x -> not (convertible sg neu x)) args with
             | [] -> [neu] | s -> s)
         | _ -> args
       in
       let id_comp = Signature.get_id_comparator sg in
       let args  = List.sort (compare_term id_comp) args in
       match args, remain_args with
       |    [], _         -> assert false
       | a::[], []        -> a
       | a::[], ra::rargs -> mk_App a ra args
       | a::tl, rargs ->
           let rec to_comb = function
             | [] -> assert false
             | a::[] -> a
             | a::tl -> mk_App (mk_Const l m v) a [to_comb tl]
           in
           mk_App (mk_Const l m v) a ((to_comb tl)::rargs)
     end
  | t -> t

let rec find_case (flattenner:loc->ident->ident->stack->stack)
                  (st:state) (case:case) : stack option =
  match st, case with
  | { ctx; term=Const (l,m,v); stack } , CConst (nargs,m',v',ac_symb) ->
     if ident_eq v v' && ident_eq m m' && List.length stack == nargs
     then
       match ac_symb,stack with
       | true , t1::t2::s -> Some ({st with stack = flattenner l m v [t1;t2]}::s)
       | false, _         -> Some stack
       | _ -> assert false
     else None
  | { ctx; term=DB (l,x,n); stack }, CDB(nargs,n') ->
      assert ( ctx = LList.nil ); (* no beta in patterns *)
      if n==n' && List.length stack == nargs then Some stack else None
  | { ctx; term=Lam (_,_,_,_) }, CLam ->
    begin
      match term_of_state st with (*TODO could be optimized*)
      | Lam (_,_,_,te) -> Some [{ ctx=LList.nil; term=te; stack=[] }]
      | _ -> assert false
    end
  | _ -> None

let rec nbags l n = match l, n with
  | [], n -> let rec f = function 0 -> [] | n -> [] :: (f (n-1)) in [f n]
  | hd::tl, n ->
     let rec dispatch_in_one_bag acc prefix = function
       | [] -> acc
       | bag::obags -> dispatch_in_one_bag
                         ((List.rev_append prefix ((hd::bag)::obags))::acc)
                         (bag::prefix) obags
     in
     let rec dispatch_in_all_bag_sets = function
       | [] -> []
       | h :: t -> List.rev_append (dispatch_in_one_bag [] [] h) (dispatch_in_all_bag_sets t)
     in
     dispatch_in_all_bag_sets (nbags tl n)


let rec fetch_case sg (flattenner:loc->ident->ident->stack->stack)
                   (state:state) (case:case)
                   (dt_suc:dtree) (dt_def:dtree option) : (dtree*state*stack) list =
  let def_s = match dt_def with None -> [] | Some g -> [(g,state,[])] in
  match state, case with
  | {ctx; term=Const(l,m,v); stack=[]   }, Unflatten 0 -> (dt_suc,state,[]) :: def_s
  | {ctx; term=Const(l,m,v); stack=_    }, Unflatten 0 -> def_s
  | {ctx; term=Const(l,m,v); stack=stack}, Unflatten n ->
     let rec to_combed = function
       | [] -> []
       | s :: ls -> 
          try
            let combs = List.map (fun s -> to_comb sg {state with stack=s}) s in
            (dt_suc, {state with stack=[]}, combs) :: (to_combed ls)
          with Signature.SignatureError _ -> to_combed ls
     in
     to_combed (nbags stack n)
  | {ctx; term=Const(l,m,v); stack=stack},_ ->
     let rec f acc stack_acc st = match st, case with
       | [], _ -> acc
       | hd::tl, _ ->
          let new_stack_acc = (hd::stack_acc) in
          let new_acc = match find_case flattenner hd case with
            | None   -> acc
            | Some s -> let new_stack = List.rev_append stack_acc tl in (* Remove hd from stack *)
                        let new_state = {state with stack=new_stack} in
                        (dt_suc,new_state,s)::acc
          in
          f new_acc new_stack_acc tl
     in
     List.rev_append (f [] [] stack) def_s
   | _ -> assert false


let rec find_cases (flattenner:loc->ident->ident->stack->stack)
                  (st:state) (cases:(case * dtree) list)
                  (default:dtree option) : (dtree*stack) list =
  match cases with
  | [] -> (match default with None -> [] | Some g -> [(g,[])])
  | (case,tr)::tl ->
     (
       match find_case flattenner st case with
       | None -> find_cases flattenner st tl default
       | Some stack -> [(tr,stack)]
     )



let rec gamma_rw_list (sg:Signature.t)
                  (convertible:convertibility_test)
                  (forcing:rw_strategy)
                  (strategy:rw_state_strategy) : (stack*dtree) list -> (env*term) option =
  function
  | [] -> None
  | (stack, tree) :: tl ->
     match gamma_rw sg convertible forcing strategy stack tree with
     | None -> gamma_rw_list sg convertible forcing strategy tl
     | Some _ as x -> x

(*TODO implement the stack as an array ? (the size is known in advance).*)
and gamma_rw (sg:Signature.t) (convertible:convertibility_test)
             (forcing:rw_strategy) (strategy:rw_state_strategy)
             ?rewrite:(rewrite=true) : stack -> dtree -> (env*term) option =
  let rec rw stack = function
    | Fetch (i,case,dt_suc,dt_def) ->
       let rec split_ith acc i l = match i,l with
         | 0, h::t -> (acc,h,t)
         | i, h::t -> split_ith (h::acc) (i-1) t
         | _ -> assert false
       in
       let (stack_h, arg_i, stack_t) = split_ith [] i stack in
       assert (match arg_i.term with Const(l,m,v) -> Signature.is_AC sg l m v | _ -> false);
       let new_cases =
         List.map
           (function
            | g, new_s, [] -> (List.rev_append stack_h (new_s::stack_t  ), g)
            | g, new_s, s  -> (List.rev_append stack_h (new_s::stack_t@s), g) )
           (fetch_case sg (flatten_AC_stack sg strategy convertible) arg_i case dt_suc dt_def) in
       gamma_rw_list sg convertible forcing strategy new_cases
    | Switch (i,cases,def) ->
       let arg_i = (List.nth stack i) in
       let arg_i = if rewrite then strategy sg arg_i else arg_i in
       let new_cases =
         List.map
           (function
            | g, [] -> ( stack    , g)
            | g, s  -> ( (stack@s), g) )
           (find_cases (flatten_AC_stack sg strategy convertible) arg_i cases def) in
       gamma_rw_list sg convertible forcing strategy new_cases
    | Test (Syntactic ord, eqs, right, def) ->
       begin
         match get_context_syn sg forcing stack ord with
         | None -> bind_opt (rw stack) def
         | Some ctx ->
            if test sg convertible ctx eqs then Some (ctx, right)
            else bind_opt (rw stack) def
       end
    | Test (MillerPattern lst, eqs, right, def) ->
       begin
         match get_context_mp sg forcing stack lst with
         | None -> bind_opt (rw stack) def
         | Some ctx ->
            if test sg convertible ctx eqs then Some (ctx, right)
            else bind_opt (rw stack) def
       end
  in
  rw

and gamma_head_rw (sg:Signature.t)
                  (convertible:convertibility_test)
                  (forcing:rw_strategy)
                  (strategy:rw_state_strategy) (s:state) (t:dtree) : state option =
  match s, t with
  | { ctx; term=Const(l,m,v); stack }, Switch (0,cases,None) ->
     let rec f = function
       | []      -> None
       | ((CConst(nargs,_,_,_),_) as c) :: tl ->
          begin
            match split_stack nargs stack with
            | None -> f tl
            | Some (s1,s2) ->
               begin
                 let s' = {s with stack=s1} in
                 match gamma_rw sg convertible forcing strategy ~rewrite:false [s']
                                (Switch (0,[c],None)) with
                 | None -> f tl
                 | Some (ctx,term) -> Some { ctx; term; stack=s2 }
               end
          end
       | _ -> assert false
     in
     f cases
  | _ -> assert false


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
 * - when state.term is an AC constant, then state.stack contains no application
 *     of that same constant
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
    if n < LList.len ctx then
      state_whnf sg { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    else
      { ctx=LList.nil; term=(mk_DB l x (n-LList.len ctx)); stack }
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
  | { ctx; term=Const (l,m,v); stack } ->
     begin
       match Signature.get_dtree sg ~select:(!selection) l m v with
       | None    -> flatten_AC sg state_whnf are_convertible st
       | Some tr -> begin
           match gamma_head_rw sg are_convertible snf state_whnf st tr with
           | None -> flatten_AC sg state_whnf are_convertible st
           | Some st -> state_whnf sg st
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
  | App (f,a,lst) ->
     let res = mk_App (snf sg f) (snf sg a) (List.map (snf sg) lst) in
     flatten_SNF_AC_term sg are_convertible res
  | Pi (_,x,a,b) -> mk_Pi dloc x (snf sg a) (snf sg b)
  | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (snf sg) a) (snf sg b)

and are_convertible_lst sg : (term*term) list -> bool = function
  | [] -> true
  | (t1,t2)::lst ->
    begin
      match (
        if term_eq t1 t2 then Some lst
        else
          let t1 = whnf sg t1 in
          let t2 = whnf sg t2 in
          match t1, t2 with
          | Kind, Kind | Type _, Type _ -> Some lst
          | Const (_,m,v), Const (_,m',v') when ( ident_eq v v' && ident_eq m m' ) -> Some lst
          | DB (_,_,n), DB (_,_,n') when ( n==n' ) -> Some lst
          | App (Const(l ,m ,v ), _, _),
            App (Const(l',m',v'), _, _) when Signature.is_AC sg l m v ->
             (* TODO: Replace this with less hardcore criteria: put all terms in whnf
              * then look at the heads to match arguments with one another. *)
             if ident_eq m m' && ident_eq v v' then
               match snf sg t1, snf sg t2 with
               | App (Const(l ,m2 ,v2 ), a , args ),
                 App (Const(l',m2',v2'), a', args') ->
                  if ident_eq m2 m && ident_eq m2' m && ident_eq v2 v' && ident_eq v2' v' then
                    add_to_list2 args args' ((a,a')::lst)
                  else None
               | _ -> None
             else None
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
  (* Weak heah beta normal terms *)
let rec state_one_step (sg:Signature.t) : int*state -> int*state = function
  | (0  , st) as s -> s
  | (red, st) -> begin
     match st with
     | { term=Type _ }
     | { term=Kind }
     | { term=Pi _ } -> (red,st)
     | { ctx=ctx; term=Lam(loc,id,ty,t); stack=[] } ->
        let n',state' = state_one_step sg (red,{ctx=ctx;term=t; stack=[]}) in
        (n', {ctx=ctx; term=mk_Lam loc id ty (term_of_state state'); stack = []})
     (*
    let t' = Subst.shift 1 term in
    begin
      match t' with
      | Lam(loc,id,ty,t) ->
        let (red',st) = state_one_step sg
            (red,{ctx=LList.cons (lazy (mk_DB dloc id 0)) ctx; term=t; stack = []}) in
        (red',{ctx = ctx; term=mk_Lam loc id ty (Subst.unshift 1 (term_of_state st)); stack = []})
      | _ -> assert false
    end *)
     (* DeBruijn index: environment lookup *)
     | { ctx; term=DB (_,_,n); stack } ->
        if n < LList.len ctx then
          state_one_step sg (red,{ ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack })
        else
          (red,st)
     (* Beta redex *)
     | { ctx; term=Lam (loc,id,ty,t); stack=p::s } ->
        if !beta then
          (red-1,{ ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s })
        else
          let (red',state') = state_one_step sg (red,{ctx=ctx;term=t; stack=[]}) in
          (red',{ctx=ctx; term=mk_Lam loc id ty (term_of_state state'); stack = p::s})
  (* Application: arguments go on the stack *)
     | { ctx; term=App (f,a,lst); stack=s } ->
        let aux = ref red in
        (* rev_map + rev_append to avoid map + append*)
        let new_stack =
          List.rev_map
            (fun t ->
              let new_aux,st = state_one_step sg (!aux, {ctx;term=t;stack=[]}) in
              aux := new_aux;
              st)
            (a::lst) in
        state_one_step sg (!aux,{ ctx; term=f; stack=List.rev_append new_stack s })
     (* Potential Gamma redex *)
     | { ctx; term=Const (l,m,v); stack } ->
        begin
          match Signature.get_dtree sg ~select:(!selection) l m v with
          | None    -> (red,flatten_AC sg state_whnf are_convertible st)
          | Some tr -> begin
              match gamma_head_rw sg are_convertible snf state_whnf st tr with
              | None -> (red,flatten_AC sg state_whnf are_convertible st)
              | Some st -> state_one_step sg (red-1,st)
            end
        end
    end


let nsteps sg n t =
  term_of_state (snd (state_one_step sg (n,{ ctx=LList.nil; term=t; stack=[] })))

let reduction sg strategy te =
  match strategy with
  | Hnf      -> hnf    sg te
  | Snf      -> snf    sg te
  | Whnf     -> whnf   sg te
  | NSteps n when n > 0 -> nsteps sg n te
  | NSteps n -> te
