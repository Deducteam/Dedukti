open Basic
open Format
open Rule
open Term
open Dtree
open Matching
open Ac

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
  ctx   : env;    (* context *)
  term  : term;   (* term to reduce *)
  stack : stack;  (* stack *)
}
and stack = state list

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx 0 term ) in
  mk_App2 t (List.map term_of_state stack)

let state_of_term t = {ctx=LList.nil;term=t;stack=[]}

(* Pretty Printing *)

(* Do we need these ?*)
(*
let pp_state fmt st =
  fprintf fmt "{ctx} {%a} {stack[%i]}\n" pp_term st.term (List.length st.stack)

let pp_stack fmt stck = fprintf fmt "[\n%a]\n" (pp_list "" pp_state) stck
*)

let pp_env fmt (ctx:env) =
  pp_list ", " pp_term fmt (List.map Lazy.force (LList.lst ctx))

let pp_stack fmt (st:stack) =
  fprintf fmt "[ %a ]\n" (pp_list "\n | " pp_term) (List.map term_of_state st)

let pp_state ?(if_ctx=true) ?(if_stack=true) fmt { ctx; term; stack } =
  if if_ctx
  then fprintf fmt "{ctx=[%a];@." pp_env ctx
  else fprintf fmt "{ctx=[...](%i);@." (LList.len ctx);
  fprintf fmt "term=%a;@." pp_term term;
  if if_stack
  then fprintf fmt "stack=%a}@." pp_stack stack
  else fprintf fmt "stack=[...]}@.";
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

(* ********************* *)

type rw_strategy         = Signature.t -> term -> term
type rw_state_strategy   = Signature.t -> state -> state
type convertibility_test = Signature.t -> term -> term -> bool

(* Problem convertion *)

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
  Matching.mk_matching_problem convert convert_ac_sets problem

(* AC related operations *)

let filter_neutral conv sg l cst terms =
  match Signature.get_algebra sg l cst with
  | ACU neu ->
     (match List.filter (fun x -> not (conv neu x)) terms with
      | [] -> [neu] | s -> s)
  | _ -> terms

let rec unflatten sg l cst = function
  | []     -> Signature.get_neutral sg l cst
  | [t]    -> t
  | t1::tl -> mk_App (mk_Const dloc cst) t1 [(unflatten sg l cst tl)]

let ac_to_comb sg t = match t with
  | App( Const (l,cst), a1, a2::remain_args)
       when Signature.is_AC sg l cst ->
     mk_App2 (unflatten sg l cst (flatten_AC_term cst t)) remain_args
  | t -> t
  
(*       AC manipulating functions     *)

(* Unfolds all occurence of the AC(U) symbol in the stack
 * Removes occurence of neutral element.  *)
let flatten_AC_stack sg strategy convertible
                     (l:loc) (cst:name) (stack:stack) : stack =
  let rec flatten acc = function
    | [] -> acc
    | st :: tl ->
       match strategy sg st with
       | { ctx; term=Const (l',cst'); stack=(st1::st2::[]) }
            when name_eq cst cst' ->
          flatten acc (st1 :: st2 :: tl)
       | whnf_st -> flatten (whnf_st::acc) tl
  in
  let stack = flatten [] stack in
  match Signature.get_algebra sg l cst with
  | ACU neu -> List.filter (fun st -> not (convertible sg (term_of_state st) neu)) stack
  | _ -> stack

let to_comb sg l cst ctx stack =
  let rec f = function
    | []     -> {ctx=LList.nil;term=Signature.get_neutral sg l cst;stack=[]}
    | [t]    -> t
    | t1::t2::tl -> f ({ ctx; term=mk_Const l cst; stack=[t1;t2]} :: tl)
  in
  f stack

let comb_state_shape_if_AC sg strategy convertible : state -> state = function
  | { ctx; term=Const (l,cst); stack=(s1::s2::rstack) } when Signature.is_AC sg l cst ->
     let nstack = flatten_AC_stack sg strategy convertible l cst [s1;s2] in
     let s = to_comb sg l cst ctx nstack in
     (match rstack with [] -> s | l ->  {s with stack=(s.stack@l)})
  | st -> st

let comb_term_shape_if_AC (sg:Signature.t)
                        (convertible:convertibility_test) : term -> term = function
  | App(Const (l,cst), a1, a2::remain_args) when Signature.is_AC sg l cst ->
     let id_comp = Signature.get_id_comparator sg in
     let args = flatten_AC_terms cst [a1;a2] in
     let args = filter_neutral (convertible sg) sg l cst args in
     let args = List.sort (compare_term id_comp) args in
     let _ = assert (List.length args > 0) in
     mk_App2 (unflatten sg l cst args) remain_args
  | t -> t

let rec find_case (flattenner:loc->name->stack->stack)
    (st:state) (case:case) : stack option =
  match st, case with
  | { ctx; term=t; stack } , CConst (nargs,cst,true)
    (* TODO: check that this case is used properly !  *)
    (* is_pos (+ 1 i) --> T. #SNF is_pos 1.  *)
    when List.length stack == nargs - 2 (* should we check for type of t ? *) ->
    let new_st = {st with stack=[]} in
    let new_stack = flattenner dloc cst [new_st] in
    Some ({ctx   = ctx;
           term  = mk_Const dloc cst;
           stack = new_stack } :: stack)
      
  | { ctx; term=Const (l,cst); stack=t1::t2::s } , CConst (nargs,cst',true)
    when name_eq cst cst' && nargs == List.length s + 2 ->
    Some ({st with stack = flattenner l cst [t1;t2]}::s)
      
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
            | Some s -> let new_stack = List.rev_append stack_acc tl in (* Remove hd from stack *)
                        let new_state = {state with stack=new_stack} in
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
             ?rewrite:(rewrite=true) (stack:stack) :  dtree -> (env*term) option = function
  | Fetch (i,case,dt_suc,dt_def) -> (* Fetch case from AC-headed i-th state *)
    let rec split_ith acc i l = match i,l with
      | 0, h::t -> (acc,h,t)
      | i, h::t -> split_ith (h::acc) (i-1) t
      | _ -> assert false
    in
    let (stack_h, arg_i, stack_t) = split_ith [] i stack in
    assert (match arg_i.term with Const(l,cst) -> Signature.is_AC sg l cst | _ -> false);
    let new_cases = (* Generate all possible pick for the fetch *)
      List.map
        (function
          | g, new_s, [] -> (List.rev_append stack_h (new_s::stack_t  ), g)
          | g, new_s, s  -> (List.rev_append stack_h (new_s::stack_t@s), g) )
        (fetch_case sg (flatten_AC_stack sg strategy convertible) arg_i case dt_suc dt_def) in
    gamma_rw_list sg convertible forcing strategy new_cases (* ... try them all *)
  | ACEmpty (i, dt_suc, dt_def) ->
    begin
      match List.nth stack i with
      | {ctx; term=Const(l,cst); stack=st} ->
        begin
          assert (Signature.is_AC sg l cst);
          if st == []
          then gamma_rw sg convertible forcing strategy stack dt_suc
          else bind_opt (gamma_rw sg convertible forcing strategy stack) dt_def
        end
      | _ -> assert false
    end
  | Switch (i,cases,def) ->
    let arg_i = List.nth stack i in
    let arg_i = if rewrite then strategy sg arg_i else arg_i in
    let new_cases =
      List.map
        (fun (g,l) -> ( (match l with |[] -> stack | s -> stack@s), g))
        (find_cases (flatten_AC_stack sg strategy convertible) arg_i cases def) in
    gamma_rw_list sg convertible forcing strategy new_cases
  | Test (problem, cstr, right, def) ->
    let pb = convert_problem stack problem in (* Convert problem with the stack *)
    begin
      match Matching.solve_problem (forcing sg) (convertible sg) pb with
      | None -> bind_opt (gamma_rw sg convertible forcing strategy stack) def
      | Some subst ->
        begin
          let aux e acc = match e with None -> assert false | Some e -> e :: acc in
          let ctx_list = Array.fold_right aux subst [] in
          let ctx = LList.make ~len:(Array.length subst) ctx_list in
          if List.exists
              (fun (i,t2) ->
                 let t1 = mk_DB dloc dmark i in
                 not (convertible sg (term_of_state { ctx; term=t1; stack=[] })
                        (term_of_state { ctx; term=t2; stack=[] })) )
              cstr
          then failwith "Error while reducing a term: a guard was not satisfied."
          else Some (ctx, right)
        end
    end

and gamma_head_rw (sg:Signature.t)
                  (convertible:convertibility_test)
                  (forcing:rw_strategy)
                  (strategy:rw_state_strategy) (s:state) (t:dtree) : state option =
  match s, t with
  | { ctx; term=Const(l,cst); stack }, Switch (0,cases,None) ->
     let rec f = function
       | []      -> None
       | ((CConst(nargs,_,_),_) as c) :: tl ->
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
  | { ctx; term=Const (l,cst); stack } ->
     begin
       match Signature.get_dtree sg ~select:(!selection) l cst with
       | None    -> comb_state_shape_if_AC sg state_whnf are_convertible st
       | Some tr -> begin
           match gamma_head_rw sg are_convertible snf state_whnf st tr with
           | None -> comb_state_shape_if_AC sg state_whnf are_convertible st
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
     comb_term_shape_if_AC sg are_convertible res
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
          | Const (_,n), Const (_,n') when ( name_eq n n' ) -> Some lst
          | DB (_,_,n), DB (_,_,n') when ( n==n' ) -> Some lst
          | App (Const(l,cst), _, _),
            App (Const(l',cst'), _, _) when Signature.is_AC sg l cst ->
             (* TODO: Replace this with less hardcore criteria: put all terms in whnf
              * then look at the heads to match arguments with one another. *)
             if name_eq cst cst' then
               match snf sg t1, snf sg t2 with
               | App (Const(l ,cst2 ), a , args ),
                 App (Const(l',cst2'), a', args') ->
                  if name_eq cst2 cst && name_eq cst2' cst &&
                       name_eq cst2 cst' && name_eq cst2' cst' then
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
     | { ctx; term=Const(l,name); stack } ->
        begin
          match Signature.get_dtree sg ~select:(!selection) l name with
          | None    -> (red, comb_state_shape_if_AC sg state_whnf are_convertible st)
          | Some tr -> begin
              match gamma_head_rw sg are_convertible snf state_whnf st tr with
              | None -> (red, comb_state_shape_if_AC sg state_whnf are_convertible st)
              | Some st -> state_one_step sg (red-1,st)
            end
        end
    end

let nsteps sg n t = term_of_state (snd (state_one_step sg (n, state_of_term t)))

let reduction sg strategy te =
  match strategy with
  | Hnf      -> hnf    sg te
  | Snf      -> snf    sg te
  | Whnf     -> whnf   sg te
  | NSteps n when n > 0 -> nsteps sg n te
  | NSteps n -> te
