open Basic
open Format
open Rule
open Term
open Dtree
open Matching
open Ac


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

let pp_env fmt (ctx:env) =
  pp_list ", " pp_term fmt (List.map Lazy.force (LList.lst ctx))


(* A state {ctx; term; stack} is the state of an abstract machine that
represents a term where [ctx] is a ctx that contains the free variables
of [term] and [stack] represents the terms that [term] is applied to. *)
type state =
  {
    ctx   : env;    (* context *)
    term  : term;   (* term to reduce *)
    stack : stack;  (* stack *)
    reduc : state ref option;
    (* Pointer to a state in a more reduced form representing the same term.
       [None] means the term is already in normal form.  *)
  }
and stack = state list

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx term ) in
  mk_App2 t (List.map term_of_state stack)

let st_cnt = ref 0

(** Creates a fresh state with reduc pointing to itself. *)
let mk_state ctx term stack =
  let rec t = { ctx; term; stack; reduc = Some (ref t)} in
  incr st_cnt;
  Debug.(debug d_debug "States: %i -> %a" !st_cnt pp_term (term_of_state t) );
  t

let mk_final ctx term stack = { ctx; term; stack; reduc=None}

(* Pretty Printing *)

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
  match st.reduc with
  | None -> assert false
  | Some reduc ->
    let st' = { ctx; term; stack; reduc=Some reduc } in
    reduc := st';
    st'

(** Creates a (fresh) final state from given state and redirect pointer to it. *)
let rec mk_reduc_final st ctx term stack =
  match st.reduc with
    | None   -> assert false
    | Some r ->
      let st' = mk_final ctx term stack in
      r := st';
      st'

(** Sets [st'] as a reduc of [st] and returns it *)
let set_reduc_state st st' =
  if st == st' then st
  else mk_reduc_state st st'.ctx st'.term st'.stack

let rec set_reduc_final st st' = mk_reduc_final st st'.ctx st'.term st'.stack

let state_of_term t = mk_state LList.nil t []

exception Not_convertible




(* ********************* *)

type rw_strategy         = Signature.t -> term -> term
type rw_state_strategy   = Signature.t -> state -> state
type convertibility_test = Signature.t -> term -> term -> bool


(*******      AC manipulating functions   *******)

let filter_neutral conv sg l cst terms =
  match Signature.get_algebra sg l cst with
  | ACU neu ->
    (match List.filter (fun x -> not (conv neu x)) terms with
     | [] -> [neu] | s -> s)
  | _ -> terms

(* Unfolds all occurences of the AC(U) symbol in the stack
 * Removes occurence of neutral element.  *)
let flatten_AC_stack sg strategy convertible
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
  | ACU neu -> List.filter (fun st -> not (convertible sg (term_of_state st) neu)) stack
  | _ -> stack

(** Builds a comb-shaped AC term from a list of arguments. *)
let to_comb sg l cst ctx stack =
  let rec f = function
    | []         -> state_of_term (Signature.get_neutral sg l cst)
    | [t]        -> t
    | t1::t2::tl -> f ((mk_state ctx (mk_Const l cst) [t1;t2]) :: tl)
  in
  f stack

let comb_state_shape_if_AC sg strategy convertible st =
  match st with
  | { ctx; term=Const (l,cst); stack=(s1::s2::rstack) } when Signature.is_AC sg l cst ->
    let nstack = flatten_AC_stack sg strategy convertible l cst [s1;s2] in
    let s = to_comb sg l cst ctx nstack in
    let rstack = (match rstack with [] -> s.stack | l -> s.stack@l) in
    mk_reduc_state st s.ctx s.term rstack
  | st -> st

let comb_term_shape_if_AC (sg:Signature.t)
                          (convertible:convertibility_test) : term -> term = function
  | App(Const (l,cst), a1, a2::remain_args) when Signature.is_AC sg l cst ->
     let id_comp = Signature.get_id_comparator sg in
     let args = flatten_AC_terms cst [a1;a2] in
     let args = filter_neutral (convertible sg) sg l cst args in
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

(* TODO implement the stack as an array ? (the size is known in advance). *)
and gamma_rw (sg:Signature.t) (convertible:convertibility_test)
             (forcing:rw_strategy) (strategy:rw_state_strategy)
             (stack:stack) :  dtree -> (env*term) option = function
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
        (match s with
          | [] -> new_s::stack_t
          | s  -> new_s::stack_t@s), g in
    let cases =  (* Generate all possible pick for the fetch... *)
      fetch_case sg (flatten_AC_stack sg strategy convertible) arg_i case dt_suc dt_def in
    let new_cases = List.map process cases in
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
    let arg_i = strategy sg (List.nth stack i) in
    let new_cases =
      List.map
        (fun (g,l) -> ( (match l with |[] -> stack | s -> stack@s), g))
        (find_cases (flatten_AC_stack sg strategy convertible) arg_i cases def) in
    gamma_rw_list sg convertible forcing strategy new_cases
  | Test (rule_name, problem, cstr, right, def) ->
    let pb = convert_problem stack problem in (* Convert problem with the stack *)
    begin
      match solve_problem (forcing sg) (convertible sg) (forcing sg) pb with
      | None ->
        bind_opt (gamma_rw sg convertible forcing strategy stack) def
      | Some subst ->
        begin
          let aux e acc = match e with None -> assert false | Some e -> e :: acc in
          let ctx_list = Array.fold_right aux subst [] in
          let ctx = LList.make ~len:(Array.length subst) ctx_list in
          if List.exists
              (fun (i,t2) ->
                 let t1 = mk_DB dloc dmark i in
                 not (convertible sg (term_of_state (mk_state ctx t1 []))
                        (term_of_state (mk_state ctx t2[] ))) )
              cstr
          then failwith "Error while reducing a term: a guard was not satisfied."
          else Some (ctx, right)
        end
    end


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
  match st.reduc with
  | Some reduc when !reduc != st ->
    begin
      Debug.(debug d_reduce "Jumping %a ---> %a" simpl_pp_state st simpl_pp_state !reduc);
      state_whnf sg !reduc
    end
  | None -> st
  | _ ->
    let _ = Debug.(debug d_reduce "Reducing %a" simpl_pp_state st) in
    let rec_call c t s = state_whnf sg (mk_reduc_state st c t s) in
  match st with
  (* Weak heah beta normal terms *)
  | { term=Type _ }
  | { term=Kind }
  | { term=Pi _ }
  | { term=Lam _; stack=[] } -> set_reduc_final st st
  (* DeBruijn index: environment lookup *)
  | { ctx; term=DB (l,x,n); stack } ->
    if n < LList.len ctx
    then rec_call LList.nil (Lazy.force (LList.nth ctx n)) stack
    else mk_reduc_final st LList.nil (mk_DB l x (n-LList.len ctx)) stack
  (* Beta redex *)
  | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
    if not !beta
    then set_reduc_final st st
    else rec_call (LList.cons (lazy (term_of_state p)) ctx) t s
  (* Application: arguments go on the stack *)
  | { ctx; term=App (f,a,lst); stack=s } ->
    (* rev_map + rev_append to avoid map + append*)
    let tl' = List.rev_map (fun t -> mk_state ctx t []) (a::lst) in
    rec_call ctx f (List.rev_append tl' s)
  (* Potential Gamma redex *)
  | { ctx; term=Const (l,cst); stack } ->
    let trees = Signature.get_dtree sg !selection l cst in
    match find_dtree (List.length stack) trees with
    | None -> set_reduc_final st (comb_state_shape_if_AC sg state_whnf are_convertible st)
    | Some (ar, tree) ->
      let s1, s2 = split_list ar stack in
      let s1 =
        if Signature.is_AC sg l cst && ar > 1
        then
          match s1 with
          | t1 :: t2 :: tl ->
            let flat = flatten_AC_stack sg state_whnf are_convertible l cst [t1;t2] in
            (mk_state ctx (mk_Const l cst) flat):: tl
          | _ -> assert false
        else s1 in
      match gamma_rw sg are_convertible snf state_whnf s1 tree with
      | None -> set_reduc_final st (comb_state_shape_if_AC sg state_whnf are_convertible st)
      | Some (ctx,term) -> rec_call ctx term s2

(* ********************* *)

(* Weak Head Normal Form *)
and whnf sg term = term_of_state ( state_whnf sg (state_of_term term) )

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

and are_convertible_lst sg : (term*term) list -> bool =
  function
  | [] -> true
  | (t1,t2)::lst ->
    are_convertible_lst sg
      begin
        if term_eq t1 t2 then lst
        else
          let t1 = whnf sg t1 in
          let t2 = whnf sg t2 in
          match t1, t2 with
          | Kind, Kind | Type _, Type _ -> lst
          | Const (_,n), Const (_,n') when ( name_eq n n' ) -> lst
          | DB (_,_,n), DB (_,_,n') when ( n==n' ) -> lst
          | App (Const(l,cst), _, _), App (Const(l',cst'), _, _)
            when Signature.is_AC sg l cst && name_eq cst cst' ->
            begin
              (* TODO: Replace this with less hardcore criteria: put all terms in whnf
               * then look at the heads to match arguments with one another. *)
              match snf sg t1, snf sg t2 with
               | App (Const(l ,cst2 ), a , args ),
                 App (Const(l',cst2'), a', args')
                 when name_eq cst2 cst && name_eq cst2' cst &&
                      name_eq cst2 cst' && name_eq cst2' cst' ->
                 List.fold_left2 (fun l a b -> (a,b)::l) ((a,a')::lst) args args'
               | t1', t2' -> (t1',t2') :: lst
            end
          | App (f,a,args), App (f',a',args') ->
            List.fold_left2 (fun l a b -> (a,b)::l) ((f,f')::(a,a')::lst) args args'
          | Lam (_,_,_,b), Lam (_,_,_,b') -> ((b,b')::lst)
          | Pi (_,_,a,b), Pi (_,_,a',b') -> ((a,a')::(b,b')::lst)
          | t1, t2 -> ( Debug.(debug d_rule "Not convertible: %a / %a" pp_term t1 pp_term t2 );
                        raise Not_convertible)
    end

(* Convertibility Test *)
and are_convertible sg t1 t2 =
  try are_convertible_lst sg [(t1,t2)]
  with Not_convertible -> false
     | Invalid_argument _ -> false

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
    else
      match st.reduc with
      | None -> (red, st)
      | Some reduc ->
        if !reduc != st then aux (red-1, !reduc)
        else
      match st with
      (* Normal terms *)
      | { term=Type _ }  | { term=Kind } -> (red, st)
      (* Pi types are head normal terms *)
      | { term=Pi _ } when strat <> Snf  -> (red, st)
      (* Strongly normalizing Pi types *)
      | { ctx=ctx; term=Pi(l,x,a,b) } ->
        let (red, a') = aux (red, mk_state ctx a []) in
        let snf_a = term_of_state a' in
        let state_b = mk_state (LList.cons (lazy snf_a) ctx) b [] in
        let (red, b') = aux (red, state_b) in
        let snf_b = term_of_state b' in
        (red, mk_reduc_state st ctx (mk_Pi l x snf_a snf_b) [])

      (* Beta redex *)
      | { ctx; term=Lam (_,_,_,t); stack=p::s } when !beta ->
        aux (red-1, mk_reduc_state st (LList.cons (lazy (term_of_state p)) ctx) t s)
      (* Not a beta redex (or beta disabled) *)
      | { term=Lam _ } when strat == Whnf -> (red, st)
      (* Not a beta redex (or beta disabled) but keep looking for normal form *)
      | { ctx; term=Lam(l,x,ty_opt,t); stack } ->
        begin
          match term_of_state st with
          | Lam(_,_,_,t') ->
            let (red, st_t) = aux (red, mk_reduc_state st LList.nil t' []) in
            let t' = term_of_state st_t in
            begin
              match strat, ty_opt with
              | Snf, Some ty ->
                let red, ty = aux (red, mk_state ctx ty []) in
                (red, mk_reduc_state st LList.nil
                   (mk_Lam l x (Some (term_of_state ty)) t') stack)
              | _ -> (red, mk_reduc_state st ctx (mk_Lam l x ty_opt t') stack)
            end
          | _ -> assert false
        end

      (* DeBruijn index: environment lookup *)
      | { ctx; term=DB (_,_,n); stack } when n < LList.len ctx ->
        aux (red, mk_reduc_state st LList.nil (Lazy.force (LList.nth ctx n)) stack)
      (* DeBruijn index: out of environment *)
      | { term=DB _ } -> (red, st)

      (* Application: arguments go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s } when strat <> Snf ->
        let tl' = List.rev_map ( fun t -> mk_state ctx t [] ) (a::lst) in
        aux (red, mk_reduc_state st ctx f (List.rev_append tl' s) )
      (* Application: arguments are reduced then go on the stack *)
      | { ctx; term=App (f,a,lst); stack=s } ->
        let redc = ref red in
        let reduce t =
          let new_redc, st = aux (!redc, mk_state ctx t []) in
          redc := new_redc;
          st in
        let new_stack = List.rev_append (List.rev_map reduce (a::lst)) s in
        aux (!redc, mk_reduc_state st ctx f new_stack)

      (* Potential Gamma redex *)
      | { ctx; term=Const(l, cst); stack } ->
        let trees = Signature.get_dtree sg !selection l cst in
        match find_dtree (List.length stack) trees with
        | None -> (red,comb_state_shape_if_AC sg state_whnf are_convertible st)
        | Some (ar, tree) ->
          let s1, s2 = split_list ar stack in
          let s1 =
            if Signature.is_AC sg l cst && ar > 1
            then
              match s1 with
              | t1 :: t2 :: tl ->
                let flat = flatten_AC_stack sg state_whnf are_convertible l cst [t1;t2] in
                (mk_state ctx (mk_Const l cst) flat) :: tl
              | _ -> assert false
            else s1 in
          match gamma_rw sg are_convertible snf state_whnf s1 tree with
          | None -> (red,comb_state_shape_if_AC sg state_whnf are_convertible st)
          | Some (ctx,term) -> aux (red-1, mk_reduc_state st ctx term s2)
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
