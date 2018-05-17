open Basic
open Preterm
open Term
open Rule

let get_db_index ctx id =
  let rec aux n = function
    | [] -> None
    | x::_ when (ident_eq id x) -> Some n
    | _::lst -> aux (n+1) lst
  in aux 0 ctx

let empty = mk_ident ""

let rec t_of_pt md (ctx:ident list) (pte:preterm) : term =
  let t_of_pt = t_of_pt md in
  match pte with
    | PreType l    -> mk_Type l
    | PreId (l,id) ->
        begin
          match get_db_index ctx id with
            | None   -> mk_Const l (mk_name md id)
            | Some n -> mk_DB l id n
        end
    | PreQId (l,cst) -> mk_Const l cst
    | PreApp (f,a,args) ->
        mk_App (t_of_pt ctx f) (t_of_pt ctx a) (List.map (t_of_pt ctx) args)
    | PrePi (l,None,a,b) -> mk_Arrow l (t_of_pt ctx a) (t_of_pt (empty::ctx) b)
    | PrePi (l,Some x,a,b) -> mk_Pi l x (t_of_pt ctx a) (t_of_pt (x::ctx) b)
    | PreLam  (l,id,None,b) -> mk_Lam l id None (t_of_pt (id::ctx) b)
    | PreLam  (l,id,Some a,b) ->
        mk_Lam l id (Some (t_of_pt ctx a)) (t_of_pt (id::ctx) b)

let scope_term md ctx (pte:preterm) : term =
  t_of_pt md (List.map (fun (_,x,_) -> x) ctx) pte

(******************************************************************************)

(* [get_vars_order vars p] traverses the pattern [p] from left to right and
 * builds the list of variables, turning jokers into unapplied fresh variables.
 * Return false as second argument if some variables never occur (warning needed). *)
let get_vars_order (vars:pcontext) (ppat:prepattern) : untyped_context*bool =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    mk_ident ("?_" ^ string_of_int !nb_jokers)
  in
  let is_a_var id1 =
    let rec aux = function
      | [] -> None
      | (l,id2)::lst when ident_eq id1 id2 -> Some l
      | _::lst -> aux lst
    in aux vars
  in
  let rec aux (bvar:ident list) (ctx:(loc*ident) list) : prepattern -> untyped_context = function
    | PPattern (_,None,id,pargs) ->
      if List.exists (ident_eq id) bvar
      then List.fold_left (aux bvar) ctx pargs
      else
        let ctx = (
          match is_a_var id with
          | Some l when not (List.exists (fun (_,a) -> ident_eq id a) ctx)
            -> (l,id)::ctx
          | _ -> ctx
        ) in
        List.fold_left (aux bvar) ctx pargs
    | PPattern (l,Some md,id,pargs) -> List.fold_left (aux bvar) ctx pargs
    | PLambda (l,x,pp) -> aux (x::bvar) ctx pp
    | PCondition _ -> ctx
    | PJoker l -> (l, get_fresh_name ()) :: ctx
  in
  let ordered_ctx = aux [] [] ppat in
  ( ordered_ctx , List.length ordered_ctx <> List.length vars + !nb_jokers )

let p_of_pp md (ctx:ident list) (ppat:prepattern) : pattern =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    mk_ident ("?_" ^ string_of_int !nb_jokers)
  in
  let rec aux (ctx:ident list): prepattern -> pattern = function
    | PPattern (l,None,id,pargs) ->
      begin
        match get_db_index ctx id with
        | Some n -> Var (l,id,n,List.map (aux ctx) pargs)
        | None -> Pattern (l,mk_name md id,List.map (aux ctx) pargs)
      end
    | PPattern (l,Some md,id,pargs) -> Pattern (l,mk_name md id,List.map (aux ctx) pargs)
    | PLambda (l,x,pp) -> Lambda (l,x, aux (x::ctx) pp)
    | PCondition pte -> Brackets (t_of_pt md ctx pte)
    | PJoker l ->
      begin
        let id = get_fresh_name () in
        match get_db_index ctx id with
        | Some n -> Var (l,id,n,[])
        | None -> assert false
      end
  in
  aux ctx ppat

(******************************************************************************)

let scope_rule md (l,pname,pctx,md_opt,id,pargs,pri:prule) : untyped_rule =
  let top = PPattern(l,md_opt,id,pargs) in
  let ctx, unused_vars = get_vars_order pctx top in
  if unused_vars
  then Debug.(debug d_warn "Local variables in the rule %a are not used (%a)")
      pp_prule (l,pname,pctx,md_opt,id,pargs,pri) pp_loc l;
  let idents = List.map snd ctx in
  let b,id =
    match pname with
    | None ->
      let id = Format.sprintf "%s!%d" (string_of_ident id) (fst (of_loc l)) in
      (false,(mk_ident id))
    | Some (_, id) -> (true,id)
  in
  let name =
    let md = match pname with
      | Some (Some md, _) -> md
      | _ -> Env.get_name ()
    in
    Gamma(b,mk_name md id)
  in
  { name ;
    ctx= ctx;
    pat = p_of_pp md idents top;
    rhs = t_of_pt md idents pri;
    pol = Both
  }
