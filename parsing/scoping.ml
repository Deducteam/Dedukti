open Kernel.Basic
open Kernel.Term
open Kernel.Rule
open Preterm

exception Scoping_error of loc * string

let get_db_index ctx id =
  let rec aux n = function
    | [] -> None
    | x :: _ when ident_eq id x -> Some n
    | _ :: lst -> aux (n + 1) lst
  in
  aux 0 ctx

let empty = mk_ident ""

let rec t_of_pt md (ctx : ident list) (pte : preterm) : term =
  let t_of_pt = t_of_pt md in
  match pte with
  | PreType l -> mk_Type l
  | PreId (l, id) -> (
      match get_db_index ctx id with
      | None -> mk_Const l (mk_name md id)
      | Some n -> mk_DB l id n)
  | PreQId (l, cst) -> mk_Const l cst
  | PreApp (f, a, args) ->
      mk_App (t_of_pt ctx f) (t_of_pt ctx a) (List.map (t_of_pt ctx) args)
  | PrePi (l, None, a, b) ->
      mk_Arrow l (t_of_pt ctx a) (t_of_pt (empty :: ctx) b)
  | PrePi (l, Some x, a, b) -> mk_Pi l x (t_of_pt ctx a) (t_of_pt (x :: ctx) b)
  | PreLam (l, id, None, b) -> mk_Lam l id None (t_of_pt (id :: ctx) b)
  | PreLam (l, id, Some a, b) ->
      mk_Lam l id (Some (t_of_pt ctx a)) (t_of_pt (id :: ctx) b)

let scope_term md ctx (pte : preterm) : term =
  t_of_pt md (List.map (fun (_, x, _) -> x) ctx) pte

(******************************************************************************)

type pre_context = preterm option context

(* [get_vars_order vars p] traverses the pattern [p] from left to right and
 * builds the list of variables, turning jokers into unapplied fresh variables.
 * Return false as second argument if some variables never occur (warning needed). *)
let get_vars_order (vars : pcontext) (ppat : prepattern) :
    pre_context * bool * bool =
  let nb_jokers = ref 0 in
  let has_brackets = ref false in
  let get_fresh_name () =
    incr nb_jokers;
    mk_ident ("?_" ^ string_of_int !nb_jokers)
  in
  let is_a_var id1 =
    let rec aux = function
      | [] -> None
      | ((l, id2), ty) :: _ when ident_eq id1 id2 -> Some (l, id2, ty)
      | _ :: lst -> aux lst
    in
    aux vars
  in
  let rec aux (bvar : ident list) (ctx : pre_context) :
      prepattern -> pre_context = function
    | PPattern (_, None, id, pargs) ->
        if List.exists (ident_eq id) bvar then
          List.fold_left (aux bvar) ctx pargs
        else
          let new_ctx =
            match is_a_var id with
            | Some l when not (List.exists (fun (_, a, _) -> ident_eq id a) ctx)
              ->
                l :: ctx
            | _ -> ctx
          in
          List.fold_left (aux bvar) new_ctx pargs
    | PPattern (_, Some _, _, pargs) -> List.fold_left (aux bvar) ctx pargs
    | PLambda (_, x, pp) -> aux (x :: bvar) ctx pp
    | PCondition _ ->
        has_brackets := true;
        ctx
    | PJoker (l, _) -> (l, get_fresh_name (), None) :: ctx
    | PApp plist -> List.fold_left (aux bvar) ctx plist
  in
  let ordered_ctx = aux [] [] ppat in
  ( ordered_ctx,
    List.length ordered_ctx <> List.length vars + !nb_jokers,
    !has_brackets )

let p_of_pp md (ctx : ident list) (ppat : prepattern) : pattern =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    mk_ident ("?_" ^ string_of_int !nb_jokers)
  in
  let rec aux (ctx : ident list) : prepattern -> pattern = function
    | PPattern (l, None, id, pargs) -> (
        match get_db_index ctx id with
        | Some n -> Var (l, id, n, List.map (aux ctx) pargs)
        | None -> Pattern (l, mk_name md id, List.map (aux ctx) pargs))
    | PPattern (l, Some md, id, pargs) ->
        Pattern (l, mk_name md id, List.map (aux ctx) pargs)
    | PLambda (l, x, pp) -> Lambda (l, x, aux (x :: ctx) pp)
    | PCondition pte -> Brackets (t_of_pt md ctx pte)
    | PJoker (l, pargs) -> (
        let id = get_fresh_name () in
        match get_db_index ctx id with
        | Some n -> Var (l, id, n, List.map (aux ctx) pargs)
        | None -> assert false)
    (* The cleaning of prepatterns suppress all the PApp *)
    | PApp _ -> assert false
  in
  aux ctx (clean_pre_pattern ppat)

(******************************************************************************)

let scope_rule md ((l, pname, pctx, md_opt, id, pargs, pri) : prule) :
    partially_typed_rule =
  let top = PPattern (l, md_opt, id, pargs) in
  let ctx, unused_vars, has_brackets = get_vars_order pctx top in
  if unused_vars then (
    Debug.(debug d_warn "Local variables in the rule:\n%a\nare not used (%a)")
      pp_prule
      (l, pname, pctx, md_opt, id, pargs, pri)
      pp_loc l;
    if has_brackets then
      raise
      @@ Scoping_error
           ( l,
             "Unused variables in context may create scoping ambiguity in \
              bracket" ));
  let idents = List.map (fun (_, x, _) -> x) ctx in
  let b, id =
    match pname with
    | None ->
        let id = Format.sprintf "%s!%d" (string_of_ident id) (fst (of_loc l)) in
        (false, mk_ident id)
    | Some (_, id) -> (true, id)
  in
  let name = Gamma (b, mk_name md id) in
  let rec ctx_of_pctx ctx acc = function
    | [] -> acc
    | (l, x, ty) :: tl ->
        ctx_of_pctx (x :: ctx) ((l, x, map_opt (t_of_pt md ctx) ty) :: acc) tl
  in
  {
    name;
    ctx = ctx_of_pctx [] [] (List.rev ctx);
    pat = p_of_pp md idents top;
    rhs = t_of_pt md idents pri;
  }
