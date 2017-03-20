open Basic
open Preterm
open Term
open Rule

let name = ref qmark

let get_db_index ctx id =
  let rec aux n = function
    | [] -> None
    | x::_ when (ident_eq id x) -> Some n
    | _::lst -> aux (n+1) lst
  in aux 0 ctx

let empty = hstring ""

let rec t_of_pt (ctx:ident list) (pte:preterm) : term =
  match pte with
    | PreType l    -> mk_Type l
    | PreId (l,id) ->
        begin
          match get_db_index ctx id with
            | None   -> mk_Const l !name id
            | Some n -> mk_DB l id n
        end
    | PreQId (l,md,id) -> mk_Const l md id
    | PreApp (f,a,args) ->
        mk_App (t_of_pt ctx f) (t_of_pt ctx a) (List.map (t_of_pt ctx) args)
    | PrePi (l,None,a,b) -> mk_Arrow l (t_of_pt ctx a) (t_of_pt (empty::ctx) b)
    | PrePi (l,Some x,a,b) -> mk_Pi l x (t_of_pt ctx a) (t_of_pt (x::ctx) b)
    | PreLam  (l,id,None,b) -> mk_Lam l id None (t_of_pt (id::ctx) b)
    | PreLam  (l,id,Some a,b) ->
        mk_Lam l id (Some (t_of_pt ctx a)) (t_of_pt (id::ctx) b)

let scope_term ctx (pte:preterm) : term =
  t_of_pt (List.map (fun (_,x,_) -> x) ctx) pte

(******************************************************************************)

(* [get_vars_order vars p] traverses the pattern [p] from left to right and
 * builds the list of variables, taking jokers as variables. *)
let get_vars_order (vars:pcontext) (ppat:prepattern) : untyped_context =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    hstring ("?_" ^ string_of_int !nb_jokers)
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
      begin
        if List.exists (ident_eq id) bvar then
          List.fold_left (aux bvar) ctx pargs
        else
          match is_a_var id with
          | Some l ->
            if List.exists (fun (_,a) -> ident_eq id a) ctx then
              List.fold_left (aux bvar) ctx pargs
            else
              let ctx2 = (l,id)::ctx in
              List.fold_left (aux bvar) ctx2 pargs
          | None -> List.fold_left (aux bvar) ctx pargs
      end
    | PPattern (l,Some md,id,pargs) -> List.fold_left (aux bvar) ctx pargs
    | PLambda (l,x,pp) -> aux (x::bvar) ctx pp
    | PCondition _ -> ctx
    | PJoker _ -> (dloc,get_fresh_name ())::ctx
  in
  aux [] [] ppat

let p_of_pp (ctx:ident list) (ppat:prepattern) : pattern =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    hstring ("?_" ^ string_of_int !nb_jokers)
  in
  let rec aux (ctx:ident list): prepattern -> pattern = function
    | PPattern (l,None,id,pargs) ->
      begin
        match get_db_index ctx id with
        | Some n -> Var (l,id,n,List.map (aux ctx) pargs)
        | None -> Pattern (l,!name,id,List.map (aux ctx) pargs)
      end
    | PPattern (l,Some md,id,pargs) -> Pattern (l,md,id,List.map (aux ctx) pargs)
    | PLambda (l,x,pp) -> Lambda (l,x, aux (x::ctx) pp)
    | PCondition pte -> Brackets (t_of_pt ctx pte)
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

let scope_rule (l,pname,pctx,md_opt,id,pargs,pri,ruletype:prule) : untyped_rule * ruletype =
  let top = PPattern(l,md_opt,id,pargs) in
  let ctx = get_vars_order pctx top in
  let idents = List.map snd ctx in
  let md = match md_opt with | None -> Env.get_name () | Some md -> md in
  let b,id =
    match pname with
    | None ->
      let id = Format.sprintf "%s!%d" (string_of_ident id) (fst (of_loc l)) in
      (false,(hstring id))
    | Some id -> (true,id)
  in
  let name = Gamma(b,md,id) in
  let rule = {name ; ctx= ctx; pat = p_of_pp idents top; rhs = t_of_pt idents pri}  in
  if List.length ctx <> List.length pctx then
    debug 1 "Warning: local variables in the rule %a are not used"
      Pp.print_untyped_rule (rule, ruletype);
  rule, ruletype

