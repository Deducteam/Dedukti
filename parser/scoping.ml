open Basics
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

let scope_term (ctx:context) (pte:preterm) : term =
  t_of_pt (List.map (fun (_,x,_) -> x) ctx) pte

(******************************************************************************)

let p_of_pp (ctx:ident list) : prepattern -> pattern =
  let rec aux k ctx = function
    | PPattern (l,None,id,pargs) ->
        let args = List.map (aux k ctx) pargs in
        ( match get_db_index ctx id with
            | Some n -> Var (l,id,n,args)
            | None -> Pattern (l,!name,id,args)
        )
    | PPattern (l,Some md,id,args) -> Pattern (l,md,id,List.map (aux k ctx) args)
    | PLambda (l,x,p) -> Lambda (l,x,aux (k+1) (x::ctx) p)
    | PCondition pte -> Brackets (t_of_pt ctx pte)
    | PJoker l -> Errors.fail l "Unimplemeted feature '_'."
  in aux 0 ctx

let scope_pattern (ctx:context) (pp:prepattern) : pattern =
  p_of_pp (List.map (fun (_,x,_) -> x) ctx) pp

(******************************************************************************)

let scope_context pctx =
  let aux ctx0 = function
    | PDecl (l,x,ty) -> (l,x,scope_term ctx0 ty)::ctx0
    | PDef _ -> failwith "Unimplemented: Definition in rule context"
  in
  List.fold_left aux [] pctx

let scope_rule (l,pctx,id,pargs,pri) =
  let ctx = scope_context pctx in
  let pat = scope_pattern ctx (PPattern(l,None,id,pargs)) in
  let ri = scope_term ctx pri in
    (ctx,pat,ri)
