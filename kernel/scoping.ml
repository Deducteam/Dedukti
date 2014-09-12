open Term
open Rule

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
            | None   -> mk_Const l !Env.name id
            | Some n -> mk_DB l id n
        end
    | PreQId (l,md,id) -> mk_Const l md id
    | PreApp (f,a,args) ->
        mk_App (t_of_pt ctx f) (t_of_pt ctx a) (List.map (t_of_pt ctx) args)
    | PrePi (l,None,a,b) -> mk_Arrow l (t_of_pt ctx a) (t_of_pt (empty::ctx) b)
    | PrePi (l,Some x,a,b) -> mk_Pi l x (t_of_pt ctx a) (t_of_pt (x::ctx) b)
    | PreLam  (l,id,a,b) ->
        mk_Lam l id (t_of_pt ctx a) (t_of_pt (id::ctx) b)

let get_bound_var = function
  | BoundVar (l,id,n,[]) -> (l,id,n)
  | p -> Print.fail (get_loc_pat p) "the pattern '%a' is not a bound variable."
           Pp.pp_pattern p

(* The arguments should be disctinct bound variables *)
let get_args l id k args =
  let err () =
    Print.fail l "Ill-formed pattern: the arguments \
                         of the variable '%a' should be disctint bound variables."
      pp_ident id in
  let arr = Array.make k false in
  let aux = function
    | BoundVar (l,id,n,[]) -> (
        assert (n < k);
        if arr.(n) then err () else ( arr.(n) <- true; (l,id,n) )
      )
    | _ -> err ()
  in
    List.map aux args

let is_closed k t =
  let rec aux q = function
  | Kind | Type _ | Const _ -> true
  | DB (_,_,n) -> ( n<q || n>= (k+q) )
  | Lam (_,_,a,b) | Pi (_,_,a,b) -> (aux q a) && (aux (q+1) b)
  | App (f,a,args) -> List.for_all (aux q) (f::a::args)
  in
    aux 0 t

let p_of_pp (ctx:ident list) : prepattern -> pattern =
  let rec aux k ctx = function
    | PPattern (l,None,id,pargs) ->
        let args = List.map (aux k ctx) pargs in
        ( match get_db_index ctx id with
            | Some n ->
                if n<k then BoundVar (l,id,n,args)
                else MatchingVar (l,id,n,get_args l id k args)
            | None -> Pattern (l,!Env.name,id,args)
        )
    | PPattern (l,Some md,id,args) -> Pattern (l,md,id,List.map (aux k ctx) args)
    | PLambda (l,x,p) -> Lambda (l,x,aux (k+1) (x::ctx) p)
    | PCondition pte ->
        let te = t_of_pt ctx pte in
          if is_closed k te then Brackets te
          else Print.fail (get_loc te)
                 "The term '%a' contains a variable bound outside the brackets."
                 Pp.pp_term te
    | PJoker l -> Joker l
  in aux 0 ctx

let scope_term (ctx:context) (pte:preterm) : term =
  t_of_pt (List.map fst ctx) pte

let scope_pattern (ctx:context) (ppat:prepattern) : pattern =
  p_of_pp (List.map fst ctx) ppat
