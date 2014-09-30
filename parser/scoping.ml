open Basics
open Preterm
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
            | None   -> mk_Const l (Env.get_name ()) id
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
  t_of_pt (List.map fst ctx) pte

(******************************************************************************)

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
  | Lam (_,_,None,b) -> aux (q+1) b
  | Lam (_,_,Some a,b) | Pi (_,_,a,b) -> (aux q a) && (aux (q+1) b)
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
            | None -> Pattern (l,(Env.get_name ()),id,args)
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

let scope_pattern (ctx:context) (pp:prepattern) : pattern =
  p_of_pp (List.map fst ctx) pp

(******************************************************************************)

let get_nb_args (esize:int) (p:pattern) : int array =
  let arr = Array.make esize (-1) in (* -1 means +inf *)
  let min a b =
    if a = -1 then b
    else if a<b then a else b
  in
  let rec aux k = function
    | BoundVar (_,_,_,args) | Pattern (_,_,_,args) -> List.iter (aux k) args
    | Lambda (_,_,pp) -> aux (k+1) pp
    | MatchingVar (_,id,n,args) ->
        arr.(n-k) <- min (arr.(n-k)) (List.length args)
    | Brackets _ | Joker _ -> ()
  in
    ( aux 0 p ; arr )

let check_nb_args (nb_args:int array) (te:term) : unit =
  let rec aux k = function
    | Kind | Type _ | Const _ -> ()
    | DB (l,id,n) ->
        if n>=k && nb_args.(n-k)>0 then
          Print.fail l "The variable '%a' must be applied to at least %i argument(s)."
            pp_ident id nb_args.(n-k)
    | App(DB(l,id,n),a1,args) when n>=k ->
        if ( nb_args.(n-k) > 1 + (List.length args) ) then
          Print.fail l "The variable '%a' must be applied to at least %i argument(s)."
            pp_ident id nb_args.(n-k)
        else List.iter (aux k) (a1::args)
    | App (f,a1,args) -> List.iter (aux k) (f::a1::args)
    | Lam (_,_,None,b) -> aux (k+1) b
    | Lam (_,_,Some a,b) | Pi (_,_,a,b) -> (aux k a;  aux (k+1) b)
  in
    aux 0 te

let scope_context pctx =
  let aux ctx0 (_,x,ty) = (x,scope_term ctx0 ty)::ctx0 in
    List.fold_left aux [] pctx

let scope_rule (l,pctx,id,pargs,pri) =
  let ctx = scope_context pctx in
  let pat = scope_pattern ctx (PPattern(l,None,id,pargs)) in
  let ri = scope_term ctx pri in
  let args = match pat with
    | Pattern (_,_,_,args) -> args
    | MatchingVar (l,_,_,_) -> Print.fail l "A pattern cannot be a variable."
    | _ -> assert false in
  let esize = List.length ctx in (*TODO*)
  let nb_args = get_nb_args esize pat in
  let _ = check_nb_args nb_args ri in
    { l=l ; ctx=ctx ; md= (Env.get_name ()); id=id ; args=args ; rhs=ri }
