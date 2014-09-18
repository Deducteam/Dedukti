open Term
open Rule
(* FIXME to be tested *)
  (* FIXME compatibility with coc *)
let err l = Print.fail l "Error while infering the type of jokers." (*FIXME*)

let rec number_of_jokers = function
  | Joker _ -> 1
  | Brackets _
  | MatchingVar _ -> 0
  | Pattern (_,_,_,args)
  | BoundVar (_,_,_,args) ->
      List.fold_left (fun n p -> n+(number_of_jokers p)) 0 args
  | Lambda (_,_,p) -> number_of_jokers p

let to_term2 nb_jokers (p:pattern) : term =
  let nb = ref (-1) in
  let get_nb _ = incr nb ; !nb in
  let rec aux k = function
    | Joker l -> mk_DB l qmark (nb_jokers-1+k-(get_nb ()))
    | Brackets t -> Subst.shift nb_jokers t
    | Pattern (l,m,v,[]) -> mk_Const l m v
    | BoundVar (l,x,n,[]) | MatchingVar (l,x,n,[]) ->
        let n2 =if n<k then n else  n+nb_jokers in mk_DB l x n2
    | Pattern (l,m,v,a::args) ->
        let a2 = aux k a in
        let args2 = List.map (aux k) args in
          mk_App (mk_Const l m v) a2 args2
    | BoundVar (l,x,n,a::args) ->
        mk_App (mk_DB l x n) (aux k a) (List.map (aux k) args)
    | MatchingVar (l,x,n,(l2,x2,n2)::args) ->
        mk_App (mk_DB l x n) (aux k (BoundVar(l2,x2,n2,[])))
          (List.map (fun (l,x,n) -> aux k (BoundVar(l,x,n,[]))) args)
    | Lambda (l,x,pat) -> mk_Lam l x None (aux (k+1) pat)
  in
    aux 0 p

let db_get_type l ctx n = Subst.shift (n+1) (snd (List.nth ctx n))

let unshift l n te =
  let rec aux k = function
  | DB (_,x,i) as t when i<k -> t
  | DB (_,x,i) ->
      if i>n then mk_DB dloc x (i-n-1)
      else err l
  | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args )
  | Lam (_,x,_,f) -> mk_Lam dloc x None (aux (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (aux k a) (aux (k+1) b)
  | Type _ | Kind | Const _ as t -> t
  in
    aux 0 te

let refine (nb_jokers:int) (ctx0:context) (te:term) : context =
  let arr = Array.make nb_jokers (qmark,mk_Kind) in
  let size = List.length ctx0 in

  let rec infer k (ctx:context) : term -> term = function
    | Const (l,md,id) -> Env.get_type l md id
    | DB (l,_,n) when n<k -> db_get_type l ctx n
    | DB (l,_,n) ->
        begin
          let n' = n-nb_jokers-k in
            assert ( n' >= 0 );
            assert ( n' < size );
            db_get_type l ctx0 n'
        end
    | App (f,a,args) ->
        List.fold_left (check_app k ctx) (infer k ctx f) (a::args)
    | Lam _ -> assert false
    | Kind | Type _ | Pi _-> assert false

  and check_app k (ctx:context) (ty_f:term) (arg:term) : term =
    match Reduction.whnf ty_f with
      | Pi (_,_,a,b) -> ( check k ctx arg a; Subst.subst b arg )
      | _ -> err (get_loc arg)

  and check  k ctx (te:term) (ty_exp:term) : unit =
    match te with
      | Lam (l,x,_,u) ->
          ( match Reduction.whnf ty_exp with
              | Pi (_,x,a1,b) -> check (k+1) ((x,a1)::ctx) u b
              | _ -> err l
          )
      | DB (_,_,n) when ( 0 <= (n-k) &&  (n-k) < nb_jokers ) ->
          arr.(n-k) <- (qmark,unshift (get_loc te) n ty_exp)
      | _ ->
          let ty_inf = infer k ctx te in
            if Reduction.are_convertible ty_exp ty_inf then ()
            else err (get_loc te)
  in
  let _ = infer 0 [] te in
    (Array.to_list arr)@ctx0

let to_term (ctx:context) (pat:pattern) : context*term =
  let nb = number_of_jokers pat in
  let lhs = to_term2 nb pat in
    if nb>0 then
      let ctx2 = refine nb ctx lhs in
        ( ctx2 , lhs )
    else (ctx,lhs)
