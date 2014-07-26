open Types

(* *** Type error messages *** *)

let mk_err_msg lc ctx pp_te te pp_exp exp pp_inf inf =
  if ctx = [] then
    Global.fail lc "Error while typing '%a'\nExpected: %a\nInferred: %a."
      pp_te te pp_exp exp pp_inf inf
  else
    Global.fail lc "Error while \
      typing '%a' in context:\n%a\nExpected: %a.\nInferred: %a."
      pp_te te Pp.pp_context ctx pp_exp exp pp_inf inf
(*
let mk_err_rule lc ctx p =
  if ctx = [] then Global.fail lc "Error while typing '%a'." Pp.pp_pattern p
  else
    Global.fail lc "Error while typing '%a' in context:\n%a\n."
      Pp.pp_pattern p Pp.pp_context ctx
 *)
let err_conv ctx te exp inf =
  mk_err_msg (get_loc te) ctx Pp.pp_pterm te Pp.pp_term (Reduction.hnf exp)
    Pp.pp_term (Reduction.hnf inf)

let err_conv_pat l ctx pat exp inf =
  mk_err_msg l ctx Pp.pp_pattern pat Pp.pp_term exp Pp.pp_term inf

let err_sort ctx te inf =
  mk_err_msg (get_loc te) ctx Pp.pp_pterm te output_string "Kind or Type"
    Pp.pp_term inf

let err_topsort ctx te =
  mk_err_msg (get_loc te) ctx Pp.pp_pterm te output_string "anything but Kind"
    output_string "Kind"

let err_prod lc ctx te inf =
  mk_err_msg lc ctx Pp.pp_term te output_string "a product type" Pp.pp_term inf

let err_pattern lc ctx p inf =
  mk_err_msg lc ctx Pp.pp_pattern p output_string "a product type" Pp.pp_term inf

(* *** Monodirectional Type Inference for preterm *** *)

let get_type ctx id =
  let rec aux n = function
    | []                -> None
    | (x,ty)::lst       ->
        if ident_eq id x then Some ( n , Subst.shift (n+1) ty )
        else aux (n+1) lst
  in aux 0 ctx

let rec infer (ctx:context) (te:preterm) : term*term =
  match te with
    | PreType _                          -> ( mk_Type , mk_Kind )
    | PreId (l,id)                       ->
      ( match get_type ctx id with
      | None ->
        ( mk_Const !Global.name id ,
          Env.get_type l !Global.name id )
      | Some (n,ty)       -> ( mk_DB id n , ty ) )
    | PreQId (l,md,id)                   ->
      if ident_eq md empty && is_const id then
        ( mk_Const empty id, get_const_ty id )
      else
        ( mk_Const md id , Env.get_type l md id )
    | PreApp ( f::((_::_) as args))      ->
        List.fold_left (infer_app (get_loc f) ctx) (infer ctx f) args
    | PreApp _                           -> assert false
    | PrePi (opt,a,b)                    ->
        let a' = is_type ctx a in
        let (ctx',x) = match opt with
          | None              -> ( (empty,a')::ctx , None )
          | Some (_,id)       -> ( (id,a')::ctx , Some id )
        in
          ( match infer ctx' b with
              | ( b' , (Type|Kind as tb) )      -> ( mk_Pi x a' b' , tb )
              | ( _ , tb )                      -> err_sort ctx' b tb )
    | PreLam  (l,x,a,b)                         ->
        let a' = is_type ctx a in
        let ctx' = (x,a')::ctx in
          ( match infer ctx' b with
              | ( _ , Kind )    -> err_topsort ctx' b
              | ( b' , ty  )    -> ( mk_Lam x a' b' , mk_Pi (Some x) a' ty ) )
    | PreChar (l, c) -> (mk_Char c, mk_char_type)
    | PreStr (l, s) -> (mk_Str s, mk_string_type)
    | PreNum (l, s) -> (mk_Num s, mk_num_type)

and infer_app lc ctx (f,ty_f) u =
  match Reduction.whnf ty_f , infer ctx u with
    | ( Pi (_,a,b)  , (u',a') )  ->
        if Reduction.are_convertible a a' then
          ( mk_App [f;u'] , Subst.subst b u' )
        else err_conv ctx u a a'
    | ( t , _ )                 -> err_prod lc ctx f ty_f

and is_type ctx a =
  match infer ctx a with
    | ( a' ,Type )      -> a'
    | ( a' , ty )       -> err_conv ctx a mk_Type ty

let of_list_rev = function
    [] -> [||]
  | hd::tl as l ->
      let n = List.length l in
      let a = Array.create n hd in
      let rec fill i = function
          [] -> a
        | hd::tl -> Array.unsafe_set a (n-i) hd; fill (i+1) tl in
        fill 2 tl

let rec infer_pattern ctx = function
  | PPattern   (l,opt,id,pargs) ->
    begin
      let ( is_var , md ) = match opt with
        | Some md       -> ( None , md )
        | None          -> if is_const id then (None , empty) else ( get_type ctx id , !Global.name )
      in
      match is_var with
      | Some (n,ty)       ->
        if pargs = [] then ( Var(id,n) , ty )
        else Global.fail l "The left-hand side of the rewrite rule cannot be a variable application."
      | None              ->
        let ty_id = if is_const id then get_const_ty id else
            Env.get_type l md id in
        let (ty,args) =
          List.fold_left (infer_pattern_aux l ctx md id) (ty_id,[]) pargs in
        ( Pattern (md,id,of_list_rev args) , ty )
    end
  | PCondition pte              ->
      let (te,ty) = infer ctx pte in
        ( Brackets te , ty )

and infer_pattern_aux l ctx md id (ty,args) parg =
  match Reduction.whnf ty with
    | Pi (_,a,b)        ->
        let (arg,a') = infer_pattern ctx parg in
          if Reduction.are_convertible a a' then
            ( Subst.subst b (term_of_pattern arg), arg::args )
          else
            err_conv_pat l ctx arg a a'
    | _                 ->
        err_pattern l ctx (Pattern (md,id,of_list_rev args)) ty

let infer_ptop ctx (l,id,args) =
  match infer_pattern ctx (PPattern(l,None,id,args)) with
    | Pattern (_,_,args), ty    -> (id,args,ty)
    | Var _, _                  ->
        Global.fail l "The Left-hand side of a rewrite rule cannot be a variable."
    | Brackets _ , _            -> assert false

let check_term ctx te exp =
  let (te',inf) = infer ctx te in
    if (Reduction.are_convertible exp inf) then te'
    else err_conv ctx te exp inf

let check_type ctx pty =
  match infer ctx pty with
    | ( ty , Kind ) | ( ty , Type )   -> ty
    | ( _ , s )                         -> err_sort ctx pty s

let check_context =
  List.fold_left ( fun ctx (_,x,ty) -> (x,check_type ctx ty)::ctx ) []

let check_rule (pctx,ple,pri:prule) : rule =
  let (l,_,_) = ple in
  let ctx = check_context pctx in
  let (id,args,ty) = infer_ptop ctx ple in
  let rhs = check_term ctx pri ty in
    { l=l ; ctx=ctx ; id=id ; args=args ; rhs=rhs ; }
