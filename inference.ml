open Types

(* *** Type error messages *** *)

let mk_err_msg lc ctx pp_te te pp_exp exp pp_inf inf =
  if ctx = [] then
    Global.fail lc "Error while typing %a\nExpected: %a\nInferred: %a." 
      pp_te te pp_exp exp pp_inf inf
  else
    Global.fail lc "Error while \
      typing %a in context:\n%a\nExpected: %a.\nInferred: %a." 
      pp_te te Pp.pp_context ctx pp_exp exp pp_inf inf

let mk_err_rule lc ctx p =
  if ctx = [] then Global.fail lc "Error while typing %a." Pp.pp_pattern p
  else 
    Global.fail lc "Error while typing %a in context:\n%a\n." 
      Pp.pp_pattern p Pp.pp_context ctx

let err_conv ctx te exp inf =
  mk_err_msg (get_loc te) ctx Pp.pp_pterm te Pp.pp_term exp Pp.pp_term inf

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
            | None              ->
                ( mk_Const !Global.name id ,
                  Env.get_type l !Global.name id )
            | Some (n,ty)       -> ( mk_DB id n , ty ) )
    | PreQId (l,md,id)                   ->
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

(* *** Bidirectional Type Inference for prepatterns *** *)
(*
let add_equation t1 t2 eqs =
  match Reduction.bounded_are_convertible 500 t1 t2 with
    | Yes      -> eqs
    | _        -> (t1,t2)::eqs

let rec infer_pattern l k ctx md_opt id (args:prepattern list) eqs =
  let is_var = match md_opt with | Some _ -> None | None -> get_type ctx id in
    match is_var with
      | Some (n,ty)       ->
          if args = [] then ( k , Var(Some id,n) , ty , eqs )
          else Global.fail l 
                 "The left-hand side of \
                             the rewrite rule cannot be a variable application."
      | None      ->
          let md = match md_opt with None -> !Global.name | Some md -> md in
          let ty_id = Env.get_global_type l md id in
          let (k2,ty2,args2,eqs2) =
            List.fold_left (infer_pattern_aux l ctx md_opt id)
              (k,ty_id,[],eqs) args in
            ( k2, Pattern (md,id,of_list_rev args2) , ty2 , eqs2 )

and infer_pattern_aux l ctx md_opt id (k,ty,args,eqs) parg =
  match Reduction.bounded_whnf 500 ty with
    | Some (Pi (_,a,b)) ->
        let ( k2 , arg , eqs2 ) = check_pattern k ctx a eqs parg in
          (k2, Subst.subst b (term_of_pattern arg), arg::args, eqs2 )
    | Some ty           ->
        let md = match md_opt with None -> !Global.name | Some md -> md in
        let args' = of_list_rev args in
          err_pattern l ctx (Pattern (md,id,args')) (Some ty)
    | None              ->
        let md = match md_opt with None -> !Global.name | Some md -> md in
        let args' = of_list_rev args in
          err_pattern l ctx (Pattern (md,id,args')) None

and check_pattern k ctx ty eqs = function
  | Unknown _                   -> ( k+1 , Var(None,k) , eqs )
  | PPattern (l,md_opt,id,args) ->
      let (k2 ,pat ,ty2 ,eqs2 ) = infer_pattern l k ctx md_opt id args eqs in
        ( k2 , pat , add_equation ty ty2 eqs2 )
 *)

let of_list_rev = function
    [] -> [||]
  | hd::tl as l ->
      let n = List.length l in
      let a = Array.create n hd in
      let rec fill i = function
          [] -> a
        | hd::tl -> Array.unsafe_set a (n-i) hd; fill (i+1) tl in
        fill 2 tl

let cpt = ref 0
let new_c () = incr cpt ; !cpt

let rec infer_pattern ctx = function
  | PPattern   (l,opt,id,pargs) -> 
      begin
        let ( is_var , md ) = match opt with 
          | Some md       -> ( None , md ) 
          | None          -> ( get_type ctx id , !Global.name ) 
        in
          match is_var with
            | Some (n,ty)       ->
                if pargs = [] then ( Var(id,n) , ty )
                else Global.fail l "The left-hand side of the rewrite rule cannot be a variable application."
            | None              ->
                let ty_id = Env.get_type l md id in
                let (ty,args) =
                  List.fold_left (infer_pattern_aux l ctx md id) (ty_id,[]) pargs in
                  ( Pattern (md,id,of_list_rev args) , ty )
      end
  | PCondition pte              ->
      let (te,ty) = infer ctx pte in
        ( Condition ( new_c () , te ) , ty )

and infer_pattern_aux l ctx md id (ty,args) parg =
  match Reduction.whnf ty with
    | Pi (_,a,b)        ->
        let (arg,a') = infer_pattern ctx parg in
          if Reduction.are_convertible a a' then
            ( Subst.subst b (term_of_pattern arg), arg::args )
          else assert false (*TODO*)
    | _                 -> 
        err_pattern l ctx (Pattern (md,id,of_list_rev args)) ty

let infer_ptop ctx (l,id,args) =
  match infer_pattern ctx (PPattern(l,None,id,args)) with
    | Pattern (_,_,args), ty    -> (id,args,ty)
    | Var _, _                  -> 
        Global.fail l "The Left-hand side of a rewrite rule cannot be a variable."
    | Condition (_,_), _        -> assert false
    | EVar, _                   -> assert false

(* *** Bidirectional Type Inference for patterns *** *)

(*
 let rec check_pattern2 ty = function
 | Var (_,_)            -> true
 | Pattern (md,id,args) ->
 let ty_id = Env.get_global_type dloc md id in
 let ty2 = Array.fold_left infer_pattern_args2 ty_id args in
 Reduction.are_convertible ty ty2

 and infer_pattern2 = function
 | Var (None,_)         -> assert false
 | Var (Some _,n)       -> assert false
 | Pattern (md,id,args) ->
 let ty_id = Env.get_global_type dloc md id in
 Array.fold_left infer_pattern_args2 ty_id args

 and infer_pattern_args2 ty arg =
 match Reduction.whnf ty with
 | Pi (_,a,b)        ->
 if check_pattern2 a arg then Subst.subst b (term_of_pattern arg)
 else assert false
 | _                 -> assert false
 *)

(* *** Monodirectional Type Inference for terms *** *)
(*
exception Infer2Error

let rec infer2 (ctx:context) = function
  | Type                -> mk_Kind
  | DB (_,n)            ->
      begin
        try Subst.shift (n+1) (snd (List.nth ctx n))
        with Not_found -> raise Infer2Error
      end
  | Const (m,v)         -> Env.get_global_type dloc m v
  | App (f::args)       ->
      List.fold_left (infer2_app ctx) (infer2 ctx f) args
  | Lam (x,a,b)         ->
      begin
        let _ = is_type2 ctx a in
          match infer2 ((x,a)::ctx) b with
            | Kind      -> raise Infer2Error
            | ty         -> mk_Pi (Some x) a ty
      end
  | Pi (opt,a,b)        ->
      begin
        let _ = is_type2 ctx a in
        let x = match opt with None -> empty | Some x -> x in
          match infer2 ((x,a)::ctx) b with
            | Type | Kind as ty -> ty
            | _                 -> raise Infer2Error

      end
  | Meta _              -> raise Infer2Error
  | App []              -> assert false
  | Kind                -> assert false

and is_type2 ctx a = match infer2 ctx a with
  | Type      -> ()
  | _         -> raise Infer2Error

and infer2_app ctx ty_f u = match Reduction.whnf ty_f , infer2 ctx u with
  | ( Pi (_,a,b)  , a' )      ->
      if Reduction.are_convertible a a' then Subst.subst b u
      else raise Infer2Error
  | ( t , _ )                 -> raise Infer2Error

let is_well_typed ctx ty =
  try ( ignore ( infer2 ctx ty ) ; true )
  with Infer2Error -> false
 *)

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

