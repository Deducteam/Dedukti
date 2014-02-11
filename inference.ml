
open Types

(* *** Error messages *** *)

let string_of_decl (x,ty) =
  if ident_eq empty x then "?: " ^ Pp.string_of_term ty
  else string_of_ident x ^ ": " ^ Pp.string_of_term ty

let mk_err_msg (lc:loc) (ctx:context) (te:string) (exp:string) (inf:string) =
  let ctx_str = String.concat "\n" (List.rev_map string_of_decl ctx) in
  let msg = 
    "Error while typing " ^ te ^ 
    (match ctx with
       | []      -> ".\n"
       | _       -> " in context:\n" ^ ctx_str ^ "\n" 
    ) ^
    "Expected type: " ^ exp ^ "\n" ^ "Inferred type: " ^ inf 
  in
    raise (TypingError ( lc , msg ) )

let err_conv ctx te exp inf =
  mk_err_msg (get_loc te) ctx (Pp.string_of_pterm te) 
    (Pp.string_of_term exp) (Pp.string_of_term inf)

let err_sort ctx te inf =
  mk_err_msg (get_loc te) ctx (Pp.string_of_pterm te) 
    "Kind or Type" (Pp.string_of_term inf)

let err_topsort ctx te =
  mk_err_msg (get_loc te) ctx 
    (Pp.string_of_pterm te) "anything but Kind" "Kind"

let err_prod lc ctx te inf =
  mk_err_msg lc ctx 
    (Pp.string_of_term te) "a product type" (Pp.string_of_term inf)

let err_pattern lc ctx p inf = 
  mk_err_msg lc ctx 
    (Pp.string_of_pattern [] p) "a product type" (Pp.string_of_term inf)

let err_rule l ctx p =
  let ctx_str = String.concat "\n" (List.rev_map string_of_decl ctx) in
  let msg = 
    "Error while typing the pattern " ^ (Pp.string_of_pattern [] p) ^ 
    (match ctx with
       | []      -> ".\n"
       | _       -> " in context:\n" ^ ctx_str ^ "\n" 
    ) in
    raise (TypingError ( l , msg ) )

(* *** Type Inference *** *)

let get_type ctx id =
  let rec aux n = function
    | []                -> None
    | (x,ty)::lst       -> if ident_eq id x then Some (n,ty) else aux (n+1) lst
  in aux 0 ctx

let rec infer (ctx:context) (te:preterm) : term*term =
  match te with
    | PreType _                          -> ( mk_Type , mk_Kind )
    | PreId (l,id)                       ->
        ( match get_type ctx id with
            | None              -> 
                ( mk_Const !Global.name id , 
                  Env.get_global_type l !Global.name id )
            | Some (n,ty)       -> 
                ( mk_DB id n , Subst.shift (n+1) 0 ty ) )
    | PreQId (l,md,id)                   -> 
        ( mk_Const md id , Env.get_global_type l md id )
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
  match Reduction.wnf ty_f , infer ctx u with
    | ( Pi (_,a,b)  , (u',a') )  ->
        if Reduction.are_convertible a a' then 
          ( mk_App [f;u'] , Subst.subst b u' )
        else err_conv ctx u a a'
    | ( t , _ )                 -> err_prod lc ctx f ty_f

and is_type ctx a =
  match infer ctx a with
    | ( a' ,Type _ )    -> a'
    | ( a' , ty )       -> err_conv ctx a mk_Type ty

(* Type Inference for patterns *)

let err_left_var = 
  "The left-hand side of the rewrite rule cannot be a variable."
let err_var_app = 
  "The left-hand side of the rewrite rule cannot be a variable application."

let is_empty = function []  -> true | _   -> false

let add_equation t1 t2 eqs = 
  match Reduction.are_convertible_with_meta t1 t2 with
    | Yes _     -> eqs
    | _        -> (t1,t2)::eqs

let rec check_pattern k ctx ty eqs = function
  | Unknown _                   -> ( k+1 , Var(None,k) , eqs ) 
  | PPattern (l,md_opt,id,args) -> 
      let ( k2 , pat , ty2 , eqs2 ) = infer_pattern k ctx l md_opt id args eqs in
        ( k2 , pat , add_equation ty ty2 eqs2 )

and infer_pattern k ctx l md_opt id args eqs =
  let is_var = match md_opt with Some _ -> None | None -> get_type ctx id in
    match is_var with
      | Some (n,ty2)      -> ( k , Var (Some id,n), (Subst.shift (n+1) 0 ty2) , eqs )  
      | None                ->
          let md = match md_opt with None -> !Global.name | Some md -> md in
          let ty_id = Env.get_global_type l md id in
          let (k2,ty2,args2,eqs2) = 
            List.fold_left (check_pattern_args l ctx) (k,ty_id,[],eqs) args in
            ( k2 , Pattern (md,id,Array.of_list (List.rev args2)) , ty2 , eqs2 )
                                      
and check_pattern_args l ctx (k,ty,args,eqs) parg =
  match Reduction.wnf_with_meta ty with 
    | Some (Pi (_,a,b)) -> 
        let ( k2 , arg , eqs2 ) = check_pattern k ctx a eqs parg in
          ( k2 , Subst.subst b (term_with_meta_of_pattern arg) , arg::args , eqs2 )
    | _                 -> assert false (*TODO 
        let md = match md_opt with None -> !Global.name | Some md -> md in
        let args' = Array.of_list (List.rev args) in
          err_pattern l ctx (Pattern (md,id,args')) ty *)
          
let rec infer2 (ctx:context) = function
  | Type                -> mk_Kind 
  | DB (_,n)            -> 
      begin
        try Subst.shift (n+1) 0 (snd (List.nth ctx n))
        with Not_found -> assert false (*TODO*)
      end
  | Const (m,v)         -> Env.get_global_type dloc m v
  | App (f::args)       -> 
      List.fold_left (infer2_app ctx) (infer2 ctx f) args
  | Lam (x,a,b)         -> 
      begin
        let _ = is_type2 ctx a in
          match infer2 ((x,a)::ctx) b with
            | Kind      -> assert false (*TODO*)
            | ty         -> mk_Pi (Some x) a ty
      end
  | Pi (opt,a,b)        -> 
      begin
        let _ = is_type2 ctx a in
        let x = match opt with None -> empty | Some x -> x in
          match infer2 ((x,a)::ctx) b with
            | Type | Kind as ty -> ty
            | _                 -> assert false (*TODO*)
      end
  | Meta _              -> assert false (*TODO*)
  | App []              -> assert false
  | Kind                -> assert false 

and is_type2 ctx a = match infer2 ctx a with
  | Type      -> ()
  | _         -> assert false (*TODO*)

and infer2_app ctx ty_f u = match Reduction.wnf ty_f , infer2 ctx u with
  | ( Pi (_,a,b)  , a' )      ->
      if Reduction.are_convertible a a' then Subst.subst b u 
      else assert false (*TODO*)
  | ( t , _ )                 -> assert false (*TODO*) 

let is_well_typed ctx ty = ignore (infer2 ctx ty)  (*FIXME*)

(* *** Type Checking *** *)

let check_term ctx te exp =
  let (te',inf) = infer ctx te in
    if (Reduction.are_convertible exp inf) then te'
    else err_conv ctx te exp inf

let check_type ctx pty =
  match infer ctx pty with
    | ( ty , Kind ) | ( ty , Type _ )   -> ty
    | ( _ , s )                         -> err_sort ctx pty s

let rec is_type_level = function
  | Pi (_,_,t)  -> is_type_level t
  | Type        -> true
  | _           -> false
 
let cpt_rule = ref 0
let get_rule_nb _ =
  incr cpt_rule;
  !cpt_rule

let check_rule (pctx,(l,id,pargs),pri:prule) : rule = (*FIXME unfold pattern*) 
  let ctx = 
    List.fold_left (fun ctx (_,x,ty) -> (x,check_type ctx ty)::ctx ) [] pctx 
  in
  let (_,le,ty0,eqs) = 
    infer_pattern (List.length ctx) ctx l None id pargs [] 
  in
  let args = match le with
      | Var _                   -> assert false (*TODO*)
      | Pattern (_,_,args)      -> args
  in
    match Unification.unify_t eqs with
      | None    -> assert false (*TODO*) 
      | Some s  ->
          let ty = Subst.subst_meta s ty0 in
          let _  = is_well_typed ctx ty   in
          let ri = check_term ctx pri ty  in 
          let r:rule = { nb=get_rule_nb (); l=l; ctx=ctx; id=id; args=args; ri=ri; sub=s; } in 
                        
            (if is_type_level ty then
               match ri with
                 | Const _ | App ( (Const _) :: _ ) -> ()
                 | _ -> Global.unset_constant_applicative l
            ) ;
            Global.vprint2 (lazy (Pp.string_of_rule r)) ;
            r
