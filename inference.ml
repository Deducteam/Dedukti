
open Types

(* *** Error messages *** *)

let string_of_decl (x,ty) =
  if ident_eq empty x then "?: " ^ Pp.string_of_term ty
  else string_of_ident x ^ ": " ^ Pp.string_of_term ty

let mk_err_msg (lc:loc) (ctx:context) (te:string) (exp:string) (inf:string) =
  let context =
    match ctx with
      | []      -> ".\n"
      | _       -> " in context:\n" ^ String.concat "\n" (List.rev_map string_of_decl ctx) ^ "\n"
  in
  let msg = "Error while typing " ^ te
                ^ context
                ^ "Expected type: " ^ exp ^ "\n"
                ^ "Inferred type: " ^ inf in
    raise (TypingError ( lc , msg ) )

let err_conv ctx te exp inf =
  mk_err_msg (get_loc te) ctx (Pp.string_of_pterm te) (Pp.string_of_term exp) (Pp.string_of_term inf)

let err_sort ctx te inf =
  mk_err_msg (get_loc te) ctx (Pp.string_of_pterm te) "Kind or Type" (Pp.string_of_term inf)

let err_topsort ctx te =
  mk_err_msg (get_loc te) ctx (Pp.string_of_pterm te) "anything but Kind" "Kind"

let err_prod lc ctx te inf =
  mk_err_msg lc ctx (Pp.string_of_term te) "a product type" (Pp.string_of_term inf)

(* *** Type Inference *** *)

let get_type ctx id =
  let rec aux n = function
    | []                -> None
    | (x,ty)::lst       -> if ident_eq id x then Some (n,ty) else aux (n+1) lst
  in aux 0 ctx

let rec infer (ctx:context) (te:pterm) : term*term =
  match te with
    | P_Type _                          -> ( mk_Type , mk_Kind )
    | P_Id (l,id)                       ->
        begin
          match get_type ctx id with
            | None              -> ( mk_Const !Global.name id , Env.get_global_type l !Global.name id )
            | Some (n,ty)       -> ( mk_DB id n , Subst.shift (n+1) 0 ty )
        end
    | P_QId (l,md,id)                   -> ( mk_Const md id , Env.get_global_type l md id )
    | P_App ( f::((_::_) as args))      -> List.fold_left (infer_app (get_loc f) ctx) (infer ctx f) args
    | P_Pi (opt,a,b)                    ->
        begin
          let a' = is_type ctx a in
          let (ctx',x) = match opt with
            | None              -> ( (empty,a')::ctx , None )
            | Some (_,id)       -> ( (id,a')::ctx , Some id )
          in
            match infer ctx' b with
              | ( b' , (Type|Kind as tb) )      -> ( mk_Pi x a' b' , tb )
              |  ( _ , tb )                     -> err_sort ctx' b tb
        end
    | P_Lam  (l,x,a,b)                  ->
        begin
          let a' = is_type ctx a in
          let ctx' = (x,a')::ctx in
            match infer ctx' b with
              | ( _ , Kind )    -> err_topsort ctx' b
              | ( b' , ty  )    -> ( mk_Lam x a' b' , mk_Pi (Some x) a' ty )
        end
    | P_App _                           -> assert false
    | P_Unknown _                          -> assert false

and infer_app lc ctx (f,ty_f) u =
  match Reduction.wnf ty_f , infer ctx u with
    | ( Pi (_,a,b)  , (u',a') )  ->
        if Reduction.are_convertible a a' then ( mk_App [f;u'] , Subst.subst b u' )
        else err_conv ctx u a a'
    | ( t , _ )                 -> err_prod lc ctx f ty_f

and is_type ctx a =
  match infer ctx a with
    | ( a' ,Type _ )    -> a'
    | ( a' , ty )       -> err_conv ctx a mk_Type ty

(* Bidir pattern inference *)

let rec infer_pattern (ctx:context) : pterm -> term*term * (term*term) list = function
  | P_Unknown (l,n)     -> raise (PatternError ( l , "Cannot find a type for '_'." ))
  | P_Id (l,id)         ->
      begin
        match get_type ctx id with
          | None        -> ( mk_Const !Global.name id , Env.get_global_type l !Global.name id , [] )
          | Some (n,ty) -> ( mk_DB id n , Subst.shift (n+1) 0 ty , [] )
      end
  | P_QId (l,md,id)     -> ( mk_Const md id , Env.get_global_type l md id , [] )
  | P_App (f0::args)    ->
      let (l,md,id) =
        match f0 with
          | P_Id (l,id)         -> (l,!Global.name,id)
          | P_QId (l,md,id)     -> (l,md,id)
          | _                   -> assert false
      in
      let aux (f,ty_f,lst:term*term*(term*term)list) (arg:pterm) : term*term*(term*term) list =
        match Reduction.hnf ty_f with
          | Pi (_,a,b)  ->
              let (arg',lst2) = check_pattern ctx a arg in
                ( mk_App [f;arg'] , Subst.subst b arg' , lst@lst2 )
          | _           -> err_prod (get_loc f0) ctx f ty_f
      in
        List.fold_left aux ( mk_Const md id , Env.get_global_type l md id , [] ) args
  | _                   -> assert false

and check_pattern (ctx:context) (ty:term) : pterm -> term * (term*term) list = function
  | P_Unknown (l,n)     -> ( mk_Meta n , [] )
  | te                  ->
      let (te',ty',lst) = infer_pattern ctx te in
        ( te' , (ty,ty')::lst )

(* *** Type Checking *** *)

let check_term ctx te exp =
  let (te',inf) = infer ctx te in
    if (Reduction.are_convertible exp inf) then te'
    else err_conv ctx te exp inf

let check_type ctx pty =
  match infer ctx pty with
    | ( ty , Kind ) | ( ty , Type _ )   -> ty
    | ( _ , s )                         -> err_sort ctx pty s

let check_rule (ctx,te,ri) =
  let l    = get_loc te in
  let ctx' =
    List.fold_left (
      fun ctx (_,x,ty) ->
        let ty' = check_type ctx ty in
          (x,ty')::ctx
    ) [] ctx in
  let (te',ty,cstr) = infer_pattern ctx' te in
  let ty' = Rules.resolve_type l ty cstr in
  let ri' = check_term ctx' ri ty' in

  let te2 = (*FIXME*)
     if !Global.raphael then ( 
       let hnf = Reduction.hnf te' in
         if not (term_eq te' hnf) then (
           Global.warning l "This pattern is not normal: replacing by it normal form." ;
           Global.eprint ("Pattern: " ^ Pp.string_of_term te') ;
           Global.eprint ("Normal form: " ^ Pp.string_of_term hnf) ;
           hnf
         )
         else te'
     )
     else te' in 

    match Rules.pattern_of_term te2 with
      | Pattern (md,id,args)    -> 
          ( assert (ident_eq md !Global.name) ; ( l , ctx' , id , args , ri' ) )
      | Var _                   ->
          raise (PatternError ( l , "The left-hand side of a rule cannot be a variable." ))
      | Joker _                 -> assert false
