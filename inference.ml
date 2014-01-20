
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

let rec infer (ctx:context) (te:preterm) : term*term =
  match te with
    | PreType _                          -> ( mk_Type , mk_Kind )
    | PreId (l,id)                       ->
        begin
          match get_type ctx id with
            | None              -> ( mk_Const !Global.name id , Env.get_global_type l !Global.name id )
            | Some (n,ty)       -> ( mk_DB id n , Subst.shift (n+1) 0 ty )
        end
    | PreQId (l,md,id)                   -> ( mk_Const md id , Env.get_global_type l md id )
    | PreApp ( f::((_::_) as args))      -> List.fold_left (infer_app (get_loc f) ctx) (infer ctx f) args
    | PrePi (opt,a,b)                    ->
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
    | PreLam  (l,x,a,b)                  ->
        begin
          let a' = is_type ctx a in
          let ctx' = (x,a')::ctx in
            match infer ctx' b with
              | ( _ , Kind )    -> err_topsort ctx' b
              | ( b' , ty  )    -> ( mk_Lam x a' b' , mk_Pi (Some x) a' ty )
        end
    | PreApp _                           -> assert false
(*    | P_Unknown _                          -> assert false *)

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
(*
let rec infer_pattern (ctx:context) : preterm -> term*term * (term*term) list = function
(*  | P_Unknown (l,n)     -> raise (PatternError ( l , "Cannot find a type for '_'." )) *)
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
      let aux (f,ty_f,lst:term*term*(term*term)list) (arg:preterm) : term*term*(term*term) list =
        match Reduction.hnf ty_f with
          | Pi (_,a,b)  ->
              let (arg',lst2) = check_pattern ctx a arg in
                ( mk_App [f;arg'] , Subst.subst b arg' , lst@lst2 )
          | _           -> err_prod (get_loc f0) ctx f ty_f
      in
        List.fold_left aux ( mk_Const md id , Env.get_global_type l md id , [] ) args
  | _                   -> assert false

and check_pattern (ctx:context) (ty:term) : preterm -> term * (term*term) list = function
 (* | P_Unknown (l,n)     -> ( mk_Meta n , [] ) *)
  | te                  ->
      let (te',ty',lst) = infer_pattern ctx te in
        ( te' , (ty,ty')::lst )
 *)


(*
let rec infer_pattern ctx : prepattern -> pattern * partial_term * (partial_term*partial_term) list = function
  | Unknown _                   -> assert false (*TODO*)
  | PPattern (l,md,id,args)     -> 
      begin
        match get_type ctx id with
          | Some (n,ty) -> 
              if is_empty args then ( Var (id,n) , mk_partial (Subst.shift (n+1) 0 ty) , [] )
              else assert false (*TODO*)
          | None        -> 
              let ty_id = mk_partial (Env.get_global_type l md id) in
              let (ty,args',eqs) = List.fold_left (check_pattern ctx) (ty_id,[],[]) args in
                ( Pattern (md,id,Array.of_list args') , ty , eqs )
      end *)

let rec pattern_to_partial_term = function
  | Var (id,n)                  -> mk_partial (mk_DB id n )
  | Joker n                     -> mk_meta n
  | Pattern (md,id,args)        -> 
      let c = mk_partial (mk_Const md id) in
      if Array.length args = 0 then c
      else mk_partial_app ( c :: (Array.to_list (Array.map pattern_to_partial_term args)) )
  | Dot _                       -> assert false

let is_empty = function []  -> true | _   -> false

let rec check_pattern (ctx:context) (ty:partial_term) (eqs:(partial_term*partial_term) list) 
      : prepattern -> pattern * (partial_term*partial_term) list = 
  function
    | Unknown (_,n)                     -> (  Joker n , eqs )
    | PPattern (l,md_opt,id,lst)        -> 
        begin 
          let ( md , is_var ) = match md_opt with
            | None      -> 
                ( match get_type ctx id with
                    | Some (_,_) as s   -> ( assert (is_empty lst) ; ( !Global.name , s ) )
                    | None              -> ( !Global.name , None )
                )
            | Some md   -> ( md , None )
          in
          match is_var with
            | Some (n,ty_id)    -> ( Var (id,n) , ( ty , mk_partial ty_id ) :: eqs )
            | None              ->
                  let ty_id = mk_partial (Env.get_global_type l !Global.name id) in
                  let (ty_pat,lst',eqs') = List.fold_left (check_pattern_args ctx) (ty_id,[],eqs) lst in
                    ( Pattern (md,id,Array.of_list lst') , (ty,ty_pat)::eqs' )
          end
                                             
and check_pattern_args ctx (ty,args,eqs:partial_term * pattern list * (partial_term*partial_term) list) 
          (parg:prepattern) : partial_term * pattern list * (partial_term*partial_term) list =
  let (a,b) = match ty with 
    | Term ty'           -> 
        ( match Reduction.hnf ty' with
            | Pi (_,a,b)        -> (mk_partial a,mk_partial b)
            | _                 -> assert false (*TODO*)
        )
    | PartialPi (_,a,b) -> (a,b)
    | _                 -> assert false (*TODO*)
  in
  let ( arg , eqs' ) = check_pattern ctx a eqs parg in
    ( Subst.subst_pt b (pattern_to_partial_term arg) , arg::args , eqs' )

let infer_ptop (ctx:context) (l,id,lst:ptop) : top*term = 
  match get_type ctx id with
    | Some (n,ty) -> 
        if is_empty lst then assert false (*TODO*) 
        else assert false (*TODO*)
    | None        -> 
        let ty_id = mk_partial (Env.get_global_type l !Global.name id) in
        let ty_args_eqs = List.fold_left (check_pattern_args ctx) (ty_id,[],[]) lst in
        let (ty,args) = Unification.resolve ty_args_eqs in
          ( ( id , Array.of_list args ) , ty ) 


(* *** Type Checking *** *)

let check_term ctx te exp =
  let (te',inf) = infer ctx te in
    if (Reduction.are_convertible exp inf) then te'
    else ( (*FIXME*)
      Global.eprint ( "Exp (hnf): " ^ Pp.string_of_term (Reduction.hnf exp) ) ;
      Global.eprint ( "Inf (hnf): " ^ Pp.string_of_term (Reduction.hnf inf) ) ;
      err_conv ctx te exp inf )

let check_type ctx pty =
  match infer ctx pty with
    | ( ty , Kind ) | ( ty , Type _ )   -> ty
    | ( _ , s )                         -> err_sort ctx pty s

let check_rule (pctx,ple,pri:prule) : rule = 
  let (l,_,_) = ple in
  let ctx = 
    List.fold_left ( fun ctx (_,x,ty) -> (x,check_type ctx ty)::ctx ) [] pctx in
  let ((id,args),ty) = infer_ptop ctx ple in
  let ri = check_term ctx pri ty in

  (*let env_size = List.length ctx in (*FIXME*) *)
  (*let (k,args_l,eqs) = linearize env_size args in*)
    ( l , ctx , id , args , ri ) 

 (* TODO
  let te2 = 
     if !Global.raphael then ( 
       let hnf = Reduction.hnf te' in
         if not (term_eq te' hnf) then (
           Global.warning l "This pattern is not normal: replacing it by its normal form." ;
           Global.eprint ("Pattern: " ^ Pp.string_of_term te') ;
           Global.eprint ("Normal form: " ^ Pp.string_of_term hnf) ;
           hnf
         )
         else te'
     )
     else te' in 
  *)

