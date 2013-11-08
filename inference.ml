
open Types

(* *** Error messages *** *)

let string_of_decl (_,x,ty) =
  if ident_eq empty x then "?: " ^ Pp.string_of_term2 ty
  else string_of_ident x ^ ": " ^ Pp.string_of_term2 ty

let mk_err_msg (ctx:context) (te:term) (exp:string) (inf:term) = 
  let context = 
    match ctx with
      | []      -> ".\n"
      | _       -> " in context:\n" ^ String.concat "\n" (List.map string_of_decl ctx) ^ "\n" 
  in
  let msg = "Error while typing " ^ Pp.string_of_term2 te 
                ^ context 
                ^ "Expected type: " ^ exp ^ "\n" 
                ^ "Inferred type: " ^ Pp.string_of_term2 inf in
    raise (TypingError ( get_loc te , msg ) )
(*
let mk_err_msg2 (ctx:context) (te:term) (exp:term) (inf:term) = 
  let context = 
    match ctx with
      | []      -> ".\n"
      | _       -> " in context:\n" ^ String.concat "\n" (List.map string_of_decl ctx) ^ "\n" 
  in
  let msg = "Error while typing " ^ Error.string_of_term te 
                ^ context 
                ^ "Expected type: " ^ Error.string_of_term exp ^ "\n" 
                ^ "Expected type (hnf): " ^ Error.string_of_term (Reduction.hnf exp) ^ "\n" 
                ^ "Inferred type: " ^ Error.string_of_term inf ^ "\n"
                ^ "Inferred type (hnf): " ^ Error.string_of_term (Reduction.hnf inf) in
    raise (TypingError ( get_loc te , msg ) )
 *)

let err_conv ctx te exp inf = 
  mk_err_msg ctx te (Pp.string_of_term2 exp) inf

let err_sort ctx te inf = 
  mk_err_msg ctx te "Kind or Type" inf

let err_topsort ctx te = 
  mk_err_msg ctx te "anything but Kind" mk_kind

let err_prod ctx te inf = 
  mk_err_msg ctx te "a product type" inf

(* *** Type Inference *** *) 

let rec nth ctx n = 
  match ctx with
    | []                -> assert false
    | (_,_,ty)::tl      -> if n<1 then ty else nth tl (n-1)

let rec infer (ctx:context) (te:term) : term = 
  match te with
    | Type _                            -> mk_kind 
    | DB (_,_,n)                        -> ( (*assert (n<k) ;*) Subst.shift (n+1) 0 (nth ctx n) )
    | GVar (l,m,v)                      -> Env.get_global_type l m v
    | Pi (l,x,a,b)                      ->
        begin
          is_type ctx a ;
          match infer ((l,x,a)::ctx) b with 
            | Kind | Type _ as t        -> t
            | ty                        -> err_sort ctx b ty
        end
    | Lam (l,x,a,t)                     -> 
        begin
          is_type ctx a ;
          match infer ((l,x,a)::ctx) t with 
            | Kind      -> err_topsort ctx te
            | b         -> mk_pi dloc x a b
        end
    | App ( f0::((_::_) as args) )      -> snd ( List.fold_left (infer_app ctx) (f0,infer ctx f0) args )
    | App _ 
    | Kind   
    | Meta _                            -> assert false
                                             
and infer_app ctx (f,ty_f) u = 
  match Reduction.wnf ty_f , infer ctx u with 
    | ( Pi (_,_,a,b) , a' )     ->  
        if Reduction.are_convertible a a' then ( mk_app [f;u] , Subst.subst b u )
        else err_conv ctx u a a'  
    | ( t , _ )                 -> err_prod ctx f ty_f 
                                     
and is_type ctx a = 
  match infer ctx a with
    | Type _    -> ()
    | ty        -> err_conv ctx a (mk_type dloc) ty

let rec term_of_pattern = function 
  | Var (l,x,n)                 -> mk_db l x n
  | Dash (l,n)                  -> mk_meta l n
  | Pattern ((l,m,v),[||])      -> mk_gvar l m v
  | Pattern ((l,m,v),args)      -> mk_uapp ( (mk_gvar l m v) :: (List.map term_of_pattern (Array.to_list args)) )

let rec infer_pattern ctx = function
  | Var (_,_,n)                 -> (* (n<List.length ctx); *) ( Subst.shift (n+1) 0 (nth ctx n) , [] )
  | Dash _                      -> assert false 
  | Pattern ((l,m,v),pats)      ->
      let aux (f,pi,lst:term*term*(term*term)list) (arg:pattern) : term*term*(term*term) list =
          match Reduction.hnf pi with
            | Pi (_,_,a,b)      -> 
                let lst2 = check_pattern ctx a arg in
                let u = term_of_pattern arg in
                  ( mk_app [f;u] , Subst.subst b u , lst@lst2 )
            | _                 -> err_prod ctx f pi
      in
      let (_,ty,lst) = Array.fold_left aux ( mk_gvar l m v , Env.get_global_type l m v , [] ) pats in
        (ty,lst)

and check_pattern ctx ty = function
  | Dash _      -> []
  | p           -> let (ty2,lst) = infer_pattern ctx p in (ty,ty2)::lst

(* *** Type Checking *** *)

let check_term ctx te exp = 
  let inf = infer ctx te in
    if not (Reduction.are_convertible exp inf) then 
      err_conv ctx te exp inf

let check_type ctx ty = 
  match infer ctx ty with
    | Kind | Type _     -> ()
    | s                 -> err_sort ctx ty s

let check_env ctx (l,x,ty) = 
  check_type ctx ty ; (l,x,ty)::ctx 

let check_rule (ctx,((l,c),args),ri) = 
  let _            = List.fold_left check_env [] (List.rev ctx) in 
  let (ty_le0,lst) = infer_pattern ctx (Pattern ((l,!Global.name,c),args)) in 
   try 
     let ty = Unification.resolve_constraints ty_le0 lst in
       (*Global.eprint (Pp.string_of_term2 ty); *)
       check_term ctx ri ty 
   with
     | Unification.CannotType           ->
         raise ( PatternError ( l , "Error while typing " ^ Pp.string_of_pattern (Pattern ((l,!Global.name,c),args)) ^ "." ))
     | Unification.CannotFindAType      ->
         raise ( PatternError ( l , "Error while typing " ^ Pp.string_of_pattern (Pattern ((l,!Global.name,c),args)) ^ ".\nCannot find a type." ))
