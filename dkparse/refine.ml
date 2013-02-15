
open Types
open Printf

type dk_type = 
  | Ty_GVar of id
  | Ty_Var  of var
  | Ty_Pi   of var option*dk_type*dk_type
  | Ty_App  of dk_ftype*dk_term
and dk_ftype = 
  | F_GVar  of id
  | F_Var   of var
  | F_App   of dk_ftype*dk_term
  | F_LamT  of var*dk_type*dk_type
  | F_LamF  of var*dk_type*dk_ftype
and dk_term =  
  | Te_GVar of id
  | Te_Var of var
  | Te_App of dk_term*dk_term
  | Te_Lam of var*dk_type*dk_term

type dk_subst =
  | S_Type of dk_type
  | S_Subst of dk_subst*dk_term
  | S_Pi of var*dk_type*dk_subst

type dk_arrow = 
  | K_Base of dk_type
  | K_Arrow of dk_type*dk_arrow

type gvar_type =
  | GV_TYPE                     (*var:Type*)
  | GV_Arrow of dk_arrow        (*var:dk_arrow*)
  | GV_Type of dk_type          (*var:dk_type*)

type context = (var*gvar_type) list

let (exth:(string,(string,gvar_type) Hashtbl.t) Hashtbl.t) = Hashtbl.create 13                 
let (ht:(string,gvar_type) Hashtbl.t) = Hashtbl.create 47

let gvar_type (m,v) = 
  if m = !Global.name then
    begin
      try Hashtbl.find ht v
      with Not_found -> assert false
    end
  else
    begin
      let ht = try Hashtbl.find exth m with Not_found -> ( prerr_string ("Can't Find "^m^".\n") ; assert false ) in
      try Hashtbl.find ht v with Not_found -> assert false
    end

let gvar_set id ty = Hashtbl.add ht id ty

let rec gen_type ctx = function
  | Ty_GVar (m,v)       -> fprintf !Global.out "%s.%s_c" m v
  | Ty_Var  v           -> 
      if List.mem_assoc v ctx then fprintf !Global.out "{ co=ccon ; id=\"%s\" ; arity=0 ; f=function() return nil end ; args={ } }" v
      else fprintf !Global.out "%s_c" v
  | Ty_Pi (v,a,b)       -> 
      begin
        let vv = match v with None -> "dummy" | Some vv -> vv in
          fprintf !Global.out "{ co=cpi ; ctype= " ;
          gen_type ctx a ;
          fprintf !Global.out " ; f = function(%s_c) return " vv;
          gen_type ctx b ;
          fprintf !Global.out " end } "
      end
  | Ty_App (f,t)        -> 
      begin
        fprintf !Global.out "app( " ;
        gen_ftype ctx f ;
        fprintf !Global.out " , " ;
        gen_term ctx t ;
        fprintf !Global.out " )"
      end
and gen_ftype ctx = function
  | F_GVar (m,v)        -> fprintf !Global.out "%s.%s_c" m v
  | F_Var v             -> 
      if List.mem_assoc v ctx then fprintf !Global.out "{ co=ccon ; id=\"%s\" ; arity=0 ; f=function() return nil end ; args={ } }" v
      else fprintf !Global.out "%s_c" v
  | F_App (f,a)         ->
      begin
        fprintf !Global.out "app( " ;
        gen_ftype ctx f ;
        fprintf !Global.out " , " ;
        gen_term ctx a ;
        fprintf !Global.out " )"
      end
  | F_LamT (v,_,b)      ->
      begin
        fprintf !Global.out "{ co=clam ; f = function(%s_c) return " v ;
        gen_type ctx b ;
        fprintf !Global.out " end } "
      end
  | F_LamF (v,_,b)      ->
      begin
        fprintf !Global.out "{ co=clam ; f = function(%s_c) return " v ;
        gen_ftype ctx b ;
        fprintf !Global.out " end } "
      end
and gen_term ctx = function
  | Te_GVar (m,v)       -> fprintf !Global.out "%s.%s_c" m v
  | Te_Var v            -> 
      if List.mem_assoc v ctx then fprintf !Global.out "{ co=ccon ; id=\"%s\" ; arity=0 ; f=function() return nil end ; args={ } }" v
      else fprintf !Global.out "%s_c" v
  | Te_App (f,a)        ->
      begin
        fprintf !Global.out "app( " ;
        gen_term ctx f ;
        fprintf !Global.out " , " ;
        gen_term ctx a ;
        fprintf !Global.out " )"
      end
  | Te_Lam (v,_,b)      ->
      begin
        fprintf !Global.out "{ co=clam ; f = function(%s_c) return " v ;
        gen_term ctx b ;
        fprintf !Global.out " end } "
      end

let rec gen_subst ctx = function
  | S_Type ty           -> gen_type ctx ty
  | S_Subst (s,t)       ->
      begin
        fprintf !Global.out "pi_app( " ;
        gen_subst ctx s ;
        fprintf !Global.out " , " ;
        gen_term ctx t ;
        fprintf !Global.out " )\n" 
      end
  | S_Pi (v,ty,s)       ->
      begin
        fprintf !Global.out " { co=cpi ; ctype=" ;
        gen_type ctx ty ;
        fprintf !Global.out " ; f=function(%s_c) return " v;
        gen_subst ctx s ;
        fprintf !Global.out " end } "
      end

let conv_subst_subst ctx s1 s2 = 
  match ( s1 , s2 ) with
    | ( S_Type ty1 , S_Type ty2 ) when ty1=ty2  -> ()
    | ( _ , _ )                                 ->
        begin
          fprintf !Global.out "is_conv( 0, " ;
          gen_subst ctx s1 ;
          fprintf !Global.out " , " ;
          gen_subst ctx s2 ;
          fprintf !Global.out " )\n"
        end

let conv_type_subst ctx ty1 = function
  | S_Type ty2 when ty1=ty2     -> ()
  | s                           ->
      begin
        fprintf !Global.out "is_conv( 0, " ;
        gen_type ctx ty1 ;
        fprintf !Global.out " , " ;
        gen_subst ctx s ;
        fprintf !Global.out " )\n"
      end

let conv_type_type ctx ty1 ty2 = 
  if ty1 <> ty2 then
    begin
      fprintf !Global.out "is_conv( 0, " ;
      gen_type ctx ty1 ;
      fprintf !Global.out " , " ;
      gen_type ctx ty2 ;
      fprintf !Global.out " )\n"
    end

let rec conv_arrow_arrow ctx a1 a2 = 
  match ( a1 , a2 ) with
    | ( K_Base b1 , K_Base b2 )                 -> conv_type_type ctx b1 b2
    | ( K_Arrow (b1,ar1') , K_Arrow (b2,ar2') ) -> ( conv_type_type ctx b1 b2 ; conv_arrow_arrow ctx ar1' ar2' )
    | ( _ , _ )                                 -> assert false

let convPi_subst_subst ctx (pi:dk_subst) (a:dk_subst) = 
  match pi with
    | S_Pi (_,ty,_)             -> conv_type_subst ctx ty a
    | S_Type (Ty_Pi (_,ty,_))   -> conv_type_subst ctx ty a
    | S_Type (Ty_GVar id)       -> assert false
    | S_Type (Ty_Var v)         -> assert false
    | s                         ->
        begin
          fprintf !Global.out "convPi( " ;
          gen_subst ctx s ;
          fprintf !Global.out " , " ;
          gen_subst ctx a ;
          fprintf !Global.out " )\n"       
        end

let convPi_arrow_subst ctx (pi:dk_arrow) (a:dk_subst) = 
  match pi with
    | K_Base b          -> conv_type_subst ctx b a
    | K_Arrow (b,_)     -> conv_type_subst ctx b a


let context_add_opt ctx ty = function
  | None        -> ctx
  | Some v      -> (v,GV_Type ty)::ctx

let rec mk_type (ctx:context) : term -> dk_type = function
  | Pi (v,a,b)          -> let a0 = mk_type ctx a in Ty_Pi ( v , a0 , mk_type (context_add_opt ctx a0 v) b )
  | GVar id             -> 
      begin
        match gvar_type id with
          | GV_TYPE     -> Ty_GVar id
          | _           -> assert false
      end
  | App (f,a)           -> 
      let (f_te,f_ty) = mk_ftype ctx f in
      let (a_te,a_ty) = mk_term  ctx a in
        convPi_arrow_subst ctx f_ty a_ty ;
        Ty_App ( f_te , a_te ) 
  | Type                -> assert false
  | Var _               -> assert false (*TODO*) 
  | Lam (_,_,_)         -> assert false

and mk_ftype (ctx:context) : term -> dk_ftype * dk_arrow = function
  | GVar id             -> 
      begin 
        match gvar_type id with
          | GV_Arrow ar -> ( F_GVar id , ar )
          | _           -> assert false
      end
  | App (f,a)           -> 
      let (f_te,f_ty) = mk_ftype ctx f in
      let (a_te,a_ty) = mk_term  ctx a in 
        begin
          match f_ty with
            | K_Base _          -> assert false
            | K_Arrow (ty,ar)   -> ( conv_type_subst ctx ty a_ty ; ( F_App ( f_te , a_te ) , ar ) )
        end
  | Lam (v,Some ty,te)  -> 
      begin
        let ty0 = mk_type ctx ty in
          match te with
            | Type              -> assert false
            | Pi (vv,a,b)       -> 
                let ctx2 = (v,GV_Type ty0)::ctx in
                let a0 = mk_type ctx2 a         in
                  ( F_LamT ( v , ty0 , Ty_Pi ( vv , a0 , mk_type (context_add_opt ctx2 a0 vv) b ) ) , K_Base ty0 )
            | GVar id           -> 
                begin
                  match gvar_type id with
                    | GV_TYPE           -> ( F_LamT (v,ty0,Ty_GVar id) , K_Base ty0 )
                    | GV_Arrow ar       -> ( F_LamF (v,ty0,F_GVar id) ,  K_Arrow (ty0,ar) ) 
                    | GV_Type _         -> assert false 
                end
            | Var _             -> assert false 
            | App (f,a)         -> 
                begin
                  let (a_te,a_ty) = mk_term ctx a in 
                    match mk_ftype ctx f with
                      | ( f_te , K_Base b )       -> ( conv_type_subst ctx b a_ty ; ( F_LamT (v,ty0,Ty_App(f_te,a_te)) , K_Base ty0 ) )
                      | ( f_te , K_Arrow (b,ar) ) -> ( conv_type_subst ctx b a_ty ; ( F_LamF (v,ty0,F_App (f_te,a_te)) , K_Arrow (ty0,ar) ) )
                end
            | Lam (_,_,_)       -> 
                let (te0,ar) = mk_ftype ctx te in
                  ( F_LamF ( v , ty0 , te0 ) , K_Arrow (ty0,ar) )
      end 
  | Type                -> assert false
  | Pi (_,_,_)          -> assert false
  | Var _               -> assert false (*TODO*)
  | Lam (_,None,_)      -> assert false

and mk_term (ctx:context) : term -> dk_term * dk_subst = function
  | GVar id             -> 
      begin
        match gvar_type id with
          | GV_Type ty  -> ( Te_GVar id , S_Type ty )
          | _           -> assert false
      end
  | Var v               -> 
      begin
        match List.assoc v ctx with
          | GV_TYPE     -> assert false
          | GV_Arrow _  -> assert false
          | GV_Type ty  -> ( Te_Var v , S_Type ty )
      end 
  | App (f,a)           -> 
      let (f_te,f_ty) = mk_term ctx f in
      let (a_te,a_ty) = mk_term ctx a  in 
        convPi_subst_subst ctx f_ty a_ty ;
        ( Te_App ( f_te , a_te ) , S_Subst (f_ty,a_te) ) 
  | Lam (v,Some ty,te)  -> 
      let ty0     = mk_type ctx ty in
      let (te0,b) = mk_term ((v,GV_Type ty0)::ctx) te in
      ( Te_Lam ( v , ty0 , te0 ) , S_Pi (v,ty0,b) )
  | Type                -> assert false
  | Pi (_,_,_)          -> assert false
  | Lam (_,None,_)      -> assert false 

type mkT =
  | MkT_Kind  of dk_arrow option        (* None -> 'Type':Kind | Some ar -> 'ar':Kind *)
  | MkT_FType of dk_ftype*dk_arrow      (* 'dk_ftype':dk_arrow *)
  | MkT_Type  of dk_type                (* 'dk_type':Type *)
  | MkT_Term  of dk_term*dk_subst       (* 'dk_term':dk_subst *)

let rec type_inference (ctx:context) : term -> mkT = function
  | Type                -> MkT_Kind None
  | Pi (v,a,b)          ->
      begin
        let a0 = mk_type ctx a in  
          match type_inference (context_add_opt ctx a0 v) b with
            | MkT_Kind None             -> MkT_Kind (Some (K_Base a0))
            | MkT_Kind (Some ar)        -> MkT_Kind (Some (K_Arrow (a0,ar)))
            | MkT_Type ty               -> MkT_Type (Ty_Pi (v,a0,ty))
            | MkT_FType _               -> assert false
            | MkT_Term (_,_)            -> assert false
      end
  | GVar id          -> 
      begin
        match gvar_type id with
          | GV_TYPE     -> MkT_Type (Ty_GVar id)
          | GV_Arrow ar -> MkT_FType (F_GVar id,ar)
          | GV_Type ty  -> MkT_Term (Te_GVar id,S_Type ty)
      end
  | App (f,a)           -> 
      begin 
        let (a_te,a_ty) = 
          match type_inference ctx a with
            | MkT_Term (te,ty)  -> (te,ty)
            | _                 -> assert false
        in
        match type_inference ctx f with
          | MkT_FType (f_te,K_Base ty)          -> ( conv_type_subst ctx ty a_ty ; MkT_Type  (Ty_App (f_te,a_te)) )
          | MkT_FType (f_te,K_Arrow (ty,ar))    -> ( conv_type_subst ctx ty a_ty ; MkT_FType (F_App (f_te,a_te),ar) )
          | MkT_Term  (f_te,ty)                 -> ( conv_subst_subst ctx ty a_ty ; MkT_Term  (Te_App (f_te,a_te),S_Subst (ty,a_te)) )
          | _                                   -> assert false
      end
  | Var v               -> 
      begin
        match List.assoc v ctx with
          | GV_TYPE     -> MkT_Type  (Ty_Var v)
          | GV_Arrow ar -> MkT_FType (F_Var v,ar)
          | GV_Type ty  -> MkT_Term  (Te_Var v, S_Type ty)
      end
  | Lam (v,Some a,f)    -> 
      begin
        let a_ty = mk_type ctx a in
          match type_inference ((v,GV_Type a_ty)::ctx) f with
            | MkT_FType (f_te,ar) -> MkT_FType ( F_LamF (v,a_ty,f_te) , K_Arrow (a_ty,ar) )
            | MkT_Type f_te       -> MkT_FType ( F_LamT (v,a_ty,f_te) , K_Base a_ty )
            | MkT_Term  (f_te,ty) -> MkT_Term  ( Te_Lam (v,a_ty,f_te) , S_Pi (v,a_ty,ty) ) 
            | _                   -> assert false
      end
  | Lam (v,None,f)      -> assert false 

(* ENTRY POINT *)

let typecheck_decl id loc ty = 
  fprintf !Global.out "print_debug(\"%s\tChecking declaration %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  ( match type_inference [] ty with
      | MkT_Kind None             -> gvar_set id GV_TYPE
      | MkT_Kind (Some ar)        -> gvar_set id (GV_Arrow ar)
      | MkT_Type ty               -> gvar_set id (GV_Type ty)
      | MkT_FType _               -> assert false
      | MkT_Term (_,_)            -> assert false
  ) ;
  fprintf !Global.out "print_ok()\n"

let typecheck_def id loc te ty = 
  fprintf !Global.out "print_debug(\"%s\tChecking definition %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  ( match ( type_inference [] te , type_inference [] ty ) with
    | ( MkT_Kind _ , _ )                                -> assert false
    | ( MkT_Type _ , MkT_Kind None )                    -> gvar_set id GV_TYPE
    | ( MkT_Type _ , _ )                                -> assert false
    | ( MkT_FType (_,ar1) , MkT_Kind (Some ar2) )       -> ( conv_arrow_arrow [] ar1 ar2 ; gvar_set id (GV_Arrow ar2) )
    | ( MkT_FType _   , _ )                             -> assert false
    | ( MkT_Term (_,ty1) , MkT_Type ty2 )               -> ( conv_type_subst [] ty2 ty1 ; gvar_set id (GV_Type ty2) )
    | ( MkT_Term (_,ty1) , _ )                          -> assert false );
  fprintf !Global.out "print_ok()\n"

let apply f args = Array.fold_left (fun f a -> App (f,a) ) f args
let rec pat_to_term = function
  | Joker               -> assert false
  | Id id               -> Var id
  | Pat ((m,v),dots,pats)  -> apply (apply (GVar (m,v)) dots) (Array.map pat_to_term pats) 

let gen_env (ctx:context) (((v,loc),ty):(var*loc)*term) =
  let ty0 = match type_inference ctx ty with
    | MkT_Kind None             -> GV_TYPE
    | MkT_Kind (Some ar)        -> GV_Arrow ar
    | MkT_Type ty0              -> GV_Type ty0
    | MkT_FType _               -> assert false
    | MkT_Term _                -> assert false
  in
  fprintf !Global.out "local %s_c = { co = ccon ; id = \"%s\" ; ctype = " v v ; 
  CodeGeneration.gen_code ty ;
  fprintf !Global.out " ; arity = 0 ; args = { } ; f = function() return nil end}\n" ; (v,ty0)::ctx

let typecheck_rule id i ((loc,env,dots,pats,ri):rule) = 
  fprintf !Global.out "print_debug(\"%s\tChecking Rule %i of %s\t\t\")\n" (Debug.string_of_loc loc) (i+1) id ;
  fprintf !Global.out "do\n" ;
  let ctx = List.fold_left gen_env [] env                       in 
  let le = pat_to_term (Pat ((!Global.name,id),dots,pats))      in
    ( match ( type_inference ctx le , type_inference ctx ri ) with 
        | ( MkT_Type  _ , MkT_Type _ )                  -> ()
        | ( MkT_FType (_,ar1) , MkT_FType (_,ar2) )     -> conv_arrow_arrow ctx ar1 ar2
        | ( MkT_Term  (_,ty1) , MkT_Term (_,ty2) )      -> conv_subst_subst ctx ty1 ty2 
        | ( _ , _ )                                     -> assert false ) ;
    fprintf !Global.out "end\n" ; 
    fprintf !Global.out "print_ok()\n" 

let generate_decl id ty =
  fprintf !Global.out "%s.%s_c = { co=ccon ; id=\"%s.%s\" ; ctype = " !Global.name id !Global.name id; 
  CodeGeneration.gen_code ty ;
  fprintf !Global.out " ; arity=0 ; f = function () return nil end ; args = {} } "; 
  fprintf !Global.out "\n\n" 

let generate_def id te =
  fprintf !Global.out "%s.%s_def = " !Global.name id ;
  CodeGeneration.gen_code te ;
  fprintf !Global.out "\n\n" 
 
let generate_rules_code id rs =
  fprintf !Global.out "%s.%s_c = " !Global.name id ;
  CodeGeneration.generate_rules_code2 id rs ;
  fprintf !Global.out "\n" 

let export _    = 
  try Marshal.to_channel (open_out (!Global.name^".dko" )) ht []
  with Sys_error _ -> assert false
                        
let import name = 
  try Hashtbl.add exth name (Marshal.from_channel (open_in (name^".dko" )))
  with Sys_error _ -> assert false
