
open Types

type dk_type = 
  | Ty_GVar of id
  | Ty_Pi  of var option*dk_type*dk_type
  | Ty_App of dk_ftype*dk_term
and dk_ftype = 
  | F_GVar  of id
  | F_App  of dk_ftype*dk_term
  | F_LamT of var*dk_type*dk_type
  | F_LamF of var*dk_type*dk_ftype
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
  | GV_Type
  | GV_Arrow of dk_arrow   
  | GV_Term  of dk_type    

let gvar_type id = GV_Type

let rec mk_type : term -> dk_type = function
  | Pi (v,a,b)          -> Ty_Pi ( v , mk_type a , mk_type b )
  | GVar v              -> 
      begin
        match gvar_type v with
          | GV_Type     -> Ty_GVar v
          | _           -> assert false
      end
  | App (f,a)           -> 
      let (f_te,f_ty) = mk_ftype f in
      let (a_te,a_ty) = mk_term a  in
        Ty_App ( f_te , a_te ) (*FIXME check convPi f_ty a_ty*)
  | Type                -> assert false
  | Var _               -> assert false 
  | Lam (_,_,_)         -> assert false
and mk_ftype : term -> dk_ftype * dk_arrow = function
  | GVar v              -> 
      begin 
        match gvar_type v with
          | GV_Arrow ar -> ( F_GVar v , ar )
          | _           -> assert false
      end
  | App (f,a)           -> 
      let (f_te,f_ty) = mk_ftype f in
      let (a_te,a_ty) = mk_term a  in 
        begin
          match f_ty with
            | K_Base _          -> assert false
            | K_Arrow (ty,ar)   -> ( F_App ( f_te , a_te ) , ar ) (*FIXME check conv ty a_ty*)
        end
  | Lam (v,Some ty,te)  -> 
      begin
        let ty0 = mk_type ty in
          match te with
            | Type              -> assert false
            | Pi (vv,a,b)       -> ( F_LamT ( v , ty0 , Ty_Pi ( vv , mk_type a , mk_type b ) ) , K_Base ty0 )
            | GVar vv           -> 
                begin
                  match gvar_type vv with
                    | GV_Type           -> ( F_LamT (v,ty0,Ty_GVar vv) , K_Base ty0 )
                    | GV_Arrow ar       -> ( F_LamF (v,ty0,F_GVar vv) ,  K_Arrow (ty0,ar) )
                    | GV_Term _         -> assert false 
                end
            | Var _             -> assert false 
            | App (f,a)         -> 
                begin
                  let (a_te,a_ty) = mk_term a in (*FIXME check conv b a_ty*)
                    match mk_ftype f with
                      | ( f_te , K_Base b )       -> ( F_LamT (v,ty0,Ty_App(f_te,a_te)) , K_Base ty0 )
                      | ( f_te , K_Arrow (b,ar) ) -> ( F_LamF (v,ty0,F_App (f_te,a_te)) , K_Arrow (ty0,ar) )
                end
            | Lam (_,_,_)       -> 
                let (te0,ar) = mk_ftype te in
                  ( F_LamF ( v , ty0 , te0 ) , K_Arrow (ty0,ar) )
      end 
  | Type                -> assert false
  | Pi (_,_,_)          -> assert false
  | Var _               -> assert false
  | Lam (_,_,_)         -> assert false
and mk_term : term -> dk_term * dk_subst = function
  | GVar v              -> 
      begin
        match gvar_type v with
          | GV_Term ty  -> ( Te_GVar v , S_Type ty )
          | _           -> assert false
      end
  | Var v               -> ( Te_Var v , S_Type (Ty_GVar ("","")) ) (*FIXME*)
  | App (f,a)           -> 
      let (f_te,f_ty) = mk_term f in
      let (a_te,a_ty) = mk_term a  in 
        (* FIXME check convPi *)
        ( Te_App ( f_te , a_te ) , S_Subst (f_ty,a_te) ) 
  | Lam (v,Some ty,te)  -> 
      let ty0     = mk_type ty in
      let (te0,b) = mk_term te in
      ( Te_Lam ( v , ty0 , te0 ) , S_Pi (v,ty0,b) )
  | Type                -> assert false
  | Pi (v,a,b)          -> assert false
  | Lam (v,None,te)     -> assert false 

type dk = 
  | DK_Kind of dk_arrow option
  | DK_Type of dk_type

let rec mk_Type_or_Kind = function
  | Type                -> DK_Kind None
  | Pi (v,a,b)          ->
      begin
        let a0 = mk_type a in  
          match mk_Type_or_Kind b with
            | DK_Kind None      -> DK_Kind (Some (K_Base a0))
            | DK_Kind (Some ar) -> DK_Kind (Some (K_Arrow (a0,ar)))
            | DK_Type ty        -> DK_Type (Ty_Pi (v,a0,ty))
      end
  | GVar id             -> 
      begin
        match gvar_type id with
          | GV_Type     -> DK_Type (Ty_GVar id)
          | GV_Arrow ar -> assert false
          | GV_Term _   -> assert false
      end
  | App (_,_) as ty     -> DK_Type (mk_type ty)
  | Var _               -> assert false
  | Lam (_,_,_)         -> assert false

