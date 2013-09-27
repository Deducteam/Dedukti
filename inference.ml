
open Types

(* ty ?~ Type *)
let is_type te = function
    | Type      -> ()
    | ty        -> raise (TypingError (Error.err_conv te Type ty))

let mk_app f u =
  match f with
    | App lst   -> App (lst@[u])
    | _         -> App([f;u])

(* Computes a type for a given term *)
(* Invariant: k == List.length ctx *)      
let rec infer (k:int) (ctx:term list) (te:term) : term = 
  match te with
    | Type                              -> Kind
    | DB n                              -> ( (*assert (n<k) ;*) Subst.shift (n+1) 0 (List.nth ctx n) )
    | GVar (m,v)                        -> Env.get_global_type m v
    | Pi (a,b)                          ->
        begin
          is_type a (infer k ctx a) ;
          match infer (k+1) (a::ctx) b with 
            | Kind      -> Kind
            | Type      -> Type
            | ty        -> raise (TypingError (Error.err_sort b ty))
        end
    | Lam (a,t)                         -> 
        begin
          is_type a (infer k ctx a) ;
          match infer (k+1) (a::ctx) t with 
            | Kind        -> raise (TypingError (Error.err_topsort te))
            | b           -> Pi (a,b)
        end
    | App ( f::((_::_) as args) )       ->
        begin
          snd ( List.fold_left (
            fun (f,ty_f) u ->
              match Reduction.wnf ty_f , infer k ctx u with
                | ( Pi (a,b) , a' ) ->  
                    if Reduction.are_convertible a a' then ( mk_app f u , Subst.subst b u )
                    else raise (TypingError (Error.err_conv u a a'))
                | ( t , _ )         -> raise (TypingError (Error.err_prod f ty_f)) 
          ) (f,infer k ctx f) args )
        end
    | App _             -> assert false
    | Kind              -> assert false
    | LVar _            -> assert false


