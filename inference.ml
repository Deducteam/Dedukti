
open Types

(* ty ?~ Type *)
let is_type te = function
    | Type      -> ()
    | ty        -> raise (TypingError (Error.err_conv te Type ty))

let mk_app f u =
  match f with
    | App lst   -> App (lst@[u])
    | _         -> App([f;u])

(* Type Inference *)
let rec infer (ctx:term list) (te:term) : term = 
  match te with
    | Type                              -> Kind
    | DB n                              -> ( (*assert (n<k) ;*) Subst.shift (n+1) 0 (List.nth ctx n) )
    | GVar (m,v)                        -> Env.get_global_type dloc (*FIXME*) m v
    | Pi (a,b)                          ->
        begin
          is_type a (infer ctx a) ;
          match infer (a::ctx) b with 
            | Kind      -> Kind
            | Type      -> Type
            | ty        -> raise (TypingError (Error.err_sort b ty))
        end
    | Lam (a,t)                         -> 
        begin
          is_type a (infer ctx a) ;
          match infer (a::ctx) t with 
            | Kind        -> raise (TypingError (Error.err_topsort te))
            | b           -> Pi (a,b)
        end
    | App ( f::((_::_) as args) )       ->
        begin
          snd ( List.fold_left (
            fun (f,ty_f) u ->
              match Reduction.hnf ty_f , infer ctx u with
                | ( Pi (a,b) , a' ) ->  
                    if Reduction.are_convertible a a' then ( mk_app f u , Subst.subst b u )
                    else raise (TypingError (Error.err_conv u a a'))
                | ( t , _ )         -> raise (TypingError (Error.err_prod f ty_f)) 
          ) (f,infer ctx f) args )
        end
    | App _             -> assert false
    | Kind              -> assert false
    | Meta _            -> assert false

let rec concat (l:(term*term) list) : (term*term) list -> (term*term) list = function
  | []          -> l
  | a::l2       -> a::(concat l l2)

let rec term_of_pattern = function 
  | Var n                       -> DB n
  | Dash n                      -> Meta n
  | Pattern ((m,v),[||])        -> GVar (m,v)
  | Pattern ((m,v),args)        -> App( GVar (m,v) :: (List.map term_of_pattern (Array.to_list args)) )

(* Pattern Inference *)                                     

let rec infer_pattern (ctx:term list) : pattern -> term*(term*term) list = function
  | Var n                       -> (* assert (n<List.length ctx); *) ( Subst.shift (n+1) 0 (List.nth ctx n) , [] )
  | Dash _                      -> assert false 
  | Pattern ((m,v),pats)        ->
      let aux (pi,lst:term*(term*term)list) (arg:pattern) : term*(term*term) list =
          match Reduction.hnf pi with
            | Pi (a,b) -> 
               begin
                 let lst2 = check_pattern ctx a arg in
                   ( Subst.subst b (term_of_pattern arg) , (concat lst lst2) ) (*FIXME concat*)
               end
            | _         -> raise (TypingError (Error.err_prod2 pi))
      in
        Array.fold_left aux ( Env.get_global_type dloc (*FIXME*) m v , [] ) pats
                                      
and check_pattern (ctx:term list) (ty:term): pattern -> (term*term) list = function
  | Dash _      -> []
  | p           ->
      let (ty2,lst) = infer_pattern ctx p in
        (ty,ty2)::lst

