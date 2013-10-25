
open Types

(* ty ?~ Type *)
let is_type lc te = function
    | Type      -> ()
    | ty        -> raise (TypingError ( lc , Error.err_conv_type te ty ))
(*
let mk_app f u =
  match f with
    | App lst   -> App (lst@[u])
    | _         -> App([f;u])
 *)
(* Type Inference *) 
let rec infer lc (ctx:term list) (te:term) : term = 
  match te with
    | Type                              -> mk_kind 
    | DB n                              -> ( (*assert (n<k) ;*) Subst.shift (n+1) 0 (List.nth ctx n) )
    | GVar (m,v)                        -> Env.get_global_type lc m v
    | Pi (a,b)                          ->
        begin
          is_type lc a (infer lc ctx a) ;
          match infer lc (a::ctx) b with 
            | Kind | Type as t  -> t
            | ty                -> raise (TypingError (lc,Error.err_sort b ty))
        end
    | Lam (a,t)                         -> 
        begin
          is_type lc a (infer lc ctx a) ;
          match infer lc (a::ctx) t with 
            | Kind        -> raise (TypingError (lc,Error.err_topsort te))
            | b           -> mk_pi a b
        end
    | App ( f0::((_::_) as args) )      ->
        begin
          snd ( List.fold_left ( (*TODO extract*)
            fun (f,ty_f) u ->
              match Reduction.wnf ty_f , infer lc ctx u with 
                | ( Pi (a,b) , a' ) ->  
                    if Reduction.are_convertible a a' then ( mk_app [f;u] , Subst.subst b u )
                    else raise (TypingError (lc,Error.err_conv2 u a a' (Reduction.hnf a) (Reduction.hnf a')))
                | ( t , _ )         -> raise (TypingError (lc,Error.err_prod f ty_f)) 
          ) (f0,infer lc ctx f0) args )
        end
    | App _ | Kind | Meta _             -> assert false
(*
let rec concat (l:(term*term) list) : (term*term) list -> (term*term) list = function
  | []          -> l
  | a::l2       -> a::(concat l l2)
 *)
let rec term_of_pattern = function 
  | Var n                       -> mk_db n
  | Dash n                      -> mk_meta n
  | Pattern ((_,m,v),[||])      -> mk_gvar m v
  | Pattern ((_,m,v),args)      -> mk_uapp ( (mk_gvar m v) :: (List.map term_of_pattern (Array.to_list args)) )

(* Pattern Inference *)                                     

let rec infer_pattern (ctx:term list) : pattern -> term*(term*term) list = function
  | Var n                       -> (* assert (n<List.length ctx); *) ( Subst.shift (n+1) 0 (List.nth ctx n) , [] )
  | Dash _                      -> assert false 
  | Pattern ((l,m,v),pats)      ->
      let aux (pi,lst:term*(term*term)list) (arg:pattern) : term*(term*term) list =
          match Reduction.hnf pi with
            | Pi (a,b) -> 
               begin
                 let lst2 = check_pattern ctx a arg in
                   ( Subst.subst b (term_of_pattern arg) , (*concat lst lst2*) lst@lst2 )
               end
            | _         -> raise (TypingError (l,Error.err_prod2 pi))
      in
        Array.fold_left aux ( Env.get_global_type l m v , [] ) pats
                                      
and check_pattern (ctx:term list) (ty:term): pattern -> (term*term) list = function
  | Dash _      -> []
  | p           ->
      let (ty2,lst) = infer_pattern ctx p in
        (ty,ty2)::lst

