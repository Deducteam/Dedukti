
open Types

let err_conv te exp inf =  
  "Error while typing "^Debug.string_of_term te ^".\nExpected type: "^Debug.string_of_term exp^".\nInferred type: "^Debug.string_of_term inf^".\n"

let err_sort te ty =
  "Error while typing "^Debug.string_of_term te ^".\n Expected type: Type or Kind.\nInferred type: "^Debug.string_of_term ty^".\n"

let err_topsort te = 
  "Error while typing "^Debug.string_of_term te ^".\n Expected type: anything but Kind.\nInferred type: Kind.\n"

let err_prod te ty = 
  "Error while typing "^Debug.string_of_term te ^".\n Product expected.\nInferred type: "^Debug.string_of_term ty^".\n"


(* Type checking/inference*)

(* ty?=Type *)
let is_type te = function
    | Type      -> ()
    | ty        -> raise (TypingError (err_conv te Type ty))

let mk_app f u =
  match f with
    | App lst   -> App (lst@[u])
    | _         -> App([f;u])

(* Computes a type for a given term *)
(* Invariant: k == List.length ctx *)      
let rec infer (k:int) (ctx:term list) (te:term) : term = 
  match te with
    | Type                              -> Kind
    | DB n                              -> ( (*assert (n<k) ;*) Term.shift (n+1) 0 (List.nth ctx n) )
    | GVar (m,v)                        -> Env.get_global_type m v
    | Pi (a,b)                          ->
        begin
          is_type a (infer k ctx a) ;
          match infer (k+1) (a::ctx) b with 
            | Kind      -> Kind
            | Type      -> Type
            | ty        -> raise (TypingError (err_sort b ty))
        end
    | Lam (a,t)                         -> 
        begin
          is_type a (infer k ctx a) ;
          match infer (k+1) (a::ctx) t with 
            | Kind        -> raise (TypingError (err_topsort te))
            | b           -> Pi (a,b)
        end
    | App ( f::((_::_) as args) )       ->
        begin
          snd ( List.fold_left (
            fun (f,ty_f) u ->
              match Term.hnf ty_f , infer k ctx u with
                | ( Pi (a,b) , a' ) ->  
                    if Term.are_convertible a a' then ( mk_app f u , Term.subst b u )
                    else raise (TypingError (err_conv u a a'))
                | ( t , _ )         -> raise (TypingError (err_prod f ty_f)) 
          ) (f,infer k ctx f) args )
        end
    | App _             -> assert false
    | Kind              -> assert false
    | LVar _            -> assert false


(* Checks that |- te:ty *)
let check_term te ty = 
  let ty' = infer 0 [] te in
    if not (Term.are_convertible ty ty') then 
      raise (TypingError (err_conv te ty ty'))

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type k ctx ty = 
  match infer k ctx ty with
    | Kind | Type       -> ()
    | s                 -> raise (TypingError (err_sort ty s)) 

let check_rule id (penv,ple,pri) : rule2 = 
  let (k,names,ctx:int*string list*term list) = 
    List.fold_left (
      fun (k,names,ctx) ((_,v),pty) ->
        let ty = Term.of_pterm k names pty in
          check_type k ctx ty ; (k+1,v::names,ty::ctx)
    ) (0,[],[]) penv in

  let le  = Term.term_of_tpat k names ple in
  let ri  = Term.of_pterm     k names pri in
  let ty_le = infer k ctx le in
  let ty_ri = infer k ctx ri in
    if Term.are_convertible ty_le ty_ri then (names,ple,ri) 
    else
      raise (TypingError (err_conv ri ty_le ty_ri))

