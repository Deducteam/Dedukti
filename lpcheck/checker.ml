
open Types

(* Type checking/inference*)

(* ty?=Type *)
let is_type te = function
    | Type      -> ()
    | ty        -> raise (TypingError ("Error while typing "^Debug.string_of_term te ^".\n Expected type: Type.\nInfered type: "^Debug.string_of_term ty^".\n"))

let mk_app f u =
  match f with
    | App lst   -> App (lst@[u])
    | _         -> App([f;u])

(* Computes a type for a given term *)
(* Invariant: k == List.length ctx *)      
let rec infer (k:int) (ctx:term list) (te:term) : term = 
  match te with
    | Type                              -> Kind
    | DB n                              -> ( assert (n<k) ; Term.shift (n+1) 0 (List.nth ctx n) ) (*FIXME*)
    | GVar (m,v)                        -> Env.get_global_type m v
    | Pi (a,b)                          ->
        begin
          is_type a (infer k ctx a) ;
          match infer (k+1) (a::ctx) b with 
            | Kind      -> Kind
            | Type      -> Type
            | ty        -> assert false (*raise (TypingError ("Error while typing "^Debug.string_of_term b ^".\n Expected type: Type or Kind.\nInfered type: "^Debug.string_of_term ty^".\n"))FIXME*)
        end
    | Lam (a,t)                         -> 
        begin
          is_type a (infer k ctx a) ;
          match infer (k+1) (a::ctx) t with 
            | Kind        -> raise (TypingError ("TopSort")) (*FIXME*) 
            | b           -> Pi (a,b)
        end
    | App ( f::((_::_) as args) )       ->
        begin
          List.fold_left (
            fun ty_f u ->
              match Term.hnf ty_f , infer k ctx u with
                | ( Pi (a,b) , a' ) ->  
                    if Term.are_convertible a a' then Term.subst b u
                    else raise (TypingError (("Cannot convert "^Debug.string_of_term a^" with "^Debug.string_of_term a')))
                | ( t , _ )         -> raise (TypingError ("ProductExpected")) (*FIXME*)
          ) (infer k ctx f) args
        end
    | App _             -> assert false
    | Kind              -> assert false
    | LVar _            -> assert false


(* Checks that |- te:ty *)
let check_term te ty = 
  if not (Term.are_convertible (infer 0 [] te) ty ) then 
    raise (TypingError ("CannotConvert")) (*FIXME*)

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type k ctx ty = 
  match infer k ctx ty with
    | Kind | Type       -> ()
    | _                 -> raise (TypingError ("SortExpected")) (*FIXME*)

let check_rule id (penv,ple,pri) : rule2 = 
  (* FIXME check id *)
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
      raise (TypingError (("Cannot convert "^Debug.string_of_term ty_le^" with "^Debug.string_of_term ty_ri)))

