
open Types

(* Local scope *)

let ls : term IntH.t = IntH.create 47
let lenv_add n ty = IntH.add ls n ty 
let lenv_remove n = IntH.remove ls n
let lenv_get n = 
  try IntH.find ls n
  with Not_found -> assert false 

(*
let dump_ls () =
  IntH.iter (
    fun i ty ->
      Global.print_v ("\t"^string_of_int i^" -> "^Debug.string_of_term ty^"\n")
  ) ls
*)

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
let rec infer (k:int) (te:term) : term = 
  match te with
    | Kind              -> assert false
    | LVar _            -> assert false
    | Type              -> Kind
    | DB n              -> Term.shift2 (n+1) 0 (lenv_get (k-1-n)) 
    | GVar (m,v)        -> Env.get_global_type m v       
    | RVar v            -> Term.rs_find v
    | Pi (a,b)          ->                                    
        ( is_type a (infer k a) ; lenv_add k a ; 
          let s = match infer (k+1) b with 
            | Kind        -> Kind
            | Type        -> Type
            | ty           -> raise (TypingError ("Error while typing "^Debug.string_of_term b ^".\n Expected type: Type or Kind.\nInfered type: "^Debug.string_of_term ty^".\n"))
          in
            lenv_remove k ; s ) 
    | Lam (a,t)         -> 
        ( is_type a (infer k a) ; lenv_add k a ; 
          let ty = match infer (k+1) t with 
            | Kind        -> raise (TypingError ("TopSort")) (*FIXME*) 
            | b           -> Pi (a,b)
          in
            lenv_remove k ; ty )
    | App ( f::((_::_) as args) ) -> (*FIXME ???*)
        snd (List.fold_left (
          fun (f,ty_f) u ->
            match Term.hnf ty_f , infer k u with
              | ( Pi (a,b) , a' ) ->  
                  if Term.are_convertible a a' then (mk_app f u,Term.subst b u)
                  else raise (TypingError (("Cannot convert "^Debug.string_of_term a^" with "^Debug.string_of_term a'))) (*FIXME*)
              | ( t , _ )         -> raise (TypingError ("ProductExpected")) (*FIXME*)
        ) (f,infer k f) args)
    | App _             -> assert false

(* Checks that |- te:ty *)
let check_term te ty = 
  if not (Term.are_convertible (infer 0 te) ty ) then 
    raise (TypingError ("CannotConvert")) (*FIXME*)

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type ty = 
  match infer 0 ty with
    | Kind | Type       -> ()
    | _                 -> raise (TypingError ("SortExpected")) (*FIXME*)

let check_rule id (penv,ple,pri) : rule2 = 
  (* FIXME check id *)
  let env:string list = List.map (
    fun ((_,v),pty) ->
      let ty = Term.of_pterm pty in
        check_type ty ; 
        Term.rs_add v ty ; v
  ) penv in
  let le  = Term.term_of_tpat ple in
  let ri  = Term.of_pterm pri in
  let ty_le = infer 0 le in
  let ty_ri = infer 0 ri in
    if Term.are_convertible ty_le ty_ri then (
      List.iter (fun v -> Term.rs_remove v) env ;
      (env,ple,ri) 
    ) else 
      assert false (*FIXME*)

