
open Types

(* Inference/Verification de type *)

(* infers the type of a term (None=Kind)*)
let rec infer (ctx:term RAL.t) = function
  | Type        -> None                               (* Sort*)
  | DB n        -> Some (Term.shift2 (n+1) 0 (RAL.get ctx n)) (* Local Var *)
  | GVar (m,v)  -> Some (Env.get_type (m,v))          (* Global Var *)
  | Pi (a,b)    ->                                    (* Product *)
      ( is_type ctx a ; 
        match infer (RAL.cons a ctx) b with
          | None (*Kind*)       -> None                 
          | Some Type           -> Some Type
          | Some ty             -> raise (TypingError (SortExpected ty))
      )
  | Lam (a,t)   ->                                    (* Abstraction *)
      ( is_type ctx a ; 
        match infer (RAL.cons a ctx) t with
          | None (*Kind*)       -> raise (TypingError TopSortError)
          | Some b              -> Some (Pi (a,b)) 
      )
  | App  (f,u)  ->                                    (* Application *)
      let (a2,b) = 
        match infer ctx f with 
          | Some (Pi (a2,b))            -> (a2,b)
          | Some (App  (_,_) as te)     
          | Some (GVar (_,_) as te)     ->
              ( match Term.wnf te with
                  | Pi  (a2,b)  -> (a2,b)
                  | tf          -> raise (TypingError (ProductExpected (Some tf))) 
              )
          | tf                          -> raise (TypingError (ProductExpected tf))
      in
        ( match infer ctx u with
            | None      -> raise (TypingError (CannotConvert (None,a2)))
            | Some a1   ->
                if Term.is_conv a1 a2 then Some (Term.subst b u)
                else raise (TypingError (CannotConvert (Some a1,a2)))
        )
               
(* Checks that a term has type Type *) 
and is_type ctx te =
  match infer ctx te with
    | Some Type       -> ()
    | ty              -> raise (TypingError (TypeExpected ty))

(* Checks that |- te:ty *)
let check_term te ty =
  match infer RAL.empty te with
    | None (*Kind*)     -> raise (TypingError (CannotConvert (None,ty)))
    | Some ty0          -> if not (Term.is_conv ty ty0) then raise (TypingError (CannotConvert (Some ty0,ty)))

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type env ty = 
  match infer env ty with
    | None      -> ()
    | Some Type -> ()
    | Some s    -> raise (TypingError (SortExpected s))

(* Entry *)

let mk_prelude ((l,v):lvar) : unit =
  Global.msg ( Debug.string_of_loc l ^ "[Name] " ^ v ^ ".\n") ;
  Global.name := v

let mk_require (l,v) : unit = 
  Global.msg ( Debug.string_of_loc l ^ "[Import] " ^ v ^ ".\n") ;
  Env.import v

let mk_declaration (((l,id),pty):lvar*pterm) : unit = 
  let ty = Term.of_pterm pty in
    Global.msg ( Debug.string_of_loc l ^ "[Declaration] " ^ id ^ ".\n" ) ;
    check_type RAL.empty ty ;
    Env.add_decl (id,ty)

let mk_definition (((l,id),pty,pte):lvar*pterm*pterm) : unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[Definition] " ^ id ^ ".\n") ;
    check_type RAL.empty ty ;
    check_term te ty ;
    Env.add_def (id,te,ty) 

let mk_opaque (((l,id),pty,pte):lvar*pterm*pterm) : unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[Opaque] " ^ id ^ ".\n") ;
    check_type RAL.empty ty ;
    check_term te ty ;
    Env.add_decl (id,ty) 

let mk_typecheck ((l,pty,pte):loc*pterm*pterm) :unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[TypeCheck] _ \n") ;
    check_type RAL.empty ty ;
    check_term te ty

let mk_rules (lst:rule list) : unit = assert false (*TODO*) 

let mk_ending _ : unit = () 
 
(* End of File *)
