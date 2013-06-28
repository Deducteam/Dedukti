
open Types
open Term
open Hashcons

(* Inference/Verification de type *)

(* infers the type of a term (None=Kind)*)
let rec infer (ctx:term list) (t:term) : term option = 
  match t.node with
  | Type        -> None                               (* Sort*)
  | DB n        -> Some (List.nth ctx n)              (* Local Var *)
  | GVar (m,v)  -> 
      ( match Env.get_type (m,v) with
          | Env.HTerm ty    -> Some ty
          | Env.NotHTerm ty -> Some (full_hcons ty)
      )          
  | Pi (a,b)    ->                                    (* Product *)
      ( is_type ctx a ; 
        match infer (shift_lst (a::ctx)) b with
          | None (*Kind*)                       -> None                 
          | Some ty when (ty.tag = htype.tag)   -> Some ty 
          | Some ty                             -> raise (TypingError (SortExpected ty))
      )
  | Lam (a,t)   ->                                    (* Abstraction *)
      ( is_type ctx a ; 
        match infer (shift_lst (a::ctx)) t with
          | None (*Kind*)       -> raise (TypingError TopSortError)
          | Some b              -> Some (hpi (a,b)) 
      )
  | App  (f,u)  ->                                    (* Application *)
      ( match ( infer ctx f , infer ctx u ) with
          | ( Some pi , Some a1 )              ->
              ( match (wnf pi).node with
                  | Pi (a2,b)   ->
                      if Term.is_conv a1 a2 then Some (Term.subst b u)
                      else failwith "infer (App) (1)" 
                  | _           -> failwith "infer (App) (2)"
              )
          | ( _ , _ )                                   -> failwith "infer (App) (3)" 
      )
               
(* Checks that a term has type Type *) 
and is_type (ctx:term list) (te:term) : unit =
  match infer ctx te with
    | Some ty when (ty.tag=htype.tag)   -> ()
    | ty                                -> raise (TypingError (TypeExpected ty))

(* Checks that |- te:ty *)
let check_term (te:term) (ty:term) : unit =
  match infer [] te with
    | None (*Kind*)     -> raise (TypingError (CannotConvert (None,ty)))
    | Some ty0          -> if not (Term.is_conv ty ty0) then raise (TypingError (CannotConvert (Some ty0,ty)))

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type (ty:term) = 
  match infer [] ty with
    | None                              -> ()
    | Some ty when ty.tag=htype.tag     -> ()
    | Some s                            -> raise (TypingError (SortExpected s))

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
    check_type ty ;
    Env.add_decl (id,ty)

let mk_definition (((l,id),pty,pte):lvar*pterm*pterm) : unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[Definition] " ^ id ^ ".\n") ;
    check_type ty ;
    check_term te ty ;
    Env.add_def (id,te,ty) 

let mk_opaque (((l,id),pty,pte):lvar*pterm*pterm) : unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[Opaque] " ^ id ^ ".\n") ;
    check_type ty ;
    check_term te ty ;
    Env.add_decl (id,ty) 

let mk_typecheck ((l,pty,pte):loc*pterm*pterm) :unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[TypeCheck] _ \n") ;
    check_type ty ;
    check_term te ty

let mk_rules (lst:rule list) : unit = assert false 

let mk_ending _ : unit = () 
 
(* End of File *)
