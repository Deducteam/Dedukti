
open Types

(* Checks that |- te:ty *)
let check_term (te:term) (ty:term) : unit = 
  let ty' = Inference.infer [] te in
    if not (Reduction.are_convertible ty ty') then 
      raise (TypingError (Error.err_conv te ty ty'))

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type (ctx:term list) (ty:term) : unit = 
  match Inference.infer ctx ty with
    | Kind | Type       -> ()
    | s                 -> raise (TypingError (Error.err_sort ty s)) 

(* Checks a declaration in typing context *)
let check_env (k,names,types:int*string list*term list) ((_,na),pty) =
  let ty = Pterm.of_pterm names pty in
    check_type types ty ; 
    (k+1,na::names,ty::types)

(* Checks a rule: 
 * Not a variable (checked by parsing)
 * FV(r) C FV(l) (checked in matching)
 * All the rules have the same arity FIXME not done ?
 * All the rules have the head symbol FIXME not done ?
 * Rules are well-typed
* *)                             
let check_rule (penv,ple,pri) : rule = 
  let (k,names,ctx) = List.fold_left check_env (0,[],[]) penv   in
  let (cst,args)    = Pterm.top_of_ptop names ple               in
  let (ty_le0,lst)  = Inference.infer_pattern ctx (Pattern (cst,args)) in 
  let ty_le         = Unification.resolve_constraints ty_le0 lst in
  let ri            = Pterm.of_pterm names pri                  in
  let ty_ri         = Inference.infer ctx ri                    in
    if Reduction.are_convertible ty_le ty_ri then 
      { li=args; te=ri; na=Array.init k (fun i -> i)} 
    else
      raise (TypingError (Error.err_conv ri ty_le ty_ri)) 


let mk_prelude (l,v : loc*string) : unit =
        Global.print_v ( Error.string_of_loc l ^ "[Name] " ^ v ^ ".\n") ;
        Global.set_name v ;
        Env.init v

let mk_require (l,v : loc*string) : unit = 
        Global.print_v ( Error.string_of_loc l ^ "[Import] " ^ v ^ " (This is obsolete !).\n") ;
        Env.import l v

let mk_declaration ((l,id),pty : (loc*string)*pterm) : unit = 
        let ty = Pterm.of_pterm [] pty in
        Global.print_v ( Error.string_of_loc l ^ "[Declaration] " ^ id ^ ".\n" ) ;
        check_type [] ty ;
        Env.add_decl l id ty

let mk_definition ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
        let ty = Pterm.of_pterm [] pty in
        let te = Pterm.of_pterm [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[Definition] " ^ id ^ ".\n") ;
        check_type [] ty ;
        check_term te ty ;
        Env.add_def l id te ty 

let mk_infered_def ((l,id),pte : (loc*string)*pterm) : unit =
        let te = Pterm.of_pterm [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[Infered Definition] " ^ id ^ ".\n") ;
        let ty = Inference.infer [] te in
        Env.add_def l id te ty 

let mk_opaque ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
        let ty = Pterm.of_pterm [] pty in
        let te = Pterm.of_pterm [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[Opaque] " ^ id ^ ".\n") ;
        check_type [] ty ;
        check_term te ty ;
        Env.add_decl l id ty 

let mk_typecheck (l,pty,pte : loc*pterm*pterm) :unit = 
        let ty = Pterm.of_pterm [] pty in
        let te = Pterm.of_pterm [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[TypeCheck] _ \n") ;
        check_type [] ty ;
        check_term te ty

let mk_normalize (pte : pterm) : unit = 
        let te = Pterm.of_pterm [] pte in
        Global.print_v ( "[Normalize] ...\n" ) ;
        let te' = Reduction.hnf te in
        Global.print_v ( Error.string_of_term te' ^ "\n" )

let mk_rules (lst:prule list) : unit = 
        let aux = function
                | (_,((l,v),_),_)::_  -> (l,v)
                | _                     -> assert false
        in
        let (l,v) = aux lst in
        Global.print_v (Error.string_of_loc l ^ "[Rewrite] " ^ v ^ ".\n") ; 
        let rs = List.map check_rule lst in
        let gdt = Matching.get_rw (Array.of_list rs) in
        Env.add_rw l v gdt

let mk_ending _ : unit = () 
