
open Types


(* Checks that |- te:ty *)
let check_term te ty = 
  let ty' = Inference.infer 0 [] te in
    if not (Reduction.are_convertible ty ty') then 
      raise (TypingError (Error.err_conv te ty ty'))

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type k ctx ty = 
  match Inference.infer k ctx ty with
    | Kind | Type       -> ()
    | s                 -> raise (TypingError (Error.err_sort ty s)) 

(* Check a rule: the context is well formed and the left and right sides of the
* rule have convertible types *)                             
let check_rule (penv,ple,pri) : rule2 = 
  let (k,names,ctx:int*string list*term list) = 
    List.fold_left (
      fun (k,names,ctx) ((_,v),pty) ->
        let ty = Pterm.of_pterm k names pty in
          check_type k ctx ty ; (k+1,v::names,ty::ctx)
    ) (0,[],[]) penv in
  let le  = Pterm.term_of_tpat k names ple in
  let ri  = Pterm.of_pterm     k names pri in
  let ty_le = Inference.infer k ctx le in
  let ty_ri = Inference.infer k ctx ri in
    if Reduction.are_convertible ty_le ty_ri then (names,ple,ri) 
    else
      raise (TypingError (Error.err_conv ri ty_le ty_ri))


(* ***** Entry points ***** *)

let mk_prelude (l,v : loc*string) : unit =
        Global.print_v ( Error.string_of_loc l ^ "[Name] " ^ v ^ ".\n") ;
        Global.set_name v ;
        Env.init v

let mk_require (l,v : loc*string) : unit = 
        Global.print_v ( Error.string_of_loc l ^ "[Import] " ^ v ^ ".\n") ;
        Env.import v

let mk_declaration ((l,id),pty : (loc*string)*pterm) : unit = 
        let ty = Pterm.of_pterm 0 [] pty in
        Global.print_v ( Error.string_of_loc l ^ "[Declaration] " ^ id ^ ".\n" ) ;
        check_type 0 [] ty ;
        Env.add_decl id ty

let mk_definition ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
        let ty = Pterm.of_pterm 0 [] pty in
        let te = Pterm.of_pterm 0 [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[Definition] " ^ id ^ ".\n") ;
        check_type 0 [] ty ;
        check_term te ty ;
        Env.add_def id te ty 

let mk_infered_def ((l,id),pte : (loc*string)*pterm) : unit =
        let te = Pterm.of_pterm 0 [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[Infered Definition] " ^ id ^ ".\n") ;
        let ty = Inference.infer 0 [] te in
        Env.add_def id te ty 

let mk_opaque ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
        let ty = Pterm.of_pterm 0 [] pty in
        let te = Pterm.of_pterm 0 [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[Opaque] " ^ id ^ ".\n") ;
        check_type 0 [] ty ;
        check_term te ty ;
        Env.add_decl id ty 

let mk_typecheck (l,pty,pte : loc*pterm*pterm) :unit = 
        let ty = Pterm.of_pterm 0 [] pty in
        let te = Pterm.of_pterm 0 [] pte in
        Global.print_v ( Error.string_of_loc l ^ "[TypeCheck] _ \n") ;
        check_type 0 [] ty ;
        check_term te ty

let mk_normalize (pte : pterm) : unit = 
        let te = Pterm.of_pterm 0 [] pte in
        Global.print_v ( "[Normalize] ...\n" ) ;
        let te' = Reduction.wnf te in
        Global.print_v ( Error.string_of_term te' ^ "\n" )

let mk_rules (lst:rule list) : unit = 
        let aux = function
                | (_,((l,v),_,_),_)::_  -> (l,v)
                | _                     -> assert false
        in
        let (l,v) = aux lst in
        Global.print_v (Error.string_of_loc l ^ "[Rewrite] " ^ v ^ ".\n") ; 
        let rs = List.map (check_rule ) lst in
        let gdt = Matching.get_rw v rs in
        Env.add_rw v gdt

let mk_ending _ : unit = () 

