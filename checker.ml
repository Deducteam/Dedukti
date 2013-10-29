
open Types

let rec get_loc = function
  | PType l | PId (l,_) | PQid (l,_,_) 
  | PLam ((l,_),_,_) | PPi  (Some (l,_),_,_) -> l
  | PPi  (None,t,_) | PApp (t,_) -> get_loc t

(* Checks that |- te:ty *)
let check_term lc (te:term) (ty:term) : unit = 
  let ty' = Inference.infer lc [] te in
    if not (Reduction.are_convertible ty ty') then 
      raise (TypingError (lc,Error.err_conv2 te ty ty' (Reduction.hnf ty) (Reduction.hnf ty')))

(* Checks that |- ty : Type or |- ty : Kind *)
let check_type lc (ctx:term list) (ty:term) : unit = 
  match Inference.infer lc ctx ty with
    | Kind | Type       -> ()
    | s                 -> raise (TypingError (lc,Error.err_sort ty s)) 

(* Checks a declaration in typing context *)
let check_env (k,names,types:int*ident list*term list) ((_,na),pty) =
  let ty = Pterm.of_pterm names pty in
    check_type (get_loc pty) types ty ; 
    (k+1,na::names,ty::types)

(* Checks that a rule is well-typed *)                             
let check_rule (penv,ple,pri) : rule = 
  let (k,names,ctx) = List.fold_left check_env (0,[],[]) penv   in
  let (cst,args)    = Pterm.top_of_ptop names ple               in
  let (ty_le0,lst)  = Inference.infer_pattern ctx (Pattern (cst,args)) in 
    match Unification.resolve_constraints ty_le0 lst with
      | None            -> raise (PatternError ( fst (fst ple) , Error.err_rule (cst,args) ))
      | Some ty_le      ->
          let ri            = Pterm.of_pterm names pri                  in
          let ty_ri         = Inference.infer (get_loc pri) ctx ri                    in
            if Reduction.are_convertible ty_le ty_ri then 
              ( k , (cst,args) , ri ) 
            else
              raise (TypingError (get_loc pri,Error.err_conv2 ri ty_le ty_ri (Reduction.hnf ty_le) (Reduction.hnf ty_ri))) 


let add_decl lc id pty = 
  let ty = Pterm.of_pterm [] pty in
    check_type (get_loc pty) [] ty ;
    Env.add_decl lc id ty

let add_def lc id pty_opt pte = 
  let te = Pterm.of_pterm [] pte in
  let ty =
    match pty_opt with
      | None    -> Inference.infer (get_loc pte) [] te
      | Some pty -> 
          ( let ty = Pterm.of_pterm [] pty in
              check_type (get_loc pty) [] ty ;
              check_term (get_loc pte) te ty ;
              ty )
  in
    Env.add_def lc id te ty 

let add_opaque lc id pty_opt pte = 
  let te = Pterm.of_pterm [] pte in
  let ty =
    match pty_opt with
      | None    -> Inference.infer (get_loc pte) [] te
      | Some pty -> 
          ( let ty = Pterm.of_pterm [] pty in
              check_type (get_loc pty) [] ty ;
              check_term (get_loc pte) te ty ;
              ty )
  in
    Env.add_decl lc id ty 

let add_rules lc hd (lst:prule list) : unit = 
  let rs = List.map check_rule lst in
    Env.add_rw lc hd rs
