open Term
open Rule
open Env

(* ********************************* *)

let verbose = ref false

let set_debug_level lvl =
  if lvl > 0 then ( verbose := true; Pp.print_db := true )

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c;
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  ) else
    Printf.ifprintf stderr fmt

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Env.init name

let mk_declaration lc id pty =
  eprint lc "Declaration of symbol '%a'." pp_ident id;
  SafeEnv.add_decl lc id pty

let mk_definition lc id pty_opt pte =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  SafeEnv.add_def lc id pte pty_opt

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  SafeEnv.add_opaque lc id pte pty_opt

let mk_rule (pr:prule) : rule =
  let (lc,_,id,_,_) = pr in
    eprint lc "Rewrite rule for symbol '%a'." pp_ident id ;
    Inference.check_prule pr

let mk_rules : prule list -> unit = function [] -> ()
  | (lc,_,id,_,_)::_ as plst ->
      ( eprint lc "Rewrite rule for symbol '%a'." pp_ident id ;
        SafeEnv.add_rules plst )

let mk_command lc = function
  | Whnf pte          ->
      let (te,_) = Inference.infer2 pte in
        print "%a" Pp.pp_term (Reduction.whnf te)
  | Hnf pte           ->
      let (te,_) = Inference.infer2 pte in
        print "%a" Pp.pp_term (Reduction.hnf te)
  | Snf pte           ->
      let (te,_) = Inference.infer2 pte in
        print "%a" Pp.pp_term (Reduction.snf te)
  | OneStep pte       ->
      let (te,_) = Inference.infer2 pte in
        ( match Reduction.one_step te with
            | None    -> print "Already in weak head normal form."
            | Some t' -> Pp.pp_term stdout t')
  | Conv (pte1,pte2)  ->
      let (t1,_) = Inference.infer2 pte1 in
      let (t2,_) = Inference.infer2 pte2 in
        if Reduction.are_convertible t1 t2 then print "OK"
        else print "KO"
  | Check (pte,pty) ->
      let (ty1,_)   = Inference.infer2 pty in
      let (_,ty2) = Inference.infer2 pte in
        if Reduction.are_convertible ty1 ty2 then print "OK"
        else print "KO"
  | Infer pte         ->
      let (ty,te) = Inference.infer2 pte in Pp.pp_term stdout ty
  | Gdt (m0,v)        ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
      ( match Env.get_infos lc m v with
          | Decl_rw (_,_,i,g)   -> ( Pp.pp_rw stdout (m,v,i,g) ; print_newline () )
          | _                   -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> eprint lc "Unknown command '%s'." cmd

let export = ref false

let mk_ending _ =
  ( if !export then Env.export () ); Env.clear ()
