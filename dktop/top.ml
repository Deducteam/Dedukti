open Term
open Rule

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let mk_prelude _ _ = failwith "Top.prelude should not be used."

let mk_declaration lc id pty =
  SafeEnv.add_decl lc id pty;
  print "%s is declared." (string_of_ident id)

let mk_definition lc id pty_opt pte =
  SafeEnv.add_def lc id pte pty_opt;
  print "%s is defined." (string_of_ident id)

let mk_opaque lc id pty_opt pte =
  SafeEnv.add_opaque lc id pte pty_opt;
  print "%s is declared." (string_of_ident id)

let mk_rules (prs:prule list) =
  let rs = List.map Inference.check_prule prs in
    Env.add_rw rs ;
    List.iter(fun r -> print "%a" Pp.pp_rule r) rs

let mk_command lc = function
  | Whnf pte          ->
      let (te,_) = Inference.infer2 pte in
        Pp.pp_term stdout (Reduction.whnf te)
  | Hnf pte           ->
      let (te,_) = Inference.infer2 pte in
        Pp.pp_term stdout (Reduction.hnf te)
  | Snf pte           ->
      let (te,_) = Inference.infer2 pte in
        Pp.pp_term stdout (Reduction.snf te)
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
      let (ty1,_) = Inference.infer2 pty in
      let (_,ty2) = Inference.infer2 pte in
        if Reduction.are_convertible ty1 ty2 then print "OK"
        else print "KO"
  | Infer pte         ->
      let (te,ty) = Inference.infer2 pte in Pp.pp_term stdout ty
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_infos lc m v with
            | Env.Decl_rw (_,_,i,g) -> Pp.pp_rw stdout (m,v,i,g)
            | _                 -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> print "Unknown command '%s'." cmd

let mk_ending _ = ()
