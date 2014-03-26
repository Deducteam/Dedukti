open Types

let mk_prelude lc name =
  Global.debug 1 lc "Module name is '%a'." pp_ident name

let mk_declaration lc id pty =
  Global.debug 1 lc "Declaration of symbol '%a'." pp_ident id;
  let ty = Inference.check_type [] pty in
    Env.add_decl lc id ty

let mk_definition lc id ty_opt pte =
  Global.debug 1 lc "Definition of symbol '%a'." pp_ident id ;
  let (te,ty) =
    match ty_opt with
      | None          -> Inference.infer [] pte
      | Some pty      ->
          let ty = Inference.check_type [] pty in
          let te = Inference.check_term [] pte ty in
            ( te , ty )
  in
    Env.add_def lc id te ty

let mk_opaque lc id ty_opt pte =
  Global.debug 1 lc "Opaque definition of symbol '%a'." pp_ident id ;
  let ty =
    match ty_opt with
      | None          -> snd ( Inference.infer [] pte )
      | Some pty      ->
          let ty = Inference.check_type [] pty in
          let _  = Inference.check_term [] pte ty in
            ty
  in
    Env.add_decl lc id ty

let mk_static lc id pty = failwith "Not implemented (Static declaration)."

let mk_rules prs =
  let (lc,hd) = match prs with | [] -> assert false
    | (_,(l,id,_),_)::_ -> (l,id)
  in
    Global.debug 1 lc "Rewrite rules for symbol '%a'." pp_ident hd ;
    let rs = List.map Inference.check_rule prs in
      List.iter (
        fun r -> Env.add_rw lc hd r ; Global.debug_no_loc 1 "%a" Pp.pp_rule r
      ) rs

let mk_command lc = function
  | Whnf pte          ->
      let (te,_) = Inference.infer [] pte in
        Pp.pp_term stdout (Reduction.whnf te)
  | Hnf pte           ->
      let (te,_) = Inference.infer [] pte in
        Pp.pp_term stdout (Reduction.hnf te)
  | Snf pte           ->
      let (te,_) = Inference.infer [] pte in
        Pp.pp_term stdout (Reduction.snf te)
  | OneStep pte       ->
      let (te,_) = Inference.infer [] pte in
        ( match Reduction.one_step te with
            | None    -> Global.print "Already in weak head normal form."
            | Some t' -> Pp.pp_term stdout t')
  | Conv (pte1,pte2)  ->
      let (t1,_) = Inference.infer [] pte1 in
      let (t2,_) = Inference.infer [] pte2 in
        if Reduction.are_convertible t1 t2 then Global.print "OK"
        else Global.print "KO"
  | Check (pte1,pte2) ->
      let (t1,ty1) = Inference.infer [] pte1 in
      let ty2 = Inference.check_type [] pte2 in
        if Reduction.are_convertible ty1 ty2 then Global.print "OK"
        else Global.print "KO"
  | Infer pte         ->
      let (te,ty) = Inference.infer [] pte in Pp.pp_term stdout ty
  | Gdt (m,v)         ->
      ( match Env.get_infos lc m v with
          | Decl_rw (_,i,g)     -> Pp.pp_rw stdout (m,v,i,g)
          | _                   -> Global.print "No GDT." )
  | Print str         -> pp_ident stdout str
  | Other (cmd,_)     -> Global.debug 1 lc "Unknown command '%s'." cmd

let mk_ending _ =
  Env.export_and_clear ()
