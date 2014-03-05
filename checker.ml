open Types

let mk_prelude lc name =
  Global.debug 1 lc "Module name is '%s'." (string_of_ident name)

let mk_declaration lc id pty =
  Global.debug 1 lc "Declaration of symbol '%s'." (string_of_ident id);
  let ty = Inference.check_type [] pty in
    Env.add_decl lc id ty

let mk_definition lc id ty_opt pte =
  Global.debug 1 lc "Definition of symbol '%s'." (string_of_ident id) ;
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
  Global.debug 1 lc "Opaque definition of symbol '%s'." (string_of_ident id) ;
  let ty =
    match ty_opt with
      | None          -> snd ( Inference.infer [] pte )
      | Some pty      ->
          let ty = Inference.check_type [] pty in
          let _  = Inference.check_term [] pte ty in
            ty
  in
    Env.add_static lc id ty

let mk_static lc id pty =
  Global.debug 1 lc "Static declaration of symbol '%s'." (string_of_ident id) ;
  let ty = Inference.check_type [] pty in
    Env.add_static lc id ty

let mk_rules prs = 
  let (lc,hd) = match prs with | [] -> assert false
    | (_,(l,id,_),_)::_ -> (l,id)
  in
    Global.debug 1 lc "Rewrite rules for symbol '%s'." (string_of_ident hd) ;
    let rs = List.map Rule.check_rule prs in
      Env.add_rw lc hd rs ;
      List.iter (fun r -> Global.debug_no_loc 1  "%s" (Pp.string_of_rule r)) rs

let mk_command lc = function
  | Whnf pte          ->
      let (te,_) = Inference.infer [] pte in
        Global.print "%s" (Pp.string_of_term (Reduction.whnf te))
  | Hnf pte           ->
      let (te,_) = Inference.infer [] pte in
        Global.print "%s" (Pp.string_of_term (Reduction.hnf te))
  | Snf pte           ->
      let (te,_) = Inference.infer [] pte in
        Global.print "%s" (Pp.string_of_term (Reduction.snf te))
  | OneStep pte       ->
      let (te,_) = Inference.infer [] pte in
        ( match Reduction.one_step te with
            | None    -> Global.print "Already in weak head normal form."
            | Some t' -> Global.print "%s" (Pp.string_of_term t') )
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
      let (te,ty) = Inference.infer [] pte in
        Global.print "%s" (Pp.string_of_term ty)
  | Gdt (m,v)         ->
      ( match Env.get_global_rw lc m v with
          | Some (i,g,_) -> Global.print "%s" (Pp.string_of_gdt m v i g)
          | _            -> Global.print "No GDT." )
  | Print str         -> Global.print "%s" (string_of_ident str)
  | Other (cmd,_)     -> Global.debug 1 lc "Unknown command '%s'." cmd

let mk_ending _ =
  Env.export_and_clear ()
