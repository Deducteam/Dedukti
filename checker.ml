open Types

let mk_prelude lc name =
  Global.vprint lc (lazy ("Module name is '" ^ string_of_ident name ^ "'."))

let mk_declaration lc id pty =
  Global.vprint lc (lazy ("Declaration of symbol '"^string_of_ident id^"'." )) ;
  let ty = Inference.check_type [] pty in
    Env.add_decl lc id ty

let mk_definition lc id ty_opt pte =
  Global.vprint lc (lazy ("Definition of symbol '"^ string_of_ident id ^"'.")) ;
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
  Global.vprint lc (lazy ("Opaque definition of symbol '"
                            ^ string_of_ident id ^ "'."));
  let ty =
    match ty_opt with
      | None          -> snd ( Inference.infer [] pte )
      | Some pty      ->
          let ty = Inference.check_type [] pty in
          let _  = Inference.check_term [] pte ty in
            ty
  in
    Env.add_decl lc id ty

let mk_rules (prs:prule list) =
  let (lc,hd) = match prs with | [] -> assert false
    | (_,(l,id,_),_)::_ -> (l,id)
  in
    Global.vprint lc
      (lazy ("Rewrite rules for symbol '" ^ string_of_ident hd ^ "'.")) ;
    let rs = List.map Rule.check_rule prs in
      Env.add_rw lc hd rs ;
      List.iter ( fun r -> Global.vprint2 (lazy (Pp.string_of_rule r)) ) rs

let mk_command lc cmd lst =
  match Global.parse_cmd lc cmd lst with
    | Whnf pte          ->
        let (te,_) = Inference.infer [] pte in
          Global.sprint (Pp.string_of_term (Reduction.whnf te))
    | Hnf pte           ->
        let (te,_) = Inference.infer [] pte in
          Global.sprint (Pp.string_of_term (Reduction.hnf te))
    | Snf pte           ->
        let (te,_) = Inference.infer [] pte in
          Global.sprint (Pp.string_of_term (Reduction.snf te))
    | OneStep pte       ->
        assert false (*TODO merge with NormalisationPasAPas*)
    | Conv (pte1,pte2)  ->
        let (t1,_) = Inference.infer [] pte1 in
        let (t2,_) = Inference.infer [] pte2 in
          if Reduction.are_convertible t1 t2 then Global.sprint "OK"
          else Global.sprint "KO"
    | Check (pte1,pte2) ->
        let (t1,ty1) = Inference.infer [] pte1 in
        let ty2 = Inference.check_type [] pte2 in
          if Reduction.are_convertible ty1 ty2 then Global.sprint "OK"
          else Global.sprint "KO"
    | Infer pte         ->
        let (te,ty) = Inference.infer [] pte in
          Global.sprint (Pp.string_of_term ty)
    | Gdt (m,v)         ->
        ( match Env.get_global_rw lc m v with
            | Some (i,g,_)    -> Global.sprint (Pp.string_of_gdt m v i g)
            | _               -> Global.sprint "No GDT." )
    | Print str         -> Global.sprint str
    | Other             -> failwith ("Unknown command '" ^ cmd ^ "'.")

let mk_ending _ =
  Confluence.check_cpairs () ;
  Env.export_and_clear ()
