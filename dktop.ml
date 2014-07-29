open Types

module M =
struct

  let mk_prelude _ _ = assert false

  let mk_declaration lc id pty =
    let ty = Inference.is_a_type pty in
      Env.add_decl lc id ty ;
      Global.print "%s is declared." (string_of_ident id)

  let mk_definition lc id opt pte =
    let (te,ty) =
      match opt with
        | None          -> Inference.infer pte
        | Some pty      -> Inference.check pte pty
    in
      Env.add_def lc id te ty ;
      Global.print "%s is defined." (string_of_ident id)

  let mk_opaque lc id ty_opt pte =
    let (_,ty) =
      match ty_opt with
        | None          -> Inference.infer pte
        | Some pty      -> Inference.check pte pty
    in
      Env.add_decl lc id ty ;
      Global.print "%s is defined." (string_of_ident id)

  let mk_rules (prs:prule list) =
    let rs = List.map Inference.check_rule prs in
      Env.add_rw rs ;
      List.iter(fun r -> Global.print "%a" Pp.pp_rule r) rs

  let mk_command lc = function
    | Whnf pte          ->
        let (te,_) = Inference.infer pte in
          Pp.pp_term stdout (Reduction.whnf te)
    | Hnf pte           ->
        let (te,_) = Inference.infer pte in
          Pp.pp_term stdout (Reduction.hnf te)
    | Snf pte           ->
        let (te,_) = Inference.infer pte in
          Pp.pp_term stdout (Reduction.snf te)
    | OneStep pte       ->
        let (te,_) = Inference.infer pte in
          ( match Reduction.one_step te with
              | None    -> Global.print "Already in weak head normal form."
              | Some t' -> Pp.pp_term stdout t')
    | Conv (pte1,pte2)  ->
        let (t1,_) = Inference.infer pte1 in
        let (t2,_) = Inference.infer pte2 in
          if Reduction.are_convertible t1 t2 then Global.print "OK"
          else Global.print "KO"
    | Check (pte,pty) ->
        let (ty1,_) = Inference.infer pty in
        let (_,ty2) = Inference.infer pte in
          if Reduction.are_convertible ty1 ty2 then Global.print "OK"
          else Global.print "KO"
    | Infer pte         ->
        let (te,ty) = Inference.infer pte in Pp.pp_term stdout ty
    | Gdt (m,v)         ->
        ( match Env.get_infos lc m v with
            | Decl_rw (_,_,i,g) -> Pp.pp_rw stdout (m,v,i,g)
            | _                 -> Global.print "No GDT." )
    | Print str         -> pp_ident stdout str
    | Other (cmd,_)     -> Global.debug 1 lc "Unknown command '%s'." cmd

  let mk_ending _ = ()

end

module P = Parser.Make(M)

let rec parse lb =
  try
    while true do
      print_string ">> ";
      flush stdout;
      P.line Lexer.token lb
    done
  with
    | Exit      -> parse lb
    | P.Error   ->
        Printf.eprintf "Unexpected token '%s'.\n" (Lexing.lexeme lb) ; parse lb
    | EndOfFile -> exit 0

and error lb err = Global.print err ; parse lb

let  _ =
  Global.print "Welcome in Dedukti";
  let v = hstring "toplevel" in
    Global.name := v ;
    Env.init v ;
    parse (Lexing.from_channel stdin)
