
open Types

module M =
struct

  let mk_prelude _ _ = assert false

  let mk_declaration lc id ty =
    let ty' = Inference.check_type [] ty in
      Env.add_decl lc id ty' ;
      Global.print "%s is declared." (string_of_ident id)

  let mk_definition lc id ty_opt pte =
    let (te,ty) =
      match ty_opt with
        | None          -> Inference.infer [] pte
        | Some pty      ->
            let ty = Inference.check_type [] pty in
              ( Inference.check_term [] pte ty , ty )
    in
      Env.add_def lc id te ty ;
      Global.print "%s is defined." (string_of_ident id)

  let mk_opaque lc id ty_opt pte =
    let (te,ty) =
      match ty_opt with
        | None          -> Inference.infer [] pte
        | Some pty      ->
            let ty = Inference.check_type [] pty in
              ( Inference.check_term [] pte ty , ty )
    in
      Env.add_static lc id ty ;
      Global.print "%s is defined." (string_of_ident id)

  let mk_static lc id pty =
    let ty = Inference.check_type [] pty in
      Env.add_static lc id ty ;
      Global.print "%s is defined." (string_of_ident id)

  let mk_rules (prs:prule list) =
    let (lc,hd) =
      match prs with
        | (_,(l,id,_),_)::_       -> (l,id)
        | _                       -> assert false
    in
    let rs = List.map Rule.check_rule prs in
      Env.add_rw lc hd rs ;
      Global.print "Rules added."

  let mk_command lc = function
    | Whnf pte          -> 
        let (te,_) = Inference.infer [] pte in Pp.pp_term stdout (Reduction.whnf te)
    | Hnf pte           -> 
        let (te,_) = Inference.infer [] pte in Pp.pp_term stdout (Reduction.hnf te)
    | Snf pte           -> 
        let (te,_) = Inference.infer [] pte in Pp.pp_term stdout (Reduction.snf te)
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
        ( match Env.get_global_rw lc m v with
            | Some (i,g,_) -> Pp.pp_gdt stdout (m,v,i,g)
            | _            -> Global.print "No GDT." )
    | Print str         -> pp_ident stdout str
    | Other (cmd,_)     -> Global.debug 1 lc "Unknown command '%s'." cmd


  let mk_ending _ = ()

end

module P = Parser.Make(M)

let rec parse lb =
  try
    while true do
      Global.print ">> ";
      P.line Lexer.token lb
    done
  with
    | Exit      -> parse lb
    | P.Error   ->
        Printf.eprintf "Unexpected token '%s'.\n" (Lexing.lexeme lb) ; parse lb
    | EndOfFile -> exit 0

and error lb err = Global.print err ; parse lb

let  _ =
  Global.print
    "=============================================================================
    \\ \\    / /__| |__ ___ _ __  ___  | |_ ___  |   \\ ___ __| |_  _  _  _ | |_ _
                                                                                 \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_) |  _/ _ \\ | |) / -_) _` | || || |/ /|  _(_)
\\_/\\_/\\___|_\\__\\___/_|_|_\\___|  \\__\\___/ |___/\\___\\__,_|\\_,_||_|\\_\\ \\__|_|
=============================================================================
  "  ;
  let v = hstring "toplevel" in
    Global.name := v ;
    Env.init v ;
    parse (Lexing.from_channel stdin)
