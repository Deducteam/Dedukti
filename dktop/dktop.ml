open Term
open Rule

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

module M =
struct

  let mk_prelude _ _ = assert false

  let mk_declaration lc id pty =
    let ty = Inference.is_a_type2 pty in
      Env.add_decl lc id ty ;
      print "%s is declared." (string_of_ident id)

  let mk_definition lc id opt pte =
    let (te,ty) =
      match opt with
        | None          -> Inference.infer2 pte
        | Some pty      -> Inference.check2 pte pty
    in
      Env.add_def lc id te ty ;
      print "%s is defined." (string_of_ident id)

  let mk_opaque lc id ty_opt pte =
    let (_,ty) =
      match ty_opt with
        | None          -> Inference.infer2 pte
        | Some pty      -> Inference.check2 pte pty
    in
      Env.add_decl lc id ty ;
      print "%s is defined." (string_of_ident id)

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
    | Tokens.EndOfFile -> exit 0

and error lb err = print err ; parse lb

let  _ =
  print "Welcome in Dedukti";
  let v = hstring "toplevel" in
    Env.init v ;
    parse (Lexing.from_channel stdin)
