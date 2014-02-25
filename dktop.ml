
open Types

module M =
struct

  let mk_prelude _ _ = assert false

  let mk_declaration lc id ty =
    let ty' = Inference.check_type [] ty in
      Env.add_decl lc id ty' ;
      Global.sprint (string_of_ident id ^ " is declared." )

  let mk_definition lc id ty_opt pte =
    let (te,ty) =
      match ty_opt with
        | None          -> Inference.infer [] pte
        | Some pty      ->
            let ty = Inference.check_type [] pty in
              ( Inference.check_term [] pte ty , ty )
    in
      Env.add_def lc id te ty ;
      Global.sprint (string_of_ident id ^ " is defined.")

  let mk_opaque lc id ty_opt pte =
    let (te,ty) =
      match ty_opt with
        | None          -> Inference.infer [] pte
        | Some pty      ->
            let ty = Inference.check_type [] pty in
             ( Inference.check_term [] pte ty , ty )
    in
      Env.add_static lc id ty ;
      Global.sprint (string_of_ident id ^ " is defined.")

let mk_static lc id pty =
  let ty = Inference.check_type [] pty in
    Env.add_static lc id ty ;
      Global.sprint (string_of_ident id ^ " is defined.")

  let mk_rules (prs:prule list) =
    let (lc,hd) =
      match prs with
      | (_,(l,id,_),_)::_       -> (l,id)
      | _                       -> assert false
    in
    let rs = List.map Rule.check_rule prs in
      Env.add_rw lc hd rs ;
      Global.sprint ("Rules added.")

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
          let (te,_) = Inference.infer [] pte in
            ( match Reduction.one_step te with
                | None    -> Global.sprint "Already in weak head normal form."
                | Some t' -> Global.sprint (Pp.string_of_term t') )
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
      | Other             -> Global.warning lc ("Unknown command '" ^ cmd ^ "'.")

  let mk_ending _ = ()

end

module P = Parser.Make(M)

let rec parse lb =
  try
      while true do
        Global.sprint ">> ";
        P.line Lexer.token lb
      done
  with
    | LexerError (_,err)  | ParserError (_,err)
    | TypingError (_,err) | EnvError (_,err)
    | PatternError (_,err)                      ->  error lb err
    | P.Error                                   ->
        error lb ("Unexpected token '" ^ (Lexing.lexeme lb) ^ "'." )
    | EndOfFile                                 -> exit 0

and error lb err = Global.sprint err ; parse lb

let ascii_art =
"=============================================================================
 \\ \\    / /__| |__ ___ _ __  ___  | |_ ___  |   \\ ___ __| |_  _  _  _ | |_ _
  \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_) |  _/ _ \\ | |) / -_) _` | || || |/ /|  _(_)
   \\_/\\_/\\___|_\\__\\___/_|_|_\\___|  \\__\\___/ |___/\\___\\__,_|\\_,_||_|\\_\\ \\__|_|
=============================================================================
"

let  _ =
  Global.sprint ascii_art ;
  let v = hstring "toplevel" in
    Global.name := v ;
    Env.init v ;
    parse (Lexing.from_channel stdin)
