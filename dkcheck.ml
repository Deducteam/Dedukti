
open Types

module M =
struct

  let mk_prelude lc name =
    Global.vprint lc (lazy ("Module name is '" ^ string_of_ident name ^ "'."))

  let mk_require lc m =
    Global.warning lc "Import command is ignored."

  let mk_declaration lc id pty =
    Global.vprint lc (lazy ("Declaration of symbol '" ^ string_of_ident id ^ "'." )) ;
    let ty = Inference.check_type [] pty in
      Env.add_decl lc id ty

  let mk_definition lc id ty_opt pte =
    Global.vprint lc (lazy ("Definition of symbol '" ^ string_of_ident id ^ "'.")) ;
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
    Global.vprint lc (lazy ("Opaque definition of symbol '" ^ string_of_ident id ^ "'.")) ;
    let ty =
      match ty_opt with
        | None          -> snd ( Inference.infer [] pte )
        | Some pty      ->
            let ty = Inference.check_type [] pty in
            let _  = Inference.check_term [] pte ty in
              ty
    in
      Env.add_decl lc id ty

  let mk_term pte =
    Global.vprint (get_loc pte) (lazy ("Term normalization:" )) ;
    let (te,_) = Inference.infer [] pte in
    let te' = Reduction.hnf te in
      Global.sprint ( Pp.string_of_term te' ) ;
      match (
        match pte with
          | P_Id (l,id)         -> ( !Global.name , id , Env.get_global_rw l !Global.name id )
          | P_QId (l,md,id)     -> ( md ,id , Env.get_global_rw l md id )
          | _                   -> ( empty , empty , None )
      ) with
        | ( _, _ , None )               -> ()
        | ( md, id , Some (i,g) )       -> Global.vprint (get_loc pte) (lazy (Pp.string_of_gdt md id i g))

  let mk_rules (prs:prule list) =
    let (lc,hd) =
      match prs with
      | (_,P_Id(l,id),_)::_
      | (_,P_App((P_Id (l,id))::_),_)::_        -> (l,id)
      | _                                       -> assert false
    in
      Global.vprint lc (lazy ("Rewrite rules for symbol '" ^ string_of_ident hd ^ "'.")) ;
      let rs = List.map Inference.check_rule prs in
        Env.add_rw lc hd rs

  let mk_assert lc pt1 pt2 = 
    Global.vprint lc (lazy ("Checking assertion.")) ;
    let (t1,_) = Inference.infer [] pt1 in
    let (t2,_) = Inference.infer [] pt2 in
      if not (Reduction.are_convertible t1 t2) then
        failwith "Assertion failed: terms are not convertible." (*FIXME*)

  let mk_ending _ =
    Env.export_and_clear ()

end

(* *** Parsing *** *)

module P = Parser.Make(M)

let parse lb =
  try
      P.prelude Lexer.token lb ;
      while true do P.line Lexer.token lb done
  with
    | P.Error   ->
        begin
          let start = lb.Lexing.lex_start_p in
          let line = start.Lexing.pos_lnum  in
          let cnum = start.Lexing.pos_cnum - start.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (ParserError ( mk_loc line cnum , "Unexpected token '" ^ tok ^ "'." ) )
        end
    | EndOfFile -> ()

(* *** Input *** *)

let run_on_stdin _ =
  Global.vprint dloc (lazy " -- Processing standard input ...") ;
  parse (Lexing.from_channel stdin) ;
  Global.print_ok "none"

let run_on_file file =
  let input = open_in file in
    Global.vprint dloc (lazy (" -- Processing file '" ^ file ^ "' ...")) ;
    Global.set_filename file ;
    parse (Lexing.from_channel input) ;
    Global.print_ok file ;
    Global.print_stats () 

(* *** Main *** *)

let args = [
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"                       ) ;
        ("-v"    , Arg.Clear Global.quiet               , "Verbose"                     ) ;
        ("-e"    , Arg.Set Global.export                , "Create a .dko"               ) ;
        ("-nc"   , Arg.Clear Global.color               , "Disable colored output"      ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"          ) ;
        ("-r"    , Arg.Set Global.raphael               , "Undocumented"                )
]

let _ =
  try
    Arg.parse args run_on_file ("Usage: "^Sys.argv.(0)^" [options] files")
  with
    | Sys_error err             -> Global.error dloc err
    | LexerError (lc,err)       -> Global.error lc err
    | ParserError (lc,err)      -> Global.error lc err
    | TypingError (lc,err)      -> Global.error lc err
    | EnvError (lc,err)         -> Global.error lc err
    | PatternError (lc,err)     -> Global.error lc err
