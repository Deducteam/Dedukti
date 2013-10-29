
open Types

module M =
struct

  let mk_prelude lc name =
    Global.vprint (lazy (string_of_loc lc ^ "[Name] " ^ string_of_ident name ^ ".\n")) ;
    Global.name := name ;
    Env.init name

  let mk_require lc m = 
    Global.vprint (lazy (string_of_loc lc ^ "[Import] " ^ string_of_ident m ^ " (Obsolete).\n")) ;
    Env.import lc m

  let mk_declaration lc id pty = 
    Global.vprint (lazy (string_of_loc lc ^ "[Declaration] " ^ string_of_ident id ^ ".\n" )) ;
    Checker.add_decl lc id pty

  let mk_definition lc id pty pte = 
    Global.vprint (lazy (string_of_loc lc ^ "[Definition] " ^ string_of_ident id ^ ".\n")) ;
    Checker.add_def lc id pty pte

  let mk_opaque lc id pty pte = 
    Global.vprint (lazy (string_of_loc lc ^ "[Opaque] " ^ string_of_ident id ^ ".\n")) ;
    Checker.add_opaque lc id pty pte

  let mk_term pte = 
    Global.vprint (lazy (string_of_loc (get_loc pte) ^ "[Term] ...\n" )) ; 
    let te = Pterm.of_pterm [] pte in
    let te' = Reduction.hnf te in
      Global.sprint ( Error.string_of_term te' ^ "\n" )

  let mk_rules (lst:prule list) = 
    let aux = function
    | (_,((l,v),_),_)::_  -> (l,v)
    | _                     -> assert false
    in
    let (lc,hd) = aux lst in
      Global.vprint (lazy (string_of_loc lc ^ "[Rule] " ^ string_of_ident hd ^ ".\n")) ;
      Checker.add_rules lc hd lst 

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
          let curr = lb.Lexing.lex_curr_p in
          let l = curr.Lexing.pos_lnum in
          let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (ParserError ( mk_loc l c , "Unexpected token '" ^ tok ^ "'." ) ) 
        end
    | EndOfFile -> ()

(* *** Input *** *)

let run_on_stdin _ =
  Global.eprint (" -- Processing standard input ...\t") ;
  Global.vprint (lazy "\n");
  parse (Lexing.from_channel stdin) ;
  Global.eprint ("\027[32m[DONE]\027[m\n") ;
  Env.export_and_clear ()
            
let run_on_file file =
  let input = open_in file in
    Global.eprint (" -- Processing file '" ^ file ^ "' ...\t") ;
    Global.vprint (lazy "\n");
    parse (Lexing.from_channel input) ;
    Global.eprint ("\027[32m[DONE]\027[m\n") ;
    Env.export_and_clear ()

(* *** Arguments *** *)

let args = [
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"               ) ;
        ("-v"    , Arg.Clear Global.quiet               , "Verbose"             ) ;
        ("-e"    , Arg.Set Global.export                , "Create a .dko"       ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) ; 
        ("-r"    , Arg.Set Global.raphael               , "Undocumented"  ) 
]

(* *** Main *** *)

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkcheck [options] files"  
  with 
    | Sys_error err             -> Global.error dloc "System Error"  err
    | LexerError (lc,err)       -> Global.error lc "Lexing Error"  err
    | ParserError (lc,err)      -> Global.error lc "Parsing Error"  err
    | TypingError (lc,err)      -> Global.error lc "Typing Error"  err
    | EnvError (lc,err)         -> Global.error lc "Scoping Error" err 
    | PatternError (lc,err)     -> Global.error lc "Rule Error" err 
