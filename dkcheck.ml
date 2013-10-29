
open Types

module M =
struct

  let mk_prelude (l,v : loc*ident) : unit =
    Global.vprint (lazy (string_of_loc l ^ "[Name] " ^ string_of_ident v ^ ".\n")) ;
    Global.name := v ;
    Env.init v

  let mk_require (l,v : loc*ident) : unit = 
    Global.vprint (lazy (string_of_loc l ^ "[Import] " ^ string_of_ident v ^ " (This is obsolete !).\n")) ;
    Env.import l v

  let mk_declaration ((l,id),pty : (loc*ident)*pterm) : unit = 
    Global.vprint (lazy (string_of_loc l ^ "[Declaration] " ^ string_of_ident id ^ ".\n" )) ;
    Checker.add_decl ((l,id),pty)

  let mk_definition ((l,id),pty,pte : (loc*ident)*pterm*pterm) : unit = 
    Global.vprint (lazy (string_of_loc l ^ "[Definition] " ^ string_of_ident id ^ ".\n")) ;
    Checker.add_def ((l,id),pty,pte)

  let mk_infered_def ((l,id),pte : (loc*ident)*pterm) : unit =
    Global.vprint (lazy (string_of_loc l ^ "[Infered Definition] " ^ string_of_ident id ^ ".\n")) ;
    Checker.add_idef ((l,id),pte)

  let mk_opaque ((l,id),pty,pte : (loc*ident)*pterm*pterm) : unit = 
    Global.vprint (lazy (string_of_loc l ^ "[Opaque] " ^ string_of_ident id ^ ".\n")) ;
    Checker.add_odef ((l,id),pty,pte)

  let mk_typecheck (l,pty,pte : loc*pterm*pterm) :unit = 
    Global.vprint (lazy (string_of_loc l ^ "[TypeCheck] _ \n")) ;
    Checker.typecheck (l,pty,pte)

  let mk_normalize (pte : pterm) : unit = 
    Global.vprint (lazy (string_of_loc (get_loc pte) ^ "[Normalize] ...\n" )) ; 
    let te = Pterm.of_pterm [] pte in
    let te' = Reduction.hnf te in
      Global.sprint ( Error.string_of_term te' ^ "\n" )

  let mk_rules (lst:prule list) : unit = 
    let aux = function
    | (_,((l,v),_),_)::_  -> (l,v)
    | _                     -> assert false
    in
    let (l,v) = aux lst in
      Global.vprint (lazy (string_of_loc l ^ "[Rule] " ^ string_of_ident v ^ ".\n")) ;
      Checker.add_rules l v lst 

  let mk_ending _ : unit = () 

end

(* *** Parsing *** *)

module P = Parser.Make(M)

let parse lb = 
  try
      P.top Lexer.token lb
  with 
    | P.Error       -> 
        begin
          let curr = lb.Lexing.lex_curr_p in
          let l = curr.Lexing.pos_lnum in
          let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (ParserError ( mk_loc l c , "Unexpected token '" ^ tok ^ "'." ) ) 
        end

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
