
open Types

module M =
struct

  let string_of_loc lc =
    "[line:" ^ string_of_int (get_line lc) ^ ";column:" ^ string_of_int (get_column lc) ^ "]"

  let mk_prelude lc name =
    Global.vprint (lazy (string_of_loc lc ^ "[Name] " ^ string_of_ident name ^ ".")) ;
    Global.name := name ;
    Env.init name

  let mk_require lc m = 
    Global.eprint "Warning: import (ignored)." ;
    Global.vprint (lazy (string_of_loc lc ^ "[Import] " ^ string_of_ident m ^ " (Obsolete)."))

  let mk_declaration lc id ty = 
    Global.vprint (lazy (string_of_loc lc ^ "[Declaration] " ^ string_of_ident id ^ "." )) ;
    Inference.check_type [] ty ;
    Env.add_decl lc id ty

  let mk_definition lc id ty_opt te = 
    Global.vprint (lazy (string_of_loc lc ^ "[Definition] " ^ string_of_ident id ^ ".")) ;
    let ty = 
      match ty_opt with
        | None          -> Inference.infer [] te
        | Some ty       -> ( Inference.check_type [] ty ; Inference.check_term [] te ty ; ty )
    in
      Env.add_def lc id te ty 

  let mk_opaque lc id ty_opt te = 
    Global.vprint (lazy (string_of_loc lc ^ "[Opaque] " ^ string_of_ident id ^ ".")) ;
    let ty = 
      match ty_opt with
        | None          -> Inference.infer [] te
        | Some ty       -> ( Inference.check_type [] ty ; Inference.check_term [] te ty ; ty )
    in
      Env.add_decl lc id ty 

  let mk_term te = 
    Global.vprint (lazy (string_of_loc (get_loc te) ^ "[Term] ..." )) ; 
    let te' = Reduction.hnf te in
      Global.sprint ( Error.string_of_term te' )

  let mk_rules (rs:rule list) = 
    let (lc,hd) = match rs with
      | (_,((l,v),_),_)::_      -> (l,v)
      | _                       -> assert false
    in
      Global.vprint (lazy (string_of_loc lc ^ "[Rule] " ^ string_of_ident hd ^ ".")) ;
      List.iter Inference.check_rule rs ;
      Env.add_rw lc hd rs

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
  Global.vprint (lazy " -- Processing standard input ...") ;
  parse (Lexing.from_channel stdin) ;
  Global.print_ok ()
            
let run_on_file file =
  let input = open_in file in
    Global.vprint (lazy (" -- Processing file '" ^ file ^ "' ...")) ;
    parse (Lexing.from_channel input) ;
    Global.print_ok ()

(* *** Arguments *** *)

let args = [
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"               ) ;
        ("-v"    , Arg.Clear Global.quiet               , "Verbose"             ) ;
        ("-e"    , Arg.Set Global.export                , "Create a .dko"       ) ;
        ("-nc"   , Arg.Clear Global.color               , "Disable colored output"       ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) ; 
        ("-r"    , Arg.Set Global.raphael               , "Undocumented"  ) 
]

(* *** Main *** *)

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkcheck [options] files"  
  with 
    | Sys_error err             -> Global.error dloc err
    | LexerError (lc,err)       -> Global.error lc err
    | ParserError (lc,err)      -> Global.error lc err
    | TypingError (lc,err)      -> Global.error lc err
    | EnvError (lc,err)         -> Global.error lc err 
    | PatternError (lc,err)     -> Global.error lc err 
