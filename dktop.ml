
open Types

module M =
struct

  let mk_prelude (l,v : loc*ident) : unit = assert false 

  let mk_require (l,v : loc*ident) : unit = 
    Env.import l v ;
    Global.sprint (">> Module '" ^ string_of_ident v ^ "' imported.\n")

  let mk_declaration ((l,id),pty : (loc*ident)*pterm) : unit = 
    Checker.add_decl ((l,id),pty) ;
    Global.sprint (">> " ^string_of_ident id ^ " is declared.\n" ) 

  let mk_definition ((l,id),pty,pte : (loc*ident)*pterm*pterm) : unit = 
    Checker.add_def ((l,id),pty,pte) ;
    Global.sprint (">> " ^ string_of_ident id ^ " is defined.\n") 

  let mk_infered_def ((l,id),pte : (loc*ident)*pterm) : unit =
    Checker.add_idef ((l,id),pte) ;
    Global.sprint (">> " ^ string_of_ident id ^ " is defined.\n") 

  let mk_opaque ((l,id),pty,pte : (loc*ident)*pterm*pterm) : unit = 
    Checker.add_odef ((l,id),pty,pte) ;
    Global.sprint (">> " ^ string_of_ident id ^ " is defined.\n") 

  let mk_typecheck (l,pty,pte : loc*pterm*pterm) :unit = 
    Checker.typecheck (l,pty,pte) ;
    Global.sprint (">> This expression is well-typed.\n") 

  let mk_normalize (pte : pterm) : unit = 
    let te = Pterm.of_pterm [] pte in
    let te' = Reduction.hnf te in
      Global.sprint ( ">> " ^ Error.string_of_term te' ^ "\n" )

  let mk_rules (lst:prule list) : unit = 
    let aux = function
    | (_,((l,v),_),_)::_  -> (l,v)
    | _                     -> assert false
    in
    let (l,v) = aux lst in
      Checker.add_rules l v lst ; 
      Global.sprint (">> Rules added.\n") 

  let mk_ending _ : unit = exit 0

end

module P = Parser.Make(M)

let rec parse lb = 
  try
      while true do P.line Lexer.token lb done
  with 
    | e ->
        begin
          let (etype,msg) = match e with
            | P.Error       -> ( "Parsing Error" , "Unexpected token '" ^ (Lexing.lexeme lb) ^ "'." ) 
            | LexerError (_,err)        -> ( "Lexing Error" , err )
            | ParserError (_,err)       -> ( "Parsing Error" , err )
            | TypingError (_,err)       -> ( "Typing Error" , err )
            | EnvError (_,err)          -> ( "Scoping Error" , err )
            | PatternError (_,err)      -> ( "Rule Error" , err )
            | u                         -> raise u
          in
            Global.error2 etype msg ;
            parse lb
        end

let ascii_art =
"==========================================================================
 \\ \\    / /__| |__ ___ _ __  ___  | |_ ___  |   \\ ___ __| |_  _| |_| |_(_)
  \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_) |  _/ _ \\ | |) / -_) _` | || | / /  _| |
   \\_/\\_/\\___|_\\__\\___/_|_|_\\___|  \\__\\___/ |___/\\___\\__,_|\\_,_|_\\_\\\\__|_|
==========================================================================
"

let  _ =
  Global.sprint ascii_art ; 
  let v = hstring "Top" in
    Global.name := v ;
    Env.init v ;
    parse (Lexing.from_channel stdin) 
