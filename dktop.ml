
open Types

module M =
struct

  let mk_prelude _ _ = assert false 

  let mk_require lc m = 
    Env.import lc m ;
    Global.sprint ("Module '" ^ string_of_ident m ^ "' imported.\n")

  let mk_declaration lc id pty = 
    Checker.add_decl lc id pty ;
    Global.sprint (string_of_ident id ^ " is declared.\n" ) 

  let mk_definition lc id pty_opt pte = 
    Checker.add_def lc id pty_opt pte ;
    Global.sprint (string_of_ident id ^ " is defined.\n") 

  let mk_opaque lc id pty_opt pte = 
    Checker.add_opaque lc id pty_opt pte ;
    Global.sprint (string_of_ident id ^ " is defined.\n") 

  let mk_term pte = 
    let te = Pterm.of_pterm [] pte in
    let te' = Reduction.hnf te in
      Global.sprint ( Error.string_of_term te' ^ "\n" )

  let mk_rules rules = 
    let aux = function
    | (_,((l,v),_),_)::_  -> (l,v)
    | _                     -> assert false
    in
    let (lc,hd) = aux rules in
      Checker.add_rules lc hd rules ; 
      Global.sprint ("Rules added.\n") 

  let mk_ending _ : unit = exit 0

end

module P = Parser.Make(M)

let rec parse lb = 
  try
      while true do 
        Global.sprint ">> ";
        P.line Lexer.token lb 
      done
  with 
    | EndOfFile -> ()
    | e         ->
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
