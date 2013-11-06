
open Types

module M =
struct

  let mk_prelude _ _ = assert false 

  let mk_require lc m =  
    Global.sprint "Nothing was done (obsolete feature)."

  let mk_declaration lc id ty = 
    Inference.check_type [] ty ;
    Env.add_decl lc id ty ;
    Global.sprint (string_of_ident id ^ " is declared." ) 

  let mk_definition lc id ty_opt te = 
    let ty = 
      match ty_opt with
        | None          -> Inference.infer [] te
        | Some ty       -> ( Inference.check_type [] ty ; Inference.check_term [] te ty ; ty )
    in
      Env.add_def lc id te ty ; 
      Global.sprint (string_of_ident id ^ " is defined.") 

  let mk_opaque lc id ty_opt te = 
    let ty = 
      match ty_opt with
        | None          -> Inference.infer [] te
        | Some ty       -> ( Inference.check_type [] ty ; Inference.check_term [] te ty ; ty )
    in
      Env.add_decl lc id ty ;
      Global.sprint (string_of_ident id ^ " is defined.") 

  let mk_term te = 
    Global.sprint ( Pp.string_of_term (Reduction.hnf te)  )

  let mk_rules rs = 
    let (lc,hd) = match rs with
      | (_,((l,v),_),_)::_      -> (l,v)
      | _                       -> assert false
    in
      List.iter Inference.check_rule rs ;
      Env.add_rw lc hd rs ;
      Global.sprint ("Rules added.") 

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
    | EndOfFile                 -> exit 0
    | P.Error                   -> error lb ("Unexpected token '" ^ (Lexing.lexeme lb) ^ "'." ) 
    | LexerError (_,err)  | ParserError (_,err) | TypingError (_,err) | EnvError (_,err)       
    | PatternError (_,err)      ->  error lb err
  
and error lb err = Global.sprint err ; parse lb
        
(*
let ascii_art =
"==========================================================================
 \\ \\    / /__| |__ ___ _ __  ___  | |_ ___  |   \\ ___ __| |_  _| |_| |_(_)
  \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_) |  _/ _ \\ | |) / -_) _` | || | / /  _| |
   \\_/\\_/\\___|_\\__\\___/_|_|_\\___|  \\__\\___/ |___/\\___\\__,_|\\_,_|_\\_\\\\__|_|
==========================================================================
"*)
let ascii_art =
"=============================================================================
 \\ \\    / /__| |__ ___ _ __  ___  | |_ ___  |   \\ ___ __| |_  _  _  _ | |_ _
  \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_) |  _/ _ \\ | |) / -_) _` | || || |/ /|  _(_)
   \\_/\\_/\\___|_\\__\\___/_|_|_\\___|  \\__\\___/ |___/\\___\\__,_|\\_,_||_|\\_\\ \\__|_|
=============================================================================
"  

let  _ =
  Global.sprint ascii_art ; 
  let v = hstring "Top" in
    Global.name := v ;
    Env.init v ;
    parse (Lexing.from_channel stdin) 
