module P = Parser.Make(Top)


let parse' lb =
  let rec aux m =    
    try
      aux (Top.bind m (fun _ -> (P.line Lexer.token lb)))
    with
    | Tokens.EndOfFile -> m
    | P.Error -> Errors.fail (Lexer.get_loc lb) "Unexpected token '%s'." (Lexing.lexeme lb)
  in
  try
    let m = P.prelude Lexer.token lb in
    let m' = aux m in
    Top.mk_ending m'
  with
  | Tokens.EndOfFile -> ()
  | P.Error       -> Errors.fail (Lexer.get_loc lb)
    "Unexpected token '%s'." (Lexing.lexeme lb)

let  _ =
  print_string "Welcome to Dedukti\n";
  let v = Basics.hstring "?top" in
    Pp.name := v;
    Scoping.name := v;
    Env.init v ;
    parse' (Lexing.from_channel stdin)
