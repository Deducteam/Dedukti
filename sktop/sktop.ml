module P = Parser.Make(Top)

let rec parse lb =
  try
    while true do
      print_string ">> "; flush stdout; P.line Lexer.token lb
    done
  with
    | Exit      ->  flush stderr; parse lb
    | P.Error   ->
        Printf.eprintf "Unexpected token '%s'.\n" (Lexing.lexeme lb);
        flush stderr; parse lb
    | Tokens.EndOfFile -> exit 0

let  _ =
  print_string "Welcome to Sukerujo\n";
  let v = Basics.hstring "?top" in
    Pp.name := v;
    Scoping.name := v;
    Env.init v ;
    parse (Lexing.from_channel stdin)
