open Term

module P = Parser.Make(Dep)

let parse lb =
  try
      P.prelude Lexer.token lb ;
      while true do P.line Lexer.token lb done
  with
    | Tokens.EndOfFile -> ()
    | P.Error       -> Print.fail (Lexer.get_loc lb)
                         "Unexpected token '%s'." (Lexing.lexeme lb)

let run_on_file file =
  let input = open_in file in
    parse (Lexing.from_channel input)

let args =
  [ ("-o", Arg.String (fun fi -> Dep.out := open_out fi), "Output file"  ) ]

let _ =
  try
    Arg.parse args run_on_file "Usage: dkdep [options] files"
  with
    | Sys_error err             -> Printf.eprintf "ERROR %s.\n" err; exit 1
    | Exit                      -> exit 3
