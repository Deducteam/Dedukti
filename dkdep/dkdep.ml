open Term

module P = Parser.Make(Dep)

let parse lb =
  try
      P.prelude Lexer.token lb ;
      while true do P.line Lexer.token lb done
  with
    | P.Error       ->
        begin
          let curr = lb.Lexing.lex_curr_p in
          let l = curr.Lexing.pos_lnum in
          let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            Print.fail (mk_loc l c) "Unexpected token '%s'." tok
        end
    | Tokens.EndOfFile -> ()

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
