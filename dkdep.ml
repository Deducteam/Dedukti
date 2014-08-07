open Types

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
            Global.fail (mk_loc l c) "Unexpected token '%s'." tok
        end
    | EndOfFile -> ()

let run_on_file file =
  let input = open_in file in
    Global.file := file ;
    parse (Lexing.from_channel input)

let args =
  let _set_out fi = Global.out := Format.formatter_of_out_channel (open_out fi) in
  [ ("-o", Arg.String _set_out, "Output file"  ) ]

let _ =
  try
    Arg.parse args run_on_file "Usage: dkdep [options] files"
  with
    | Sys_error err             -> Printf.eprintf "ERROR %s.\n" err; exit 1
    | Exit                      -> exit 3
