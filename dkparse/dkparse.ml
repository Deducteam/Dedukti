
open Types

(* Arguments *)

let args = [
        ("-o",Arg.String (fun s -> Global.out := (open_out s) ),"")
]

let set_name str =
  let bname = Filename.basename str in
  let name  =
    try Filename.chop_extension bname
    with Invalid_argument _ -> bname
  in 
    Global.name := name

(* Error Msgs *)

let error str = prerr_string str ; prerr_newline () ; exit 1 

(* Paring *)

let parse lb = 
  try
      while true do Parser.top Lexer.token lb done
  with 
    | Parsing.Parse_error       -> 
        begin
          let curr = lb.Lexing.lex_curr_p in
          let line = curr.Lexing.pos_lnum in
          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (Error (ParsingError (tok,(line,cnum))))
        end

(* Main Entry *)

let main str =
  try
    let file = open_in str      in
    let _ = set_name str        in
    let lexbuf = Lexing.from_channel file in
      LuaGenerationBase.emit ("--[[ Code for module "^(!Global.name)^". ]]\n");
      LuaGenerationBase.emit ("local "^(!Global.name)^" = { }\n\n") ; 
      parse lexbuf
  with 
    | Error err         -> error (Debug.string_of_err err)
    | Sys_error msg     -> error ("System error: "^msg)
    | End_of_file       -> exit 0

let _ = Arg.parse args main "usage: ..."
  
