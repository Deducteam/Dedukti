open Types

exception IncorrectFileName

(* Arguments *)

let args = [
        ("-o", Arg.String (fun s -> Global.out   := (open_out s)  )     , "output file"         ) ;
        ("-c", Arg.Set Global.do_not_check                              , "do not check"        ) ;
        ("-q", Arg.Set Global.quiet                                     , "quiet"               ) ;
        ("-l", Arg.String (fun s -> Global.libs := s::(!Global.libs))   , "load a library"      ) ;
        ("-r", Arg.Set Global.ignore_redeclarations                     , "ignore redeclarations" )
]

let set_name str =
  let bname = Filename.basename str in
  let name  =
    try Filename.chop_extension bname
    with Invalid_argument _ -> bname
  in 
    if Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z_0-9]*") name 0 then
      Global.name := name
    else
      raise IncorrectFileName 

(* Error Msgs *)

let error str = prerr_string str ; prerr_newline () ; exit 1 

(* Parsing *)

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
            raise (ParsingError (ParserError (tok,(line,cnum))))
        end

(* Main *)

let main str =
  try
    if !Global.quiet then () else prerr_endline (" --- Processing " ^ str ^ " --- ");
    let file = open_in str      in
    let _ = set_name str        in
    let lexbuf = Lexing.from_channel file in
      CodeGeneration.prelude () ;
      parse lexbuf
  with 
    | ParsingError err          -> error ("\027[31m" ^ (Debug.string_of_perr err) ^ "\027[m")
    | Sys_error msg             -> error ("System error: "^msg)
    | IncorrectFileName         -> error ("Incorrect File Name.") 
    | End_of_file               -> ( Hashtbl.clear Global.gs )

let _ = Arg.parse args main "Usage: dkparse [options] files"  
  
