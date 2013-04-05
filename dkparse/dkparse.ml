
open Types

(* Error Msgs *)

let error str = 
  Global.debug ("\027[31m" ^ str ^ "\027[m\n");
  CodeGeneration.exit () ;      
  exit 1 

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
            raise (ParserError (ParsingError (tok,(line,cnum))))
        end

(* Run *)

let run input =
  try
    Global.debug (" --- Processing " ^ !Global.name ^ " --- \n");
    let lexbuf = Lexing.from_channel input in
      CodeGeneration.prelude () ;
      parse lexbuf
  with 
    | ParserError err           -> error ( Debug.string_of_perr err ) 
    | End_of_file_in_comment    -> error ("Unexpected end of file.") 
    | End_of_file               -> ( Hashtbl.clear Global.gs ; Global.name := "" )

let run_on_file str =
  let file = try open_in str with Sys_error msg -> error ("System error: "^msg) in
    ( if !Global.name = "" then
        let bname = Filename.basename str in
        let name =
          try Filename.chop_extension bname
          with Invalid_argument _ -> bname
        in 
          Global.set_name name ) ;
    run file

let run_on_stdin _ = 
  ( if !Global.name == "" then Global.set_name "out" ) ; 
  run stdin ; 
  exit(0)

(* Main *)

let args = [
        ("-o"    , Arg.String Global.set_out            , "Output file"         ) ; 
        ("-n"    , Arg.String Global.set_name           , "Set module name"     ) ; 
        ("-lpath", Arg.String Global.set_path           , "Set Lua path"        ) ;
        ("-c"    , Arg.Set Global.do_not_check          , "Do not check"        ) ;
        ("-u"    , Arg.Clear Global.check_ext           , "Unsafe external symbols" ) ;
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"               ) ;
        ("-l"    , Arg.String Global.add_lib            , "Load a library"      ) ;
        ("-r"    , Arg.Set Global.ignore_redeclarations , "Ignore redeclarations" ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) 
]

let _ = 
  try Arg.parse args run_on_file "Usage: dkparse [options] files"  
  with OptionError err -> error (Debug.string_of_oerr err) 
