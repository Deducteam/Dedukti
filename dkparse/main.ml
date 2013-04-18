
open Types

(* Error Msgs *)

let error str = 
  Global.debug ("\027[31m" ^ str ^ "\027[m\n");
  (*CodeGeneration.exit () ; FIXME *)    
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

let run file =
  try
    match file with
      | None            ->
          begin
            (*Global.debug (" >>> Processing from stdin\n");*)
            parse (Lexing.from_channel stdin)
          end
      | Some file       ->
          begin
            let input = open_in file in
              (*Global.debug (" >>> Processing file '" ^ file ^ "' \n") ;*)
              parse (Lexing.from_channel input)
          end
  with 
    | Sys_error err             -> error ("System error: "^err)
    | ParserError err           -> error ( Debug.string_of_perr err ) 
    | End_of_file_in_comment    -> error ("Unexpected end of file.") 
    | End_of_file               -> () (*Global.debug (" <<< DONE !\n") *)

let run_on_file f = run (Some f)
let run_on_stdin _ = run None

(* Main *)

let args = [
        ("--mmt" , Arg.Unit Global.set_mmt              , "Dedukti to MMT"      ) ; 
        ("-o"    , Arg.String Global.set_out            , "Output file"         ) ; 
        ("-lpath", Arg.String Global.set_path           , "Set Lua path"        ) ;
        ("-c"    , Arg.Set Global.do_not_check          , "Do not check"        ) ;
        ("-u"    , Arg.Clear Global.check_ext           , "Unsafe external symbols" ) ; (*FIXME*)
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"               ) ;
        ("-l"    , Arg.String Global.add_lib            , "Load a library"      ) ;
        ("-r"    , Arg.Set Global.ignore_redeclarations , "Ignore redeclarations" ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) 
]

let _ = 
  try Arg.parse args run_on_file "Usage: dkparse [options] files"  
  with OptionError err -> error (Debug.string_of_oerr err) 
