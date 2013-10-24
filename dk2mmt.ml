open Printf
open Types

(* DK -> MMT *)

let ascii29 = Char.chr 29 (* 1d : end of block *)
let ascii30 = Char.chr 30 (* 1e : end of line  *)
let ascii31 = Char.chr 31 (* 1f : separator *)

let print_qid m id = 
  if m = !Global.name then
    fprintf !Global.out "%s" id 
  else
    fprintf !Global.out "%s.%s" m id

let rec print_term : pterm -> unit = function
  | PType _            -> fprintf !Global.out "Type"
  | PId (_,id)         -> fprintf !Global.out "%s" id
  | PQid (_,m,id)      -> print_qid m id
  | PApp (f,a)         -> 
      begin
        print_term_wp f ;
        fprintf !Global.out " " ;
        print_term_wp a ;
      end
  | PLam (v,None,te)   ->
      begin
        fprintf !Global.out "%s => " (snd v);
        print_term_wp te ;
      end
  | PLam (v,Some ty,te)->
      begin
        fprintf !Global.out "%s:" (snd v);
        print_term_wp ty ;
        fprintf !Global.out " => " ;
        print_term_wp te ;
      end
  | PPi (None,a,b)     ->
      begin
        print_term_wp a ;
        fprintf !Global.out " -> " ;
        print_term_wp b ;
      end
  | PPi (Some v,a,b)   ->
      begin
        fprintf !Global.out "%s:" (snd v) ;
        print_term_wp a ;
        fprintf !Global.out " -> " ;
        print_term_wp b ;
      end 

and print_term_wp = function
  | PType _ | PId (_,_) | PQid (_,_,_) as t       -> print_term t
  | t   ->
      fprintf !Global.out "("; 
      print_term t ;
      fprintf !Global.out ")" 

let print_dot d =
  fprintf !Global.out " {" ;
  print_term d ;
  fprintf !Global.out "} " 

let rec print_pat = function
  | PDash                       -> failwith "Not implemented (Dash Patterns)."
  | PPat ((_,m,id),pats)        ->
      if Array.length pats = 0 then
        begin
          fprintf !Global.out " " ; 
          print_qid m id
        end
      else
        begin
          fprintf !Global.out " (" ; 
          print_qid m id ;
          Array.iter print_pat pats;
          fprintf !Global.out ")" 
        end

let print_ldec ((_,id),ty) = 
  fprintf !Global.out "%s:" id ;
  print_term ty 

let mk_rule (env,((_,id),pats),te) = 
  fprintf !Global.out "_ : [" ;
  ( match env with 
      | []      -> ()
      | e::env' -> ( print_ldec e ; List.iter (fun e' -> fprintf !Global.out "," ; print_ldec e' ) env' )
  ) ;
  fprintf !Global.out "] %s" id ; 
  Array.iter print_pat pats ;
  fprintf !Global.out " --> " ;
  print_term te ;
  fprintf !Global.out "\n\t%c role RewriteRule\n\t%c\n\n" ascii31 ascii30

(* DK -> MMT (module) *)

module Mmt =
struct

let mk_prelude (l,v : loc*string) : unit =
        fprintf !Global.out "namespace %s %c\n\ntheory FILENAME =\n\n" v ascii29 ; 
        Global.set_name v 

let mk_require (l,v : loc*string) : unit = 
        failwith "Not implemented (#IMPORT)"

let mk_declaration ((l,id),pty : (loc*string)*pterm) : unit = 
        fprintf !Global.out "%s : " id ;
        print_term pty ;
        fprintf !Global.out " %c\n\n" ascii30 

let mk_definition ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
        fprintf !Global.out "%s : " id ;
        print_term pty ;
        fprintf !Global.out "%c\n = " ascii31 ;
        print_term pte ;
        fprintf !Global.out "%c\n" ascii30

let mk_infered_def ((l,id),pte : (loc*string)*pterm) : unit = 
        failwith "Not implemented"

let mk_opaque ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
        failwith "Not implemented"

let mk_typecheck (l,pty,pte : loc*pterm*pterm) : unit = 
        failwith "Not implemented"

let mk_normalize (pte : pterm) : unit = 
        failwith "Not implemented"

let mk_rules (lst:prule list) : unit = 
        List.iter mk_rule lst

let mk_ending _ : unit = 
        fprintf !Global.out "\n%c" ascii29 
 
end

(* Error Msgs *)

let error e str = 
  Global.eprint ("\n\027[31m["^e^"]\027[m " ^ str ^ "\n");
  exit 1 

(* Parsing *)

module P = Parser.Make(Mmt) 

let parse lb = 
  try
      P.top Lexer.token lb
  with 
    | P.Error       -> 
        begin
          let curr = lb.Lexing.lex_curr_p in
          let l = curr.Lexing.pos_lnum in
          let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (ParserError ( (l,c) , "Unexpected token '" ^ tok ^ "'." ) ) 
        end

(* Input *)

let run_on_stdin _ =
  Global.eprint (" -- Processing standard input ...\t") ;
  Global.vprint "\n";
  parse (Lexing.from_channel stdin) ;
  Global.eprint ("\027[32m[DONE]\027[m\n") ;
  Env.export_and_clear ()
            
let run_on_file file =
  let input = open_in file in
    Global.eprint (" -- Processing file '" ^ file ^ "' ...\t") ;
    Global.vprint "\n";
    parse (Lexing.from_channel input) ;
    Global.eprint ("\027[32m[DONE]\027[m\n") ;
    Env.export_and_clear ()

(* Args *)

let args = [
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) ; 
        ("-o", Arg.String Global.set_out                , "Output file"  ) ; 
]

(* Main *)

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkcheck [options] files"  
  with 
    | Sys_error err             -> Global.error (0,0) "System Error"  err
    | LexerError (lc,err)       -> Global.error lc "Lexing Error"  err
    | ParserError (lc,err)      -> Global.error lc "Parsing Error"  err


