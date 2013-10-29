open Printf
open Types

let ascii29 = Char.chr 29 (* 1d : end of block *)
let ascii30 = Char.chr 30 (* 1e : end of line  *)
let ascii31 = Char.chr 31 (* 1f : separator *)

let print_qid m id = 
  if m = !Global.name then
    fprintf !Global.out "%s" (string_of_ident id) 
  else
    fprintf !Global.out "%s.%s" (string_of_ident m) (string_of_ident id)

let rec print_term : pterm -> unit = function
  | PType _            -> fprintf !Global.out "Type"
  | PId (_,id)         -> fprintf !Global.out "%s" (string_of_ident id)
  | PQid (_,m,id)      -> print_qid m id
  | PApp (f,a)         -> 
      begin
        print_term_wp f ;
        fprintf !Global.out " " ;
        print_term_wp a ;
      end
  | PLam (v,None,te)   ->
      begin
        fprintf !Global.out "%s => " (string_of_ident (snd v));
        print_term_wp te ;
      end
  | PLam (v,Some ty,te)->
      begin
        fprintf !Global.out "%s:" (string_of_ident (snd v));
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
        fprintf !Global.out "%s:" (string_of_ident (snd v)) ;
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
  fprintf !Global.out "%s:" (string_of_ident id) ;
  print_term ty 

let mk_rule (env,((_,id),pats),te) = 
  fprintf !Global.out "_ : [" ;
  ( match env with 
      | []      -> ()
      | e::env' -> ( print_ldec e ; List.iter (fun e' -> fprintf !Global.out "," ; print_ldec e' ) env' )
  ) ;
  fprintf !Global.out "] %s" (string_of_ident id) ; 
  Array.iter print_pat pats ;
  fprintf !Global.out " --> " ;
  print_term te ;
  fprintf !Global.out "\n\t%c role RewriteRule\n\t%c\n\n" ascii31 ascii30

let mk_prelude lc m =
  fprintf !Global.out "namespace %s %c\n\ntheory FILENAME =\n\n" (string_of_ident m) ascii29 ; 
  Global.name := m 

let mk_require _ _ = () 

let mk_declaration _ id pty = 
  fprintf !Global.out "%s : " (string_of_ident id) ;
  print_term pty ;
  fprintf !Global.out " %c\n\n" ascii30 

let mk_definition _ id pty_opt pte = 
  match pty_opt with
    | None      -> failwith "Not implemented (definition without type)."
    | Some pty  ->
        begin
          fprintf !Global.out "%s : " (string_of_ident id) ;
          print_term pty ;
          fprintf !Global.out "%c\n = " ascii31 ;
          print_term pte ;
          fprintf !Global.out "%c\n" ascii30
        end

let mk_opaque lc id pty_opt pte = 
 failwith "Not implemented (opaque definitions)." 

let mk_term _ = () 

let mk_rules lst = 
  List.iter mk_rule lst

let mk_ending _  = 
  fprintf !Global.out "\n%c" ascii29 
