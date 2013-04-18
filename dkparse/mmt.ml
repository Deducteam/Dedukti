
open Printf
open Types

let ascii29 = Char.chr 29
let ascii30 = Char.chr 30
let ascii31 = Char.chr 31

let rec print_term = function
  | P_Type              -> fprintf !Global.out "Type "
  | P_Id (_,id)         -> fprintf !Global.out "%s" id
  | P_Qid (_,m,id)      -> fprintf !Global.out "%s.%s" m id
  | P_App (f,a)         -> 
      begin
        fprintf !Global.out "(" ;
        print_term f ;
        fprintf !Global.out " " ;
        print_term a ;
        fprintf !Global.out ")" 
      end
  | P_Lam (v,None,te)   ->
      begin
        fprintf !Global.out "(%s => " (snd v);
        print_term te ;
        fprintf !Global.out ")" 
      end
  | P_Lam (v,Some ty,te)->
      begin
        fprintf !Global.out "(%s:" (snd v);
        print_term ty ;
        fprintf !Global.out " => " ;
        print_term te ;
        fprintf !Global.out ")" 
      end
  | P_Pi (None,a,b)     ->
      begin
        fprintf !Global.out "(" ;
        print_term a ;
        fprintf !Global.out " -> " ;
        print_term b ;
        fprintf !Global.out ")" 
      end
  | P_Pi (Some v,a,b)   ->
      begin
        fprintf !Global.out "(%s:" (snd v) ;
        print_term a ;
        fprintf !Global.out " -> " ;
        print_term b ;
        fprintf !Global.out ")" 
      end 

let print_dot d =
  fprintf !Global.out " {" ;
  print_term d ;
  fprintf !Global.out "} " 

let rec print_pat = function
  | Pat_Id v                    -> fprintf !Global.out " %s " (snd v) 
  | Pat_Pa ((_,m,id),dots,pats) ->
      begin
        fprintf !Global.out " (%s.%s " m id ; 
        Array.iter print_dot dots;
        Array.iter print_pat pats;
        fprintf !Global.out ") " 
      end

let print_ldec ((_,id),ty) = 
  fprintf !Global.out "%s:" id ;
  print_term ty 

let mk_rule ((env,((_,id),dots,pats),te):prule) = 
  fprintf !Global.out "_ : [" ;
  ( match env with 
      | []      -> ()
      | e::env' -> ( print_ldec e ; List.iter (fun e' -> fprintf !Global.out "," ; print_ldec e' ) env' )
  ) ;
  fprintf !Global.out "] %s " id ; 
  Array.iter print_dot dots ;
  Array.iter print_pat pats ;
  fprintf !Global.out " --> " ;
  print_term te ;
  fprintf !Global.out " %c role RewriteRule\n" ascii31

    (* ************** *)

let mk_prelude (l,name) = 
  fprintf !Global.out "namespace %s %c\ntheory FILENAME =\n\n" name ascii29 

let mk_ending _ = 
  fprintf !Global.out "\n%c" ascii29 

let mk_declaration ((_,id),ty) = 
  fprintf !Global.out "%s : " id ;
  print_term ty ;
  fprintf !Global.out "%c\n" ascii30 

let mk_definition _ = 
  assert false (*TODO*)

let mk_rules (rs:prule list) = 
  List.iter mk_rule rs

let mk_require dep = 
  assert false (*TODO*) 
