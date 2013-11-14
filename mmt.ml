open Printf
open Types

(* TODO to be rewritten *)

let ascii29 = Char.chr 29 (* 1d : end of block *)
let ascii30 = Char.chr 30 (* 1e : end of line  *)
let ascii31 = Char.chr 31 (* 1f : separator *)

let print_qid m id =
  if m = !Global.name then
    fprintf !Global.out "%s" (string_of_ident id)
  else
    fprintf !Global.out "%s.%s" (string_of_ident m) (string_of_ident id)

let rec print_pterm _ = assert false
(*
  function
  | Type _              -> fprintf !Global.out "Type"
  | DB (_,x,_)          -> fprintf !Global.out "%s" (string_of_ident x)
  | GVar (_,m,v)        -> print_qid m v
  | App args            -> List.iter print_term_wp args
  | Lam (_,x,ty,te)     ->
      begin
        fprintf !Global.out "%s:" (string_of_ident x);
        print_term_wp ty ;
        fprintf !Global.out " => " ;
        print_term_wp te
      end
  | Pi (_,x,ty,te)      ->
      if x==empty then
        begin
          print_term_wp ty ;
          fprintf !Global.out " -> " ;
          print_term_wp te ;
        end
      else
        begin
          fprintf !Global.out "%s:" (string_of_ident x) ;
          print_term_wp ty ;
          fprintf !Global.out " -> " ;
          print_term_wp te ;
        end
  | Meta _              -> assert false
  | Kind                -> assert false

and print_term_wp = function
  | Type _ | DB _ | GVar _  as t        -> print_term t
  | t   ->
      fprintf !Global.out "(";
      print_term t ;
      fprintf !Global.out ")" *)
(*
let rec print_pat = function
  | Dash _                      -> failwith "Not implemented (Dash Patterns)."
  | Var (_,x,_)                 -> fprintf !Global.out "%s" (string_of_ident x)
  | Pattern ((_,m,id),pats)     ->
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

let print_ldec (_,id,ty) =
  fprintf !Global.out "%s:" (string_of_ident id) ;
  print_term ty
 *)
let mk_rule _ = assert false
  (*
  fprintf !Global.out "_ : [" ;
  ( match env with
      | []      -> ()
      | e::env' -> ( print_ldec e ; List.iter (fun e' -> fprintf !Global.out "," ; print_ldec e' ) env' )
  ) ;
  fprintf !Global.out "] %s" (string_of_ident id) ;
  Array.iter print_pat pats ;
  fprintf !Global.out " --> " ;
  print_term te ;
  fprintf !Global.out "\n\t%c role RewriteRule\n\t%c\n\n" ascii31 ascii30 *)

(* *** *)

let mk_prelude lc m =
  fprintf !Global.out "namespace %s %c\n\ntheory FILENAME =\n\n" (string_of_ident m) ascii29

let mk_require _ _ = ()

let mk_declaration _ id pty =
  fprintf !Global.out "%s : " (string_of_ident id) ;
  print_pterm pty ;
  fprintf !Global.out " %c\n\n" ascii30

let mk_definition _ id pty_opt pte =
  match pty_opt with
    | None      -> failwith "Not implemented (definition without type)."
    | Some pty  ->
        begin
          fprintf !Global.out "%s : " (string_of_ident id) ;
          print_pterm pty ;
          fprintf !Global.out "%c\n = " ascii31 ;
          print_pterm pte ;
          fprintf !Global.out "%c\n" ascii30
        end

let mk_opaque lc id pty_opt pte =
 failwith "Not implemented (opaque definitions)."

let mk_term _ = ()

let mk_rules lst =
  List.iter mk_rule lst

let mk_ending _  =
  fprintf !Global.out "\n%c" ascii29
