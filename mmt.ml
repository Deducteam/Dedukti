open Printf
open Types

let ascii29 = Char.chr 29 (* 1d : end of block *)
let ascii30 = Char.chr 30 (* 1e : end of line  *)
let ascii31 = Char.chr 31 (* 1f : separator *)

let print_id m id =
  match m with
    | None      -> fprintf !Global.out "%s" (string_of_ident id)
    | Some md   ->
        if md = !Global.name then
          fprintf !Global.out "%s" (string_of_ident id)
        else
          fprintf !Global.out "%s.%s" (string_of_ident md) (string_of_ident id)

let rec print_preterm = function
  | PreType _           -> fprintf !Global.out "Type"
  | PreId (_,v)         -> print_id None v
  | PreQId (_,m,v)      -> print_id (Some m) v
  | PreApp args         -> List.iter print_preterm_wp args
  | PreLam (_,x,ty,te)  ->
      begin
        fprintf !Global.out "%s:" (string_of_ident x);
        print_preterm_wp ty ;
        fprintf !Global.out " => " ;
        print_preterm_wp te
      end
  | PrePi (None,ty,te)   ->
      begin
        print_preterm_wp ty ;
        fprintf !Global.out " -> " ;
        print_preterm_wp te ;
      end
  | PrePi (Some (l,x),ty,te)   ->
      begin
        fprintf !Global.out "%s:" (string_of_ident x) ;
        print_preterm_wp ty ;
        fprintf !Global.out " -> " ;
        print_preterm_wp te ;
      end

and print_preterm_wp = function
  | PreType _ | PreId (_,_) | PreQId (_,_,_)  as t      -> print_preterm t
  | t   ->
      fprintf !Global.out "(";
      print_preterm t ;
      fprintf !Global.out ")"

let rec print_pat = function
  | Unknown (_,_)               -> failwith "Not implemented (Dash Patterns)."
  | PPattern (_,m,id,pats)    ->
      if List.length pats = 0 then
        begin
          fprintf !Global.out " " ;
          print_id m id
        end
      else
        begin
          fprintf !Global.out " (" ;
          print_id m id ;
          List.iter print_pat pats;
          fprintf !Global.out ")"
        end

let print_ldec (_,id,ty) =
  fprintf !Global.out "%s:" (string_of_ident id) ;
  print_preterm ty

let mk_rule (env,(_,id,pats),ri:prule) =
  fprintf !Global.out "_ : [" ;
  ( match env with
      | []      -> ()
      | e::env' ->
          begin
            print_ldec e ;
            List.iter (fun e' -> fprintf !Global.out "," ; print_ldec e' ) env'
          end
  ) ;
  fprintf !Global.out "] %s" (string_of_ident id) ;
  List.iter print_pat pats ;
  fprintf !Global.out " --> " ;
  print_preterm ri ;
  fprintf !Global.out "\n\t%c role RewriteRule\n\t%c\n\n" ascii31 ascii30

(* *** *)

let mk_prelude lc m =
  fprintf !Global.out "namespace %s %c\n\ntheory FILENAME =\n\n" (string_of_ident m) ascii29

let mk_require _ _ = ()

let mk_declaration _ id pty =
  fprintf !Global.out "%s : " (string_of_ident id) ;
  print_preterm pty ;
  fprintf !Global.out " %c\n\n" ascii30

let mk_definition _ id pty_opt pte =
  match pty_opt with
    | None      -> failwith "Not implemented (definition without type)."
    | Some pty  ->
        begin
          fprintf !Global.out "%s : " (string_of_ident id) ;
          print_preterm pty ;
          fprintf !Global.out "%c\n = " ascii31 ;
          print_preterm pte ;
          fprintf !Global.out "%c\n" ascii30
        end

let mk_opaque lc id pty_opt pte =
 failwith "Not implemented (opaque definitions)."

let mk_command _ _ _ = ()

let mk_rules = List.iter mk_rule

let mk_ending _  =
  fprintf !Global.out "\n%c" ascii29
