open Printf
open Types

let ascii29 = Char.chr 29 (* 1d : end of block *)
let ascii30 = Char.chr 30 (* 1e : end of line  *)
let ascii31 = Char.chr 31 (* 1f : separator *)

let print_id out (m,id) =
  match m with
    | None      -> fprintf out "%s" (string_of_ident id)
    | Some md   ->
        if md = !Global.name then
          fprintf out "%s " (string_of_ident id)
        else
          fprintf out "%s.%s" (string_of_ident md) (string_of_ident id)

let rec print_preterm out = function
  | PreType _           -> fprintf out "Type" (*FIXME*)
  | PreId (_,v)         -> print_id out (None,v)
  | PreQId (_,m,v)      -> print_id out (Some m,v)
  | PreApp (f::args)    ->
      begin
        print_preterm_wp out f ;
        List.iter (fprintf out " %a" print_preterm_wp) args
      end
  | PreLam (_,x,ty,te)  ->
        fprintf out "%s:%a => %a"
          (string_of_ident x)
          print_preterm_wp ty
          print_preterm_wp te
  | PrePi (None,ty,te)  ->
        fprintf !Global.out "%a -> %a"
          print_preterm_wp ty
          print_preterm_wp te
  | PrePi (Some (l,x),ty,te)   ->
        fprintf out "%s:%a -> %a"
          (string_of_ident x)
          print_preterm_wp ty
          print_preterm_wp te
  | PreApp [] -> assert false

and print_preterm_wp out = function
  | PreType _ | PreId (_,_) | PreQId (_,_,_)  as t
        -> print_preterm out t
  | t   -> fprintf out "(%a)" print_preterm t

let rec print_pat out = function
  | Unknown (_,_)             -> failwith "Not implemented ( _ )."
  | PPattern (_,m,id,pats)    ->
      if List.length pats = 0 then
        fprintf out " %a" print_id (m,id)
      else
        begin
          fprintf out " (%a" print_id (m,id) ;
          List.iter (fprintf out " %a" print_pat) pats;
          fprintf out ")"
        end

let print_ldec out (_,id,ty) =
  fprintf out "%s:%a" (string_of_ident id) print_preterm ty

let mk_rule out (env,(_,id,pats),ri:prule) =
  fprintf out "_ : [" ; (*FIXME*)
  ( match env with
      | []      -> ()
      | e::env' ->
          begin
            print_ldec out e ;
            List.iter (fun e' -> fprintf out ",%a" print_ldec e' ) env'
          end
  ) ;
  fprintf out "] %s" (string_of_ident id) ;
  List.iter (fprintf out " %a" print_pat) pats ;
  fprintf out " --> %a" print_preterm ri ;
  fprintf out "\n\t%c role RewriteRule\n\t%c\n\n" ascii31 ascii30

(* *** *)

let mk_prelude lc m =
  fprintf !Global.out "namespace %s %c\n\ntheory FILENAME =\n\n"
    (string_of_ident m) ascii29

let mk_require _ _ = ()

let mk_declaration _ id pty =
  fprintf !Global.out "%s : %a %c\n\n"
    (string_of_ident id) print_preterm pty ascii30

let mk_definition _ id pty_opt pte =
  match pty_opt with
    | None      -> failwith "Not implemented (definition without type)."
    | Some pty  ->
        begin
          fprintf !Global.out "%s : %a %c\n = %a %c\n" (string_of_ident id)
            print_preterm pty ascii31
            print_preterm pte ascii30
        end

let mk_opaque lc id pty_opt pte =
  failwith "Not implemented (opaque definitions)."

let mk_command _ _ _ = ()

let mk_rules = List.iter (mk_rule !Global.out)

let mk_ending _  = fprintf !Global.out "\n%c" ascii29
