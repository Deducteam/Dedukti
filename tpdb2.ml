open Printf
open Types

let rule_num = ref 0
let get_rule_num _ =
  incr rule_num ; !rule_num

let rec print_arg num = function
  | Var (x,_)           -> fprintf !Global.tpdb_file "%s_%i" (string_of_ident x) num
  | Joker _             -> fprintf !Global.tpdb_file "_" 
  | Pattern (m,c,args)  ->
      if Array.length args = 0 then
        fprintf !Global.tpdb_file "%s.%s" (string_of_ident m) (string_of_ident c) 
      else
        begin 
          Array.iter (fun _ -> fprintf !Global.tpdb_file "#APP(" ) args ;
          fprintf !Global.tpdb_file "%s.%s" (string_of_ident m) (string_of_ident c) ;
          Array.iter (fun a -> ( fprintf !Global.tpdb_file "," ; print_arg num a ; fprintf !Global.tpdb_file ")" )) args
        end

let rec print_term num i = function
  | Kind | Type | Meta _        -> assert false
  | Const (m,c)                 -> fprintf !Global.tpdb_file "%s.%s" (string_of_ident m) (string_of_ident c) 
  | Lam (_,ty,te)               -> 
      begin
        fprintf !Global.tpdb_file "#LAMBDA(" ;
        print_term num i ty ;
        fprintf !Global.tpdb_file "," ;
        print_term num (i+1) te ;
        fprintf !Global.tpdb_file ")" 
      end
  | Pi (_,a,b)                  -> 
      begin
        fprintf !Global.tpdb_file "#PI(" ;
        print_term num i a ;
        fprintf !Global.tpdb_file "," ;
        print_term num (i+1) b ;
        fprintf !Global.tpdb_file ")" 
      end
  | DB (x,n)                    -> 
      begin
        if n>=i then
          fprintf !Global.tpdb_file "%s_%i" (string_of_ident x) num
        else 
          fprintf !Global.tpdb_file "#DB_%i_%i" num (i-n-1)
      end
  | App ([]|[_])                -> assert false 
  | App (f::lst)                -> 
      begin 
        List.iter (fun _ -> fprintf !Global.tpdb_file "#APP(" ) lst;
        print_term num i f ;
        List.iter (fun a -> ( fprintf !Global.tpdb_file "," ; print_term num i a ; fprintf !Global.tpdb_file ")" )) lst
      end

let print_rule (_,ctx,id,args,te:rule) =
  let num = get_rule_num () in
  (* VAR *)
    ( match ctx with
      | []      -> ()
      | _       -> 
          begin
            fprintf !Global.tpdb_file "(VAR";
            List.iter (fun (x,_) -> fprintf !Global.tpdb_file " %s_%i" (string_of_ident x) num ) ctx ;
            fprintf !Global.tpdb_file ")\n";
          end ) ;
    (* RULES *)
(*FIXME*)
    Global.eprint ( 
      (Pp.string_of_pattern (Pattern (!Global.name,id,args)))
      ^ " --> " ^
        (Pp.string_of_term te)
    ) ;
    
    fprintf !Global.tpdb_file "(RULES ";
    print_arg num (Pattern (!Global.name,id,args)) ;
    fprintf !Global.tpdb_file " -> ";
    print_term num 0 te ;
    fprintf !Global.tpdb_file ")\n\n"
(*
let print_rules (rs:rule list) = List.iter print_rule rs
 *)
