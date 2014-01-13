open Printf
open Types

let rule_num = ref 0
let get_rule_num _ =
  incr rule_num ; !rule_num

let rec print_arg num = function
  | Var (x,_)           -> fprintf !Global.tpdb_file "%s_%i" (string_of_ident x) num 
  | Joker _             -> fprintf !Global.tpdb_file "_" 
  | Pattern (m,c,args)  ->
      begin
        fprintf !Global.tpdb_file "%s.%s" (string_of_ident m) (string_of_ident c) ;
        if Array.length args > 0 then
          begin 
            fprintf !Global.tpdb_file "(";
            print_arg num args.(0) ;
            Array.iteri (fun i arg -> if i!=0 then ( fprintf !Global.tpdb_file "," ; print_arg num arg) ) args ;
            fprintf !Global.tpdb_file ")"
          end
      end

let rec print_term num i = function
  | Kind | Type | Meta _        -> assert false
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
        else (
          assert (i-n-1 < 10); (*FIXME*)
          fprintf !Global.tpdb_file "#DB_%i" (i-n-1) )
      end
  | Const (m,c)                 -> fprintf !Global.tpdb_file "%s.%s" (string_of_ident m) (string_of_ident c) 
  | App ((Const (m,c))::a1::l)  -> 
      begin
        fprintf !Global.tpdb_file "%s.%s(" (string_of_ident m) (string_of_ident c) ;
        print_term num i a1 ;
        List.iter (fun t -> fprintf !Global.tpdb_file "," ; print_term num i t ) l ;
        fprintf !Global.tpdb_file ")";
      end
  | App (_::a1::l)              -> failwith "[TPDB Export] Beta-redex or variable application in righthand side of a rule."
  | App _                       -> assert false 


let print_rule (_,ctx,id,args,te:rule) =
  let num  = get_rule_num () in
  (* VAR *)
    ( match ctx with
      | []      -> ()
      | _       -> 
          begin
            fprintf !Global.tpdb_file "(VAR";
            List.iter (fun (x,_) -> fprintf !Global.tpdb_file " %s_%i" (string_of_ident x) num) ctx ;
            fprintf !Global.tpdb_file ")\n";
          end ) ;
    (* RULES *)
    fprintf !Global.tpdb_file "(RULES ";
    print_arg num (Pattern (!Global.name,id,args)) ;
    fprintf !Global.tpdb_file " -> ";
    print_term num 0 te ;
    fprintf !Global.tpdb_file ")\n\n"

let print_rules (rs:rule list) = List.iter print_rule rs
