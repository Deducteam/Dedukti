
open Types

(* to_string functions *)

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let rec string_of_term : term -> string = function
  | Kind        -> "Kind"
  | Type        -> "Type"
  | DB  n       -> string_of_int n
  | GVar (m,v)  -> m^"."^v
  | App args    -> "(" ^ String.concat " " (List.map string_of_term args) ^ ")"
  | Lam (a,f)   -> "(\ "^string_of_term a^" => "^string_of_term f^")"
  | Pi  (a,b)   -> "(" ^ string_of_term a^" -> "^string_of_term b ^")" 

let rec string_of_pat2 = function
  | Joker               -> "_"
  | Var v               -> v
  | Pattern ((m,v),arr) -> "("^m^"."^v^" "^String.concat " " (List.map string_of_pat2 (Array.to_list arr))^")"

(* --- Error messages --- *)

let err_conv te exp inf =  
  "Error while typing "^string_of_term te ^".\nExpected type: "^string_of_term exp^".\nInferred type: "^string_of_term inf^".\n"

let err_sort te ty =
  "Error while typing "^string_of_term te ^".\n Expected type: Type or Kind.\nInferred type: "^string_of_term ty^".\n"

let err_topsort te = 
  "Error while typing "^string_of_term te ^".\n Expected type: anything but Kind.\nInferred type: Kind.\n"

let err_prod te ty = 
  "Error while typing "^string_of_term te ^".\n Product expected.\nInferred type: "^string_of_term ty^".\n"

(* Debug *)

let dump_gdt id g = 
  let rec aux = function
    | Leaf te                     -> Global.print_v ("Leaf : "^string_of_term te^"\n")
    | Switch (c,cases,def)        ->
        begin
          Global.print_v ("Switch ( "^string_of_int c ^") [\n");
          List.iter (fun ((m,v),g) -> Global.print_v ("Case "^m^"."^v^": ") ; aux g ) cases ;
          (match def with
             | None       -> ()
             | Some g     -> (Global.print_v "Def: " ; aux g) ) ;
          Global.print_v ("]\n")
        end
  in
    Global.print_v (" --------> GDT FOR "^id^"\n");
    aux g;
    Global.print_v " <-------- \n"

let dump_pMat id pm =
  let aux l = 
    Global.print_v " [ ] " ;
    Array.iter (fun p -> Global.print_v (string_of_pat2 p^"\t")) l.li;
    Global.print_v (" --> "^string_of_term l.te^"\n")
  in
    Global.print_v (" --------> PMAT FOR "^id^"\n");
    Array.iter aux pm ;
    Global.print_v " <-------- \n"

let dump_state (k,e,t,s) =
  Global.print ("k = "^string_of_int k^"\n");
  Global.print ("t = "^string_of_term t^"\n");
  Global.print "e = [";
  List.iter (fun u -> Global.print (" ("^string_of_term (Lazy.force u)^")")) e ;
  Global.print " ]\ns = [";
  List.iter (fun (_,_,u,_) -> Global.print (" {{ "^string_of_term u^" }}")) s ;
  Global.print " ]\n"


