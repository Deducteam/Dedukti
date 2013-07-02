
open Types

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let string_of_id (m,id) = m ^ "." ^ id

let rec string_of_term (te:term) : string =
  match te with
    | Type                -> "Type"
    | GVar (m,v)          -> m^"."^v
    | DB  n               -> string_of_int n
    | App (f,u)           ->  "("^string_of_term f^" "^string_of_term u^")"
    | Lam (a,f)           -> "(\ "^string_of_term a^" => "^string_of_term f^")"
    | Pi  (a,b)           -> "(" ^ string_of_term a^" ->_t "^string_of_term b ^")" 
    | Subst (t,_)         -> "Subst("^string_of_term t^"[...])"

let string_of_terr = function
  | UndefinedSymbol (m,v)       -> "Undefined symbol '" ^ m ^ "." ^ v^ "'"
  | SortExpected te             -> "Sort expected ("^string_of_term te^")" 
  | TopSortError                -> "Top sort"
  | TypeExpected None           -> "Type expected (Kind)"
  | TypeExpected (Some te)      -> "Type expected ("^string_of_term te^")"
  | CannotConvert (None,ty)     -> "Cannot convert Kind with ..."
  | CannotConvert (Some ty1,ty2)-> "Cannot convert "^string_of_term ty1^" with "^string_of_term ty2

let string_of_perr =  function
  | LexerError          (tk,loc)                -> string_of_loc loc ^ " Lexing Error near '" ^ tk ^ "'."
  | ParsingError        (tk,loc)                -> string_of_loc loc ^ " Parsing Error near '" ^ tk ^ "'."
  | SetNameError        (name,loc)              -> string_of_loc loc ^ " Invalid module name: '" ^ name ^ "'"

let nn = ref (-1)
let mk_var _ = incr nn ; C_Var (!nn)

let rec string_of_code = function
  | C_Type                      -> "Type"
  | C_App ((m,v),lst)           -> "("^m^"."^v^string_of_code_lst lst^")"
  | C_Lam (a,te)                -> "(Lam "^string_of_code a^". "^string_of_code (te (mk_var ()))^")"
  | C_Pi  (a,b)                 -> "(Pi "^string_of_code a^". "^string_of_code (b (mk_var ()))^")"
  | C_Var i                     -> "Var"^string_of_int i
and string_of_code_lst : code list -> string = function
  | []          -> ""
  | c::lst      -> ( " "^(string_of_code c)^(string_of_code_lst lst) )

let rec string_of_pattern2 = function
  | Joker                       -> "*"
  | Var v                       -> v
  | Pattern ((m,v),pats)        -> "("^m^"."^v^" ...)"

let print_pMat (pm:pMat) = assert false (*
  Array.iter (
    fun (le,ri) ->
     Array.iter (
       fun p    -> Global.debug ("\t"^string_of_pattern2 p)
     ) le   ;
     Global.debug ("\t"^string_of_term ri^"\n")
  ) pm *)

let rec print_gdt _ = assert false (* function
  | Leaf (_,te)         -> Global.debug (string_of_term te)
  | Switch (c,lst,def)  -> (
      Global.debug ("CASE args.("^string_of_int c^") {") ;
      List.iter (
        fun ((m,v),g) -> Global.debug ("\n "^m^"."^v^": ") ; print_gdt g 
      ) lst ;
      ( match def with
        | None          -> ()
        | Some g        -> ( Global.debug "\n default: " ; print_gdt g ) ) ;
      Global.debug "}\n"
    ) *)

 

(*
let rec string_of_term =  function
  | Type                -> "Type"
  | GVar id             -> string_of_id id
  | Var  v              -> v
  | App (t1,t2)         -> string_of_term t1 ^ " " ^ string_of_term t2
  | Lam(v,None,te)      -> "(\\" ^ v ^ " -> " ^ string_of_term te ^ ")"
  | Lam(v,Some ty,te)   -> "(\\" ^ v ^ " : " ^ string_of_term ty ^ " -> " ^ string_of_term te ^ ")"
  | Pi(None,ty,te)      -> "([_:"^    string_of_term ty^"] "^string_of_term te^")"
  | Pi(Some v,ty,te)    -> "(["^v^":"^string_of_term ty^"] "^string_of_term te^")"

let concat sep t = 
  Array.fold_left ( fun acc s -> if acc="" then s else ( acc^sep^s ) ) "" t
 
let rec string_of_pattern _ = function 
  | Joker               -> "_"
  | Id v               -> "(Id "^ v ^")"
  | Pat (c,ds,ps)       -> "("^string_of_id c^" [|"^(concat ";" (Array.map string_of_term ds))^"|] [|"^(concat ";" (Array.map string_of_pattern ps))^"|])" 

let print_pMat pm =
  prerr_string "\n ###> \n";
  Array.iteri ( 
    fun i li -> 
      prerr_string "( " ;
      Array.iter (fun p -> Printf.eprintf "%s\t" (string_of_pattern p)) li ;
      prerr_string (" [ "^string_of_term (snd pm.a.(i))^" ]\n")
  ) pm.p ;
  prerr_string "<###\n" 
  *)


