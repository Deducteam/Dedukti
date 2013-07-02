
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

let string_of_terr = function
  | SortExpected te             -> "Sort expected ("^string_of_term te^")" 
  | TopSortError                -> "Top sort"
  | TypeExpected None           -> "Type expected (Kind)"
  | TypeExpected (Some te)      -> "Type expected ("^string_of_term te^")"
  | CannotConvert (None,ty)     -> "Cannot convert Kind with "^string_of_term ty
  | CannotConvert (Some ty1,ty2)-> "Cannot convert "^string_of_term ty1^" with "^string_of_term ty2
  | ProductExpected _           -> "TODO"

let string_of_eerr = function
  | UndefinedSymbol (m,v)       -> "Undefined symbol '" ^ m ^ "." ^ v^ "'"
  | AlreadyDefinedSymbol v      -> "Already defined symbol '" ^ v^ "'"
  | AlreadyOpenedModule m       -> "TODO"
  | FailToOpenModule m          -> "TODO"
  | CannotFindModule m          -> "TODO"

let string_of_perr =  function
  | LexerError          (tk,loc)                -> string_of_loc loc ^ " Lexing Error near '" ^ tk ^ "'."
  | ParsingError        (tk,loc)                -> string_of_loc loc ^ " Parsing Error near '" ^ tk ^ "'."
(*  | SetNameError        (name,loc)              -> string_of_loc loc ^ "
 *  Invalid module name: '" ^ name ^ "'" *)

(*
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

let print_pMat (pm:pMat) =
  Array.iter (
    fun (le,ri) ->
     Array.iter (
       fun p    -> Global.msg ("\t"^string_of_pattern2 p)
     ) le   ;
     Global.msg ("\t"^string_of_term ri^"\n")
  ) pm 

let rec print_gdt = function
  | Leaf (_,te)         -> Global.msg (string_of_term te)
  | Switch (c,lst,def)  -> (
      Global.msg ("CASE args.("^string_of_int c^") {") ;
      List.iter (
        fun ((m,v),g) -> Global.msg ("\n "^m^"."^v^": ") ; print_gdt g 
      ) lst ;
      ( match def with
        | None          -> ()
        | Some g        -> ( Global.msg "\n default: " ; print_gdt g ) ) ;
      Global.msg "}\n"
    )

 *)
