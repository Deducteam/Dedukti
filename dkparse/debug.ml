
open Types

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let string_of_id (m,id) = m ^ "." ^ id

let string_of_perr = function
  | LexerError         (tk,loc)         -> string_of_loc loc ^ " Lexing Error near '" ^ tk ^ "'."
  | ParserError        (tk,loc)         -> string_of_loc loc ^ " Parsing Error near '" ^ tk ^ "'."
  | ConstructorMismatch (i1,l1,i2,l2)   -> string_of_loc l2  ^ " Constructor mismatch '" ^ i1 ^ "'!='" ^ i2 ^ "'."
  | AlreadyDefinedId    (id,loc)        -> string_of_loc loc ^ " Already defined constructor: '" ^ string_of_id id ^ "'."
  | ScopeError          (id,loc)        -> string_of_loc loc ^ " Scope Error: '" ^ string_of_id id ^ "'."
  | UnknownModule       (id,loc)        -> string_of_loc loc ^ " Missing dependency: '" ^ id ^ "'."

let rec string_of_term = function
  | Kind                -> "Kind"
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
 
let rec string_of_pattern = function
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

