
open Types

let extract_msg err = (*FIXME fragile*)
  let reg = Str.regexp_string ":" in
  try 
    let p1 = Str.search_forward reg err 0               in 
    let p2 = Str.search_forward reg err (p1+1) + 2      in 
      String.sub err p2 (String.length err - p2)
  with
    | Not_found -> assert false

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let string_of_perr = function
  | LexerError         (tk,loc)         -> string_of_loc loc ^ " Lexing Error near '" ^ tk ^ "'."
  | ParserError        (tk,loc)         -> string_of_loc loc ^ " Parsing Error near '" ^ tk ^ "'."
  | ConstructorMismatch (i1,l1,i2,l2)   -> string_of_loc l2  ^ " Constructor mismatch '" ^ i1 ^ "'!='" ^ i2 ^ "'."
  | AlreadyDefinedId    (id,loc)        -> string_of_loc loc ^ " Already defined constructor: '" ^ id ^ "'."
  | ScopeError          (id,loc)        -> string_of_loc loc ^ " Scope Error: '" ^ id ^ "'."

let rec string_of_term = function
  | Kind                -> "Kind"
  | Type                -> "Type"
  | EVar v              -> v
  | GVar v              -> v
  | Var  v              -> v
  | App (t1,t2)         -> string_of_term t1 ^ " " ^ string_of_term t2
  | Lam(v,None,te)      -> "(\\" ^ v ^ " -> " ^ string_of_term te ^ ")"
  | Lam(v,Some ty,te)   -> "(\\" ^ v ^ " : " ^ string_of_term ty ^ " -> " ^ string_of_term te ^ ")"
  | Pi(None,ty,te)      -> "([_:"^    string_of_term ty^"] "^string_of_term te^")"
  | Pi(Some v,ty,te)    -> "(["^v^":"^string_of_term ty^"] "^string_of_term te^")"

let string_of_lerr = function
  | LuaTypeCheckingFailed (te,ty,err)   -> "Type Checking Failed !\n[Term] " ^ (string_of_term te) ^ "\n[Type] " ^ (string_of_term ty) ^ "\n" ^ (extract_msg err)
  | LuaRuleCheckingFailed (id,err)      -> "Type Checking Failed !\n[Rule] " ^ id ^ "\n" ^ (extract_msg err)
  | LuaRequireFailed err                -> "Unable to load module '" ^ err ^ "'."

let concat sep t = 
  Array.fold_left ( fun acc s -> if acc="" then s else ( acc^sep^s ) ) "" t
 
let rec string_of_pattern = function
  | Id v                -> "(Id "^v^")"
  | Pat (c,ds,ps)       -> "("^c^" [|"^(concat ";" (Array.map string_of_term ds))^"|] [|"^(concat ";" (Array.map string_of_pattern ps))^"|])"

let print_pMat pm =
  prerr_string "\n ###> \n";
  Array.iteri ( 
    fun i li -> 
      prerr_string "( " ;
      Array.iter (fun p -> Printf.eprintf "%s\t" (string_of_pattern p)) li ;
      prerr_string (" [ "^string_of_term (snd pm.a.(i))^" ]\n")
  ) pm.p ;
  prerr_string "<###\n"

