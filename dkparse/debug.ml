
open Types

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let rec string_of_term = function
  | Type                -> "Type"
  | GVar (m,v)          -> m^"."^v
  | Var  v              -> v
  | App (t1,t2)         -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | Lam(v,None,te)      -> "(\\" ^ v ^ " -> " ^ string_of_term te ^ ")"
  | Lam(v,Some ty,te)   -> "(\\" ^ v ^ " : " ^ string_of_term ty ^ " -> " ^ string_of_term te ^ ")"
  | Pi(None,ty,te)      -> "([_:"^    string_of_term ty^"] "^string_of_term te^")"
  | Pi(Some v,ty,te)    -> "(["^v^":"^string_of_term ty^"] "^string_of_term te^")"

let string_of_parsing_error = function
  | LexerError         (tk,loc)         -> string_of_loc loc ^ " Lexing Error near '" ^ tk ^ "'."
  | ParserError        (tk,loc)         -> string_of_loc loc ^ " Parsing Error near '" ^ tk ^ "'."
  | ConstructorMismatch (i1,l1,i2,l2)   -> string_of_loc l2  ^ " Constructor mismatch '" ^ i1 ^ "'!='" ^ i2 ^ "'."
  | AlreadyDefinedId    (id,loc)        -> string_of_loc loc ^ " Already defined constructor: '" ^ id ^ "'."
  | ScopeError          (id,loc)        -> string_of_loc loc ^ " Scope Error: '" ^ id ^ "'."
  | UnknownModule       (id,loc)        -> string_of_loc loc ^ " Missing dependency: '" ^ id ^ "'."

let string_of_inference_error = function
  | CannotFindExternalModule m          -> "Cannot find module "^m^"."
  | CannotFindExternalSymbol (m,v)      -> "Cannot find symbol "^m^"."^v^"." 

let string_of_inference_error0 = function
  | NotAType1 t         -> string_of_term t ^ " is not a type."
  | NotAType2 (_,_,_)   -> "NotAType2"
  | ConvPi1             -> "Conv1"
  | ConvPi2             -> "Conv2"
  | TypeInf0            -> "Inf0"
  | TypeInf1            -> "Inf1"
  | TypeInf2            -> "Inf2"
  | TypeInf3            -> "INf3"
  | TypeInf4            -> "Inf4"
  | TypeCheckDef        -> "Def"
  | TypeCheckRule       -> "Rule"

let string_of_internal_error = function
  | ContextError (v,ctx)        -> "Can't find variable " ^ v ^ " in { " ^(String.concat " , " (List.map fst ctx) )^ " }"
  | IsAlias1 (m,v)              -> "IsAlias1 " ^ m ^ " " ^ v
  | IsAlias2 (m,v)              -> "IsAlias2 " ^ m ^ " " ^ v

let concat sep t = 
  Array.fold_left ( fun acc s -> if acc="" then s else ( acc^sep^s ) ) "" t
 
let rec string_of_pattern = function
  | Joker               -> "_"
  | Id v                -> "(Id "^v^")"
  | Pat ((_,c),ds,ps)   -> "("^c^" [|"^(concat ";" (Array.map string_of_term ds))^"|] [|"^(concat ";" (Array.map string_of_pattern ps))^"|])"

let print_pMat pm =
  prerr_string "\n ###> \n";
  Array.iteri ( 
    fun i li -> 
      prerr_string "( " ;
      Array.iter (fun p -> Printf.eprintf "%s\t" (string_of_pattern p)) li ;
      prerr_string (" [ "^string_of_term (snd pm.a.(i))^" ]\n")
  ) pm.p ;
  prerr_string "<###\n"

