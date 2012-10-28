
open Types

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let string_of_err = function
  | LexingError         (tk,loc)        -> string_of_loc loc ^ " Lexing Error near '" ^ tk ^ "'."
  | ParsingError        (tk,loc)        -> string_of_loc loc ^ " Parsing Error near '" ^ tk ^ "'."
  | ConstructorMismatch (i1,l1,i2,l2)   -> string_of_loc l2  ^ "Constructor mismatch '" ^ i1 ^ "'!='" ^ i2 ^ "'."
  | AlreadyDefinedId    (id,loc)        -> string_of_loc loc ^ " Already defined constructor: '" ^ id ^ "'."
  | ScopeError          (id,loc)        -> string_of_loc loc ^ " Scope Error: '" ^ id ^ "'."

let rec string_of_term = function
  | Type                -> "Type"
  | EVar v              -> v
  | GVar v              -> v
  | Var  v              -> v
  | App (t1,t2)         -> string_of_term t1 ^ " " ^ string_of_term t2
  | Lam(v,None,te)      -> "(\\" ^ v ^ " -> " ^ string_of_term te ^ ")"
  | Lam(v,Some ty,te)   -> "(\\" ^ v ^ " : " ^ string_of_term ty ^ " -> " ^ string_of_term te ^ ")"
  | Pi(None,ty,te)      -> "([_:"^    string_of_term ty^"] "^string_of_term te^")"
  | Pi(Some v,ty,te)    -> "(["^v^":"^string_of_term ty^"] "^string_of_term te^")"

let concat sep t = 
  Array.fold_left ( fun acc s -> if acc="" then s else ( acc^sep^s ) ) "" t
 
let rec string_of_pattern = function
  | Id v                -> "(Id "^v^")"
  | Pat (c,ds,ps)       -> "("^c^" [|"^(concat ";" (Array.map string_of_term ds))^"|] [|"^(concat ";" (Array.map string_of_pattern ps))^"|])"

let print_pMat pm =
  Array.iter ( 
    fun li -> 
      print_string "( " ;
      Array.iter (fun p -> Printf.printf "%s\t" (string_of_pattern p)) li ;
      print_string ")\n"
  ) pm.p

(*
let get_arities (_,dots,pats,_) = (Array.length dots,Array.length pats)

let isValideRules (_,lst) = 
  let rec aux ar = function
    | []        -> true
    | x::lst'   ->
        if get_arities x = ar then aux ar lst'
        else false
  in
    match lst with
      | []        -> false
      | x::lst'   -> aux (get_arities x) lst'

let isValidpMat str pm = 
  let li1 = Array.length pm.p   in
    if li1 = 0 then failwith (str^": isValidpMat li=0")
    else
      let li2 = Array.length pm.a   in
        if li1!=li2 then failwith (str^": isValidpMat li1!=li2")
        else
          let co1 = Array.length pm.p.(0)       in
            if co1=0 then failwith (str^": isValidpMat co=0")
            else
              let co2 = Array.length pm.loc         in
                if co1!=co2 then failwith (str^": isValidpMat co1!=co2")
                else 
                  Array.iter (function | [] -> failwith (str^": isValidpMat loc=[]") | _ -> ()) pm.loc
 *)
  

    
