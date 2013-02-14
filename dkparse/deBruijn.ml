type term =
  | Kind
  | Type
  | Ext of string
  | Par of string
  | Var of int
  | App of term * term
  | Lam of term option * term
  | Pi  of term * term

let rec string_of_term = function
  | Kind                -> "Kind"
  | Type                -> "Type"
  | Ext v               -> v
  | Par v               -> v
  | Var i               -> string_of_int i
  | App (t1,t2)         -> string_of_term t1 ^ " " ^ string_of_term t2
  | Lam(None,te)        -> "(fun " ^ string_of_term te ^ ")"
  | Lam(Some ty,te)     -> "(fun { " ^ string_of_term ty ^ " } " ^ string_of_term te ^ ")"
  | Pi(ty,te)           -> "(["^string_of_term ty^"] "^string_of_term te^")"

(*
type expr = 
  | Const of string
  | Var of int 
  | Lambda of expr
  | App of expr * expr
 
let rec print = function
  | Const s     -> print_string s
  | Var i       -> print_int i
  | Lambda e    -> ( print_string "(fun " ; print e ; print_string ")" )
  | App (f,a)   -> ( print f ; print_string " " ; print a )
 *)

let printn t = print_string (string_of_term t) ; print_newline ()

let shift d t =
  let rec shift0 c = function 
    | Var i             -> if i >= c then Var (i+d) else Var i
    | App (t1,t2)       -> App ( shift0 c t1 , shift0 c t2 )
    | Lam (None,t0)     -> Lam ( None , shift0 (c+1) t0 )
    | Lam (Some dom,t0) -> Lam ( Some (shift0 c dom) , shift0 (c+1) t0 )
    | Pi (ty,te)        -> Pi  ( shift0 c ty , shift0 (c+1) te )
    | t                 -> t 
  in
    shift0 0 t

let subst j s t =
  let rec subst0 c = function
    | Var i             -> if i=j+c then shift c s else Var i
    | App (t1,t2)       -> App ( subst0 c t1 , subst0 c t2 )
    | Lam (None,t0)     -> Lam ( None , subst0 (c+1) t0 )
    | Lam (Some dom,t0) -> Lam ( Some (subst0 c dom) , subst0 (c+1) t0 )
    | Pi (ty,te)        -> Pi  ( subst0 c ty , subst0 (c+1) te )
    | t                 -> t 
  in
    subst0 0 t

let apply = function
  | ( Lam (_,f) , a )   -> shift (-1) (subst 0 (shift 1 a) f)
  | _                   -> assert false

let f = Lam (None,Lam (None,App (Var 0,Var 1)))
let a = Par "c"

let _ =
  printn f ;
  printn a ;
  printn (apply (f,a))
