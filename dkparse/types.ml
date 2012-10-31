type loc = int*int
type id  = string

type error = 
  | LexingError                 of string*loc
  | ParsingError                of string*loc
  | ConstructorMismatch         of id*loc*id*loc
  | AlreadyDefinedId            of id*loc
  | ScopeError                  of id*loc


exception Error of error

type term =
        | Type
        | EVar of string
        | GVar of string
        | Var of string
        | App of term * term
        | Lam of id * term option * term
        | Pi  of id option * term * term

type pattern = 
        | Id of id
        | Pat of id*term array*pattern array

type env = (id*term) list

type rule  = env * term array * pattern array * term                    (* context * dots patterns * patterns --> term *)

type rules = id * rule list                                             (* constructeur * dot arity * arity * rules *)

type occ = int list

type pMat = { p:pattern array array ; a:(env*term) array ; loc:occ array ; nb_dots:int ; }


