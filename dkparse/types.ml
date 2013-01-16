type loc = int*int
type id  = string

type term =
  | Kind
  | Type
  | EVar of string
  | GVar of string
  | Var of string
  | App of term * term
  | Lam of id * term option * term
  | Pi  of id option * term * term

type parsing_error = 
  | LexerError                  of string*loc
  | ParserError                 of string*loc
  | ConstructorMismatch         of id*loc*id*loc
  | AlreadyDefinedId            of id*loc
  | ScopeError                  of id*loc

exception ParsingError          of parsing_error

type pattern = 
        | Id of id
        | Pat of id*term array*pattern array

type env = ((id*loc)*term) list

type rule  = loc * env * term array * pattern array * term (* loc * context * dots patterns * patterns --> term *)

type rules = id * rule list                                 (* constructeur * dot arity * arity * rules *)

type occ = int list

type pMat = { p:pattern array array ; a:(env*term) array ; loc:occ array ; }


