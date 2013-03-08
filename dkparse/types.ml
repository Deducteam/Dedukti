type loc = int*int
type id  = string*string
type var  = string

type term =
  | Kind
  | Type
  | GVar of id
  | Var of string
  | App of term * term
  | Lam of var * term option * term
  | Pi  of var option * term * term

type parsing_error = 
  | LexerError                  of string*loc
  | ParserError                 of string*loc
  | ConstructorMismatch         of var*loc*var*loc
  | AlreadyDefinedId            of id*loc
  | ScopeError                  of id*loc
  | UnknownModule               of string*loc

exception ParsingError          of parsing_error
exception End_of_file_in_comment

type pattern = 
  | Joker
  | Id of var
  | Pat of id*term array*pattern array

type env = ((var*loc)*term) list

type rule  = loc * env * term array * pattern array * term (* loc * context * dots patterns * patterns --> term *)

type rules = string * rule list                                 (* constructeur * dot arity * arity * rules *)

type occ = int list

type pMat = { p:pattern array array ; a:(env*term) array ; loc:occ array ; }


