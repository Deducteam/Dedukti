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

type lua_error =
  | LuaTypeCheckingFailed       of term*term*string
  | LuaRuleCheckingFailed       of id*string
  | LuaRequireFailed            of string

exception ParsingError          of parsing_error
exception TypeCheckingError     of lua_error

type pattern = 
        | Id of id
        | Pat of id*term array*pattern array

type env = (id*term) list

type rule  = env * term array * pattern array * term                    (* context * dots patterns * patterns --> term *)

type rules = id * rule list                                             (* constructeur * dot arity * arity * rules *)

type occ = int list

type pMat = { p:pattern array array ; a:(env*term) array ; loc:occ array ; }


