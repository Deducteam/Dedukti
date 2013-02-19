type loc = int*int
type id  = string*string
type var = string

type term =
  | Type
  | GVar of id
  | Var of var
  | App of term * term
  | Lam of var * term option * term
  | Pi  of var option * term * term

type parsing_error = 
  | LexerError                  of string*loc
  | ParserError                 of string*loc
  | ConstructorMismatch         of string*loc*string*loc
  | AlreadyDefinedId            of string*loc
  | ScopeError                  of string*loc
  | UnknownModule               of string*loc

type classifier = 
  | CTypeOf     of id
  | CSub        of term
  | CPi         of var*term*classifier
  | CDepType    of int*term
                        (* and cterm = (var*classifier) list*term*)

type inference_error =
  | CannotFindExternalModule of string
  | CannotFindExternalSymbol of id

type inference_error0 = (*FIXME*)
  | NotAType1 of term
  | NotAType2 of var*term*classifier
  | ConvPi1
  | ConvPi2
  | TypeInf0
  | TypeInf1
  | TypeInf2
  | TypeInf3
  | TypeInf4
  | TypeCheckDef
  | TypeCheckRule

type internal_error =
  | ContextError of var*(var*classifier) list
  | IsAlias1 of id
  | IsAlias2 of id

exception TypeSynthError of inference_error
exception ParsingError of parsing_error
exception InternalError of internal_error

type pattern = 
  | Joker
  | Id of var
  | Pat of id*term array*pattern array

type env = ((var*loc)*term) list

type rule  = loc * env * term array * pattern array * term (* loc * context * dots patterns * patterns --> term *)

type rules = id * rule list                                 (* constructeur * dot arity * arity * rules *)

type occ = int list

type pMat = { p:pattern array array ; a:(env*term) array ; loc:occ array ; }


