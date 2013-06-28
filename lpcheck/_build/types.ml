
(* Lexing *)

type loc  = int*int
type lvar = loc*string
type lid  = loc*string*string

(* Parsing *)

type pterm =
  | PType
  | PId  of loc*string*string
  | PApp of pterm * pterm
  | PLam of (loc*string) * pterm option * pterm
  | PPi  of (loc*string) option * pterm * pterm

type pattern = 
  | Pat of (loc*string*string) * pterm array * pattern array

type top_pattern = (loc*string) * pterm array * pattern array 

type context = ( (loc*string) * pterm ) list

type rule  = context * top_pattern * pterm (* [ env ] top_pattern --> term *)

type parser_error = 
  | LexerError                  of string * loc
  | ParsingError                of string * loc
  | SetNameError                of string * loc              

exception ParserError          of parser_error
exception End_of_file_in_comment

(* Typing *)

type id  = string*string

type term = 
  | Type                                (* Type *)
  | GVar of id                          (* Global variable *)
  | DB   of int                         (* deBruijn *)
  | App  of term*term                   (* Application *)
  | Lam  of term*term                   (* Lambda abstraction *)
  | Pi   of term*term                   (* Pi abstraction *)

type code =
  | C_Type
  | C_App of id*code list
  | C_Lam of code*(code->code)
  | C_Pi  of code*(code->code)
  | C_Var of int

type rw_env = (string*term) list
type nfun = code array -> code option

type typing_error =
  | UndefinedSymbol of id
  | SortExpected of term
  | TopSortError
  | TypeExpected of term option
  | CannotConvert of term option*term
exception TypingError of typing_error
                                       
(* Pattern matching *)

type pattern2 =
  | Joker 
  | Var of string
  | Pattern of id * pattern2 array

type loc_type = (string option) array

type pMat = (pattern2 array*term) array 

type gdt = 
  | Leaf     of (string*int) list * term 
  | Switch   of int * (id*gdt) list * gdt option
