type action =
  | PrintDedukti
  | PrintMMT
  | LuaGeneration

type loc = int*int
type id  = loc*string*string
type var = loc*string

(* Parsing *)

type pterm =
  | P_Type
  | P_Id  of var
  | P_Qid of id
  | P_App of pterm * pterm
  | P_Lam of var * pterm option * pterm
  | P_Pi  of var option * pterm * pterm

type ppat = 
  | Pat_Id of var
  | Pat_Pa of id * pterm array * ppat array

type top_ppat = var * pterm array * ppat array

type prule  = ( var * pterm ) list * top_ppat * pterm (* [ env ] top_pattern --> pterm *)

type definition =
  | Def         of var * pterm * pterm    (* id : ty := te *)
  | Opaque      of var * pterm * pterm    (* { id } : ty := te *)
  | Anon        of loc * pterm * pterm    (* _ : ty := te *)

(* Scoping *)

type term =
  | Type
  | LVar of var
  | GVar of id
  | App  of term * term
  | Lam  of var * term option * term
  | Pi   of var option * term * term

type pat =
  | Joker 
  | Var of var
  | Pat of id * term array * pat array

type top_pat = var * term array * pat array

type rule  = ( var * term ) list * top_pat * term (* [ env ] top_pattern --> term *)

(* Pattern matching *)

type occ = int list
type gdt = 
  | LeafNil 
  | Leaf1 of pat array*occ array*term
  | Leaf2 of term
  | Node  of (occ*id*gdt) list*gdt

(* Errors *)

type option_error =
  | SetLuaPathError     of string * string        (* (path,err) *)
  | SetOutError         of string * string        (* (file*err) *) 

type parser_error = 
  | LexerError                  of string * loc
  | ParsingError                of string * loc
  | SetNameError                of string * loc              (* invalid name *) 
  | ConstructorMismatch         of var * var
  | AlreadyDefinedId            of var
  | ScopeError                  of var

exception ParserError          of parser_error
exception OptionError          of option_error
exception End_of_file_in_comment
