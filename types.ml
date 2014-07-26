
(* *** Identifiers (hashconsed strings) *** *)

type ident = string
let string_of_ident s = s
let ident_eq s1 s2 = s1==s2 || s1=s2
let pp_ident = output_string

module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash       = WS.create 251
let hstring     = WS.merge shash
let empty       = hstring ""

(* *** Localization *** *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (l,c)
let of_loc l = l

(* *** Parsing *** *)

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID         of ( loc * ident * ident )
  | NAME        of ( loc * ident )
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW
  | WHNF        of loc
  | HNF         of loc
  | SNF         of loc
  | STEP        of loc
  | INFER       of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | OTHER       of ( loc * string )
  | CHAR        of ( loc * char )
  | STRING      of ( loc * string )
  | NUM         of ( loc * string )

exception EndOfFile

(* *** Pseudo Terms *** *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of (loc*ident) option * preterm * preterm
  | PreChar of loc * char
  | PreStr  of loc * string
  | PreNum  of loc * string

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list

type ptop = loc * ident * prepattern list

let mk_pre_type lc          = PreType lc
let mk_pre_id lc id         = PreId (lc,id)
let mk_pre_qid lc md id     = PreQId (lc,md,id)
let mk_pre_lam lc x ty te   = PreLam (lc,x,ty,te)
let mk_pre_arrow a b        = PrePi (None,a,b)
let mk_pre_pi lc x a b      = PrePi (Some(lc,x),a,b)
let mk_pre_char lc c        = PreChar (lc, c)
let mk_pre_string lc s      = PreStr (lc, s)
let mk_pre_num lc s         = PreNum (lc, s)
let mk_pre_app              = function
  | []                  -> assert false
  | [t]                 -> t
  | (PreApp l1)::l2      -> PreApp (l1@l2)
  | lst -> PreApp lst

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = pcontext * ptop * preterm

let rec get_loc = function
  | PreType l | PreId (l,_) | PreQId (l,_,_)
  | PreLam  (l,_,_,_) | PrePi   (Some(l,_),_,_)
  | PreStr (l,_) | PreChar (l,_) | PreNum (l,_) -> l
  | PrePi   (None,f,_) | PreApp (f::_)          -> get_loc f
  | PreApp _                                    -> assert false

(* *** Terms *** *)

type term =
  | Kind                   (* Kind *)
  | Type                   (* Type *)
  | DB     of ident*int    (* deBruijn *)
  | Const  of ident*ident  (* Global variable *)
  | App    of term list    (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam    of ident*term*term            (* Lambda abstraction *)
  | Pi     of ident option*term*term     (* Pi abstraction *)
  | Meta   of int
  | Char   of char
  | Str    of string
  | Num    of string
  | GConst of ident                      (* Global constant *)

let mk_Kind             = Kind
let mk_Type             = Type
let mk_DB x n           = DB (x,n)
let mk_Const m v        = Const (m,v)
let mk_Lam x a b        = Lam (x,a,b)
let mk_Pi x a b         = Pi (x,a,b)
let mk_Meta n           = Meta n
let mk_Char c           = Char c
let mk_Str s            = Str s
let mk_Num s            = Num s
let mk_GConst v         = GConst v

(* Constants *)
let mk_num_type : term = mk_GConst (hstring "nat")
let mk_0 = mk_GConst (hstring "0")
let mk_S = mk_GConst (hstring "S")

let mk_char_type : term = mk_GConst (hstring "char")
let mk_string_type : term = mk_GConst (hstring "string")
let mk_str_nil  = mk_GConst (hstring "\"\"")
let mk_str_cons = mk_GConst (hstring "string_cons")

let const_env = [
  hstring "nat", mk_Type;
  hstring "0", mk_num_type;
  hstring "S", mk_Pi None mk_num_type mk_num_type;
  hstring "char", mk_Type;
  hstring "string", mk_Type;
  hstring "\"\"", mk_string_type;
  hstring "string_cons", mk_Pi None mk_char_type (mk_Pi None mk_string_type mk_string_type);
]

let mk_App              = function
  | [] | [_] -> assert false
  | (App l1)::l2 -> App (l1@l2)
  | lst -> App lst

let cpt = ref (-1)
let mk_Unique _ =
  incr cpt ;
  Const ( empty , hstring (string_of_int !cpt) )

let rec unsugar_nat = function
  | 0 -> mk_0
  | n -> App [mk_S; unsugar_nat (n-1)]

let rec unsugar_str = function
  | "" -> mk_str_nil
  | s -> App [mk_str_cons; mk_Char s.[0]; unsugar_str (String.sub s 1 ((String.length s) -1))]

let unsugar = function
  | Num s -> unsugar_nat (int_of_string s)
  | Str s -> unsugar_str s
  | t -> t

let rec term_eq t1 t2 =
  (* t1 == t2 || *)
  match unsugar t1, unsugar t2 with
    | Kind, Kind | Type , Type          -> true
    | DB (_,n), DB (_,n')
    | Meta n, Meta n'                   -> n=n'
    | Const (m,v), Const (m',v')        -> ident_eq v v' && ident_eq m m'
    | App l, App l'                     -> ( try List.for_all2 term_eq l l'
                                             with _ -> false )
    | Lam (_,a,b), Lam (_,a',b')
    | Pi (_,a,b), Pi (_,a',b')          -> term_eq a a' && term_eq b b'
    | Str s, Str s'                     -> s = s'
    | Num s, Num s'                     -> int_of_string s = int_of_string s'
    | _, _                              -> false

(* *** Rewrite Rules *** *)

type pattern =
  | Var         of ident*int
  | Brackets    of term
  | Pattern     of ident*ident*pattern array

let rec term_of_pattern = function
  | Var (id,n)                  -> DB (id,n)
  | Brackets t                  -> t
  | Pattern (md,id,args)        ->
      let c = Const (md,id) in
        if Array.length args = 0 then c
        else mk_App ( c :: (Array.to_list (Array.map term_of_pattern args)) )

type top = ident*pattern array
type context = ( ident * term ) list

type rule = {
        l:loc;
        ctx:context;
        id:ident;
        args:pattern array;
        rhs:term; }

type rule2 =
    { loc:loc ; pats:pattern array ; right:term ;
      constraints:(term*term) list ; env_size:int ; }

type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of (term*term) list * term * dtree option

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule2 list*int*dtree

(* Misc *)

type yes_no_maybe = Yes | No | Maybe
type 'a option2 = None2 | DontKnow | Some2 of 'a
type ('a,'b) sum = Success of 'a | Failure of 'b

(* Commands *)

type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Tying*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident*ident
  | Print of string
  | Other of string*preterm list
