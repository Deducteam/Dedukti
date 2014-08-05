(** basic datatypes *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val empty : ident
val string_of_ident : ident -> string
val pp_ident : out_channel -> ident -> unit
val hstring : string -> ident
val ident_eq : ident -> ident -> bool
val ident_cmp : ident -> ident -> int

module IdentMap : Map.S with type key = ident

(** {2 Variables} *)

module Var : sig
  type t = private (ident * int)

  val fresh: t -> t
  val fresh_of_ident : ident -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : out_channel -> t -> unit

  val ident : t -> ident
end

module VarMap : Map.S with type key = Var.t
module VarSet : Set.S with type elt = Var.t

module VarTbl : sig
  include Hashtbl.S with type key = Var.t
  val get_default : 'a t -> Var.t -> x0:'a -> 'a
  (** [get_default tbl var ~x0] returns the binding of [var] in [tbl] if
      present, [x0] if [var] is not bound in the table *)
end

type 'a subst = 'a VarMap.t

(* bind a variable. If already bound, the old binding is shadowed. *)
val subst_bind : 'a subst -> Var.t -> 'a -> 'a subst

(* find the value associated to this variable, if any *)
val subst_find  : 'a subst -> Var.t -> 'a option

(** {2 Lists with Length} *)

module LList : sig
  type +'a t = private {
    len : int;
    lst : 'a list;
  }

  val cons : 'a -> 'a t -> 'a t
  val nil : 'a t
  val is_empty : _ t -> bool
  val len : _ t -> int
  val lst : 'a t -> 'a list

  val make : len:int -> 'a list -> 'a t
  val make_unsafe : len:int -> 'a list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val append_l : 'a t -> 'a list -> 'a t
  val nth : 'a t -> int -> 'a
  val remove : int -> 'a t -> 'a t
end

(** {2 Localization} *)

type loc
val dloc                : loc
val mk_loc              : int -> int -> loc
(** mk_loc [line] [column] *)
val of_loc              : loc -> (int*int)

(** {2 Parsing} *)

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
  | STRING      of string

exception EndOfFile

(** {2 PreTerms/PrePatterns} *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PJoker      of loc

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident * prepattern list * preterm

(** {2 Terms/Patterns} *)

type term = private
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | Var   of loc*Var.t                  (* Variable *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*Var.t*term*term        (* Lambda abstraction *)
  | Pi    of loc*Var.t option*term*term (* Pi abstraction *)
  | Meta  of loc*int
  | Let   of loc*Var.t*term*term        (* let x=a in b *)

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_Var      : loc -> Var.t -> term
val mk_Const    : loc -> ident -> ident -> term
val mk_Lam      : loc -> Var.t -> term -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_App_l    : term -> term list -> term
val mk_Pi       : loc -> Var.t option -> term -> term -> term
val mk_Unique   : unit -> term
val mk_Meta     : loc -> int -> term
val mk_Let      : loc -> Var.t -> term -> term -> term

val subst_empty : 'a subst

val subst_is_empty : _ subst -> bool

(* If the term is a variable, return its image in the substitution *)
val subst_deref : term subst -> term -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool

type pattern =
  | Var         of loc*Var.t
  | Pattern     of loc*ident*ident*pattern list (* qualified symbol + sub-patterms *)
  | Brackets    of term
  | Joker       of loc*Var.t

type top = ident*pattern array

(* Context for type inference *)
type context = {
  var2ty : term subst;
  const2ty : term IdentMap.t;
  ident2var : Var.t IdentMap.t; (* scoping *)
}

val ctx_empty : context

val ctx_bind : context -> Var.t -> term -> context
(** Bind a variable to its type *)

val ctx_declare : context -> ident -> term -> context
(** Declare the type of a constant *)

val ctx_bind_ident : context -> ident -> Var.t -> context
(** For scoping, bind a name to a variable *)

(**{2 Rewrite Rules} *)

type rule = {
        l:loc;
        ctx:context;
        md:ident; (* module name *)
        id:ident; (* head symbol that is rewritten *)
        args:pattern list;
        rhs:term; }

type rule_constraint =
  | EqTerm of term * term

(** How to build a substitution, given a decision tree matching stack *)
type rule_subst_builder = (int * Var.t) list

(* decision tree for matching *)
type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of rule_subst_builder * rule_constraint list * term * dtree option

(** {2 Environment} *)

module H : Hashtbl.S with type key := ident

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule list*int*dtree

(** {2 Commands} *)

type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Typing*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident*ident
  | Print of string
  | Other of string*preterm list
