(** basic datatypes *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val string_of_ident : ident -> string
val pp_ident : out_channel -> ident -> unit
val hstring : string -> ident
val ident_eq : ident -> ident -> bool
val qmark : ident

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
  val of_list : 'a list -> 'a t

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

(** {2 PreTerms} *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm option * preterm
  | PrePi   of loc * ident option * preterm * preterm

(** {2 Terms} *)

type term = private
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term option*term        (* Lambda abstraction *)
  | Pi    of loc*ident*term*term                (* Pi abstraction *)

type context = ( ident * term ) list

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> ident -> ident -> term
val mk_Lam      : loc -> ident -> term option -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_Pi       : loc -> ident -> term -> term -> term
val mk_Arrow    : loc -> term -> term -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool

(** {2 Commands} *)
(*FIXME move*)
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
  | Gdt of ident option*ident
  | Print of string
  | Other of string*preterm list

