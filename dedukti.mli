module Basics : sig

  type ident
  val string_of_ident : ident -> string
  val pp_ident : out_channel -> ident -> unit
  val hstring : string -> ident
  val ident_eq : ident -> ident -> bool
  val qmark : ident

  type loc
  val dloc           : loc
  val mk_loc         : int -> int -> loc
  val of_loc         : loc -> (int*int)

  val add_path       : string -> unit
  val get_path       : unit -> string list

end

(* ************************************************************************** *)

module Term : sig
  open Basics

  type term = private
    | Kind                                (* Kind *)
    | Type  of loc                        (* Type *)
    | DB    of loc*ident*int              (* deBruijn *)
    | Const of loc*ident*ident            (* Global variable *)
    | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
    | Lam   of loc*ident*term option*term        (* Lambda abstraction *)
    | Pi    of loc*ident*term*term                (* Pi abstraction *)

  type context = ( loc * ident * term ) list

  val get_loc : term -> loc

  val mk_Kind     : term
  val mk_Type     : loc -> term
  val mk_DB       : loc -> ident -> int -> term
  val mk_Const    : loc -> ident -> ident -> term
  val mk_Lam      : loc -> ident -> term option -> term -> term
  val mk_App      : term -> term -> term list -> term
  val mk_Pi       : loc -> ident -> term -> term -> term
  val mk_Arrow    : loc -> term -> term -> term

  val term_eq : term -> term -> bool
end

(* ************************************************************************** *)

module Env : sig
  open Basics
  open Term
  val ignore_redecl       : bool ref
  val autodep             : bool ref

  val init                : ident -> unit
  val get_type            : loc -> ident -> ident -> term
  val get_name            : unit -> ident
  val export              : unit -> unit
end

(* ************************************************************************** *)

module Rule : sig
open Basics
open Term

type pattern =
  | Var         of loc*ident*int*pattern list
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term

val get_loc_pat : pattern -> loc
val pattern_to_term : pattern -> term

type top = ident*pattern array
type rule = context * pattern * term

end

(* ************************************************************************** *)

module Judgment : sig
  open Term
  open Rule
  open Basics

  val coc : bool ref
  val errors_in_snf : bool ref

  type 'a judgment0 = private { ctx:'a; te:term; ty: term; }

  module Context :
  sig
    type t
    val empty : t
    val add : loc -> ident -> t judgment0 -> t
    val get_type : t -> loc -> ident -> int -> term
    val is_empty : t -> bool
  end

  type judgment = Context.t judgment0
  type rule_judgment

  val infer       : Context.t -> term -> judgment
  val check       : term -> judgment -> judgment

  val inference   : term -> judgment
  val checking    : term -> term -> judgment
  val check_rule  : rule -> rule_judgment

  val declare     : loc -> ident -> judgment -> unit
  val define      : loc -> ident -> judgment -> unit
  val define_op   : loc -> ident -> judgment -> unit
  val add_rules   : rule_judgment list -> unit

  val declare2    : loc -> ident -> term -> unit
  val define2     : loc -> ident -> term -> term option -> unit
  val define_op2  : loc -> ident -> term -> term option -> unit
  val add_rules2  : Rule.rule list -> unit

  val whnf        : judgment -> judgment
  val hnf         : judgment -> judgment
  val snf         : judgment -> judgment
  val one         : judgment -> judgment
  val conv_test   : judgment -> judgment -> bool
  val check_test  : judgment -> judgment -> bool
end

(* ************************************************************************** *)

module Reduction : sig
  open Term
  val hnf         : term -> term
  val whnf        : term -> term
  val snf         : term -> term
  val are_convertible             : term -> term -> bool
  val one_step                    : term -> term option
end

(* ************************************************************************** *)

module Subst : sig
  open Term

  val shift               : int -> term -> term
  exception UnshiftExn
  val unshift             : int -> term -> term
  val subst               : term -> term -> term
end

(* ************************************************************************** *)

module Version : sig
  val version : string
end

(* ************************************************************************** *)

module Pp : sig
  open Basics
  open Term
  open Rule

  val print_db    : bool ref

  val pp_term     : out_channel -> term -> unit
  val pp_pattern  : out_channel -> pattern -> unit
  val pp_rule     : out_channel -> rule -> unit
  val pp_context  : out_channel -> context -> unit
  val pp_list     : string -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
end
