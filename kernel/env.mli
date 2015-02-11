(** Global Environment *)

open Basics
open Term
open Signature

type env_error =
  | EnvErrorType of Typing.typing_error
  | EnvErrorSignature of signature_error

val init        : ident -> unit
val get_name    : unit -> ident

val get_type    : loc -> ident -> ident -> (term,signature_error) error
val get_dtree   : loc -> ident -> ident -> (dtree_or_def,signature_error) error
val export      : unit -> bool

val declare     : loc -> ident -> term -> (unit,env_error) error
val define      : loc -> ident -> term -> term option -> (unit,env_error) error
val define_op   : loc -> ident -> term -> term option -> (unit,env_error) error
val add_rules   : Rule.rule list -> (unit,env_error) error

val infer       : term -> (term,env_error) error
val check       : term -> term -> (unit,env_error) error

val hnf         : term -> (term,env_error) error
val whnf        : term -> (term,env_error) error
val snf         : term -> (term,env_error) error
val one         : term -> (term option,env_error) error
val are_convertible : term -> term -> (bool,env_error) error

