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

(*
val mk_Type     : context -> loc -> judgment
val mk_Const    : context -> loc -> ident -> ident -> judgment
val mk_Var      : context -> loc -> ident -> int -> judgment
val mk_App      : judgment -> judgment -> judgment
val mk_Pi       : judgment -> judgment
val mk_Lam      : judgment -> judgment
val mk_Conv     : judgment -> judgment -> judgment
 *)
