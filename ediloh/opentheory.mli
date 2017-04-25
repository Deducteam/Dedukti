type name
type var
type tyop
type ty
type term
type const

type hyp
type thm

val mk_name : string list -> string -> name


val mk_var : name -> ty -> var


val mk_tyop : name -> tyop


val mk_varType : name -> ty

val ty_of_tyop : tyop -> ty list -> ty

val mk_arrow_type : ty -> ty -> ty

val mk_bool_type : ty

val mk_equal_type : ty -> ty

val mk_true_type : ty

val mk_and_type : ty

val mk_impl_type : ty


val mk_appTerm : term -> term -> term

val mk_absTerm : var -> term -> term

val mk_varTerm : var -> term

val term_of_const : const -> ty -> term

val mk_equal_term : term -> term -> ty -> term

val mk_true_term : term

val mk_and_term : term -> term -> term

val mk_impl_term : term -> term -> term

val mk_forall_term : term -> ty -> term

val const_of_name : name -> const


val mk_hyp : term list -> hyp


val mk_axiom : hyp -> term -> thm

val mk_eqMp : thm -> thm -> thm

val mk_axiom_true : term * hyp * thm

val mk_axiom_and : term -> term -> term * hyp * thm

val mk_axiom_impl : term -> term -> term * hyp * thm

val mk_axiom_forall : term -> ty -> term * hyp * thm

val mk_rule_intro_forall : name -> ty -> term -> thm -> term * hyp * thm

val mk_rule_elim_forall : thm -> term -> ty -> term -> term * hyp * thm

val mk_rule_intro_impl : thm -> term -> term -> term * hyp * thm

val mk_rule_elim_impl : thm -> thm -> term -> term -> term * hyp * thm

val thm_of_const : const -> thm

val mk_refl : term -> thm

val mk_assume : term -> thm

val mk_const : name -> term -> unit

val mk_thm : term -> hyp -> thm -> unit


val debug : unit -> unit

val to_string : unit -> string
