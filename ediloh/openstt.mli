module type OpenSTT = sig

  type name
  type var
  type tyOp
  type ty
  type term
  type const

  type hyp
  type thm

  type 'a obj

  val mk_name : string list -> string -> name obj


  val mk_var : name obj -> ty obj -> var obj


  val mk_tyOp : name obj -> tyOp obj


  val mk_varType : name obj -> ty obj

  val ty_of_tyOp : tyOp obj -> (ty obj) list -> ty obj

  val mk_arrow_type : ty obj -> ty obj -> ty obj

  val mk_bool_type : ty obj

  val mk_equal_type : ty obj -> ty obj

  val mk_impl_type : ty obj

  val mk_forall_type : ty obj -> ty obj


  val mk_app_term : term obj -> term obj -> term obj

  val mk_abs_term : var obj -> term obj -> term obj

  val mk_var_term : var obj -> term obj

  val term_of_const : const obj -> ty obj -> term obj

  val mk_equal_term : term obj -> term obj -> ty obj -> term obj

  val mk_impl_term : term obj -> term obj -> term obj

  val mk_forall_term : term obj -> ty obj -> term obj

  val const_of_name : name obj -> const obj

  val mk_subst : thm obj -> (name obj * ty obj) list -> (var obj * term obj) list -> thm obj

  val mk_hyp : (term obj) list -> hyp obj


  val mk_axiom : hyp obj -> term obj -> thm obj

  val mk_eqMp : thm obj -> thm obj -> thm obj

  val mk_appThm : thm obj -> thm obj -> thm obj

  val mk_absThm : var obj -> thm obj -> thm obj

  val mk_rule_intro_forall : name obj-> ty obj -> term obj -> thm obj -> thm obj

  val mk_rule_elim_forall : thm obj -> term obj -> ty obj -> term obj -> thm obj

  val mk_rule_intro_impl : thm obj -> term obj -> term obj -> thm obj

  val mk_rule_elim_impl : thm obj -> thm obj -> term obj -> term obj -> thm obj

  val mk_impl_equal : thm obj -> thm obj -> term obj -> term obj -> term obj -> term obj -> thm obj

  val mk_forall_equal : thm obj -> name obj -> term obj -> term obj -> ty obj -> thm obj

  val mk_equal_equal : term obj -> term obj -> term obj -> term obj -> thm obj -> thm obj -> ty obj -> thm obj

  val thm_of_const_name : name obj -> thm obj

  val mk_refl : term obj -> thm obj

  val mk_assume : term obj -> thm obj

  val mk_const : name obj -> term obj -> unit

  val mk_sym : thm obj -> thm obj

  val mk_betaConv : term obj -> thm obj

  val mk_thm : name obj -> term obj -> hyp obj -> thm obj -> unit

  val thm_of_lemma : name obj -> thm obj

  val debug : 'a obj -> unit

  val comment : ('a, Format.formatter, unit) format -> 'a
end

module OpenTheory : OpenSTT
