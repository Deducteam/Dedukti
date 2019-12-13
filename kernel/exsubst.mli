open Term

(** An extended substitution is a function mapping
    - a variable (location, identifier and DB index)
    - applied to a given number of arguments
    - under a given number of lambda abstractions
    to a term.
    A substitution raises Not_found meaning that the variable is not subsituted. *)
type ex_substitution = Basic.loc -> Basic.ident -> int -> int -> int -> term

(** [apply_exsubst subst n t] applies [subst] to [t] under [n] lambda abstractions.
      - Variables with DB index [k] <  [n] are considered "locally bound" and are never substituted.
      - Variables with DB index [k] >= [n] may be substituted if [k-n] is mapped in [sigma]
          and if they occur applied to enough arguments (substitution's arity). *)
val apply_exsubst : ex_substitution -> int -> term -> term*bool

(** This modules implements extended substitution of DB variables in a term.
    This is typically used to:
    1) infer a "most general" typing substitution from constraints gathered while
       inferring the type of the LHS of a rule.
    2) apply the substitution to the RHS of the rule before typechecking it.
*)
module ExSubst :
sig
  type t

  val identity : t
  (** Empty substitution *)

  val is_identity : t -> bool
  (** Checks emptyness *)

  val add : t -> int -> int -> term -> t
  (** [add sigma n t] returns the substitution [sigma] with the extra mapping [n] -> [t]. *)

  val subst : t -> ex_substitution
  (** Substitution function corresponding to given ExSubst.t instance [sigma].
      We lookup the table at index: (DB index) [n] - (nb of local binders) [k]
      When the variable is under applied it is simply not substituted.
      Otherwise we return the reduct is shifted up by (nb of local binders) [k] *)

  val subst2 : t -> int -> ex_substitution
  (** Special substitution function corresponding to given ExSubst.t instance [sigma]
      "in a smaller context":
      Assume [sigma] a substitution in a context Gamma = Gamma' ; Delta with |Delta|=[i].
      Then this function represents the substitution [sigma] in the context Gamma'.
      All variables of Delta are ignored and substitutes of the variables of Gamma'
      are unshifted. This may therefore raise UnshiftExn in case substitutes of
      variables of Gamma' refers to variables of Delta.
  *)

  val apply : t -> int -> term -> term*bool
  (** [apply sigma n t] applies the subsitution [sigma] to [t] considered
      under [n] lambda abstractions. *)

  val mk_idempotent : t -> t
  (** [mk_idempotent sigma] successively applies sigma to its mapped terms until this operation
      has no effect anymore. *)

  val pp : (int->Basic.ident) -> t Basic.printer
  (** Prints the substitution using given naming function *)
end
