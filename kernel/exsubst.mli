open Term

(** This modules implements extended substitution of DB variables in a term.
    This is typically used to:
    1) infer a "most general" typing substitution from constraints gathered while
       inferring the type of the LHS of a rule.
    2) apply the substitution to the RHS of the rule before typechecking it.
*)
module ExSubst : sig
  type t

  (** Empty substitution *)
  val identity : t

  (** Checks emptyness *)
  val is_identity : t -> bool

  (** [add sigma n t] returns the substitution [sigma] with the extra mapping [n] -> [t]. *)
  val add : t -> int -> int -> term -> t

  (** [apply sigma n t] applies the subsitution [sigma] to [t] considered
      under [n] lambda abstractions. *)
  val apply : t -> int -> term -> term

  (** Same as apply, but outputting a boolean [true] if the term is modified
      by the substitution. *)
  val apply' : t -> int -> term -> term * bool

  (** Special substitution function corresponding to given ExSubst.t instance [sigma]
      "in a smaller context":
      Assume [sigma] a substitution in a context Gamma = Gamma' ; Delta with |Delta|=[i].
      Then this function represents the substitution [sigma] in the context Gamma'.
      All variables of Delta are ignored and substitutes of the variables of Gamma'
      are unshifted. This may therefore raise UnshiftExn in case substitutes of
      variables of Gamma' refers to variables of Delta.
  *)
  val apply2 : t -> int -> int -> term -> term

  (** [mk_idempotent sigma] successively applies sigma to its mapped terms until this operation
      has no effect anymore. *)
  val mk_idempotent : t -> t

  (** Prints the substitution using given naming function *)
  val pp : (int -> Basic.ident) -> t Basic.printer
end
