(** Substitutions using DeBruijn indices. *)

open Term

exception UnshiftExn

(** A substitution is a function mapping a variable (location, identifier and DB index) under 
    a given number of lambda abstractions to a term.
    A substitution raises Not_found to mean that the variable is not subsituted. *)
type 'a substitution = 'a -> Basic.loc -> Basic.ident -> int -> int -> term

(** [apply_subst subst n t] applies [subst] to [t] under [n] lambda abstractions.
      - Variables with DB index [k] <  [n] are considered "locally bound" and are never substituted.
      - Variables with DB index [k] >= [n] may be substituted if [k-n] is mapped in [sigma]. *)
val apply_subst : 'a substitution -> 'a -> int -> term -> term

(** [shift i t] shifts every De Bruijn indices in [t] by [i]. *)
val shift : int -> term -> term

(** [unshift i t] shifts every De Bruijn indices in [t] by -[i].
    Raises [UnshiftExn] when [t] contains De Bruijn variables of indices {i k < }[i]. *)
val unshift : int -> term -> term

(** [psubst_l l k t] substitutes the first {i n} De Bruijn variables
    (with {i n} = [length l]) in [t] with the corresponding term in [l].
    Unshifts {i n} times the free variables with greater indices. *)
val psubst_l : (term Lazy.t) Basic.LList.t -> term -> term

(** [subst t u] substitutes the first free De Bruijn variable with [u] in [t].
    Unshifts the other free variables. *)
val subst : term -> term -> term

(** [subst_n n y t] substitutes the [n]-th free De Bruijn variable in [t] with
    a fresh variable named [y] becoming the first free variable in [t].
    All others free variables are shifted by one preventing index collision. *)
val subst_n : int -> Basic.ident -> term -> term

(** [occurs n t] returns true if [t] contains the variable [n]. *)
val occurs : int -> term -> bool

(** This modules implements substitution of DB variables in a term.
    This is typically used to:
    1) infer a "most general" typing substitution from constraints gathered while
       inferring the type of the LHS of a rule.
    2) apply the substitution to the RHS of the rule before typechecking it.
*)
module Subst :
sig
  type t

  val identity : t (** Empty substitution *)

  val is_identity : t -> bool (** Checks emptyness *)

  val add : t -> int -> term -> t
  (** [add sigma n t] returns the substitution [sigma] with the extra mapping [n] -> [t]. *)

  val subst : t substitution (** Provides substitution from Subst instance. *)
  val subst2 : (t *int) substitution (** Provides special substitution from Subst instance. *)

  val apply : t -> int -> term -> term
  (** [apply sigma n t] applies the subsitution [sigma] to [t] considered under [n] lambda abstractions. *)

  val mk_idempotent : t -> t
  (** [mk_idempotent sigma] successively applies sigma to its mapped terms until this operation
      has no effect anymore. *)

  val pp : (int->Basic.ident) -> t Basic.printer
  (** Prints the substitution using given naming function *)
end
