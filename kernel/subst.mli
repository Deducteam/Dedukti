(** Substitutions using DeBruijn indices. *)

open Term

exception UnshiftExn

(** A substitution is a function mapping a variable (location, identifier and DB index) under
    a given number of lambda abstractions to a term.
    A substitution raises Not_found meaning that the variable is not subsituted. *)
type substitution = Basic.loc -> Basic.ident -> int -> int -> term

(** [apply_subst subst n t] applies [subst] to [t] under [n] lambda abstractions.
      - Variables with DB index [k] <  [n] are considered "locally bound" and are never substituted.
      - Variables with DB index [k] >= [n] may be substituted if [k-n] is mapped in [sigma]. *)
val apply_subst : substitution -> int -> term -> term

(** [shift i t] shifts every De Bruijn indices in [t] by [i]. *)
val shift : int -> term -> term

(** [unshift i t] shifts every De Bruijn indices in [t] by -[i].
    Raises [UnshiftExn] when [t] contains De Bruijn variables of indices {i k < }[i]. *)
val unshift : int -> term -> term

(** [psubst_l l k t] substitutes the first {i n} De Bruijn variables
    (with {i n} = [length l]) in [t] with the corresponding term in [l].
    Unshifts {i n} times the free variables with greater indices. *)
val psubst_l : term Lazy.t Basic.LList.t -> term -> term

(** [subst t u] substitutes the first free De Bruijn variable with [u] in [t].
    Unshifts the other free variables. *)
val subst : term -> term -> term

(** [subst_n n y t] substitutes the [n]-th free De Bruijn variable in [t] with
    a fresh variable named [y] becoming the first free variable in [t].
    All others free variables are shifted by one preventing index collision. *)
val subst_n : int -> Basic.ident -> term -> term

(** [occurs n t] returns true if [t] contains the variable [n]. *)
val occurs : int -> term -> bool
