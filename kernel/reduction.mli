open Term
(** Term reduction and conversion test. *)

(** Head Normal Form *)
val hnf         : term -> term

(** Weak Head Normal Form *)
val whnf        : term -> term

(** Strong Normal Form *)
val snf         : term -> term

(** Conversion Test *)
val are_convertible             : term -> term -> bool

(**One Step Reduction*)
val one_step                    : term -> term option
