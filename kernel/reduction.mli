open Term
(** Term reduction and conversion test. *)

(** Head Normal Form *)
val hnf         : Signature.t -> term -> term

(** Weak Head Normal Form *)
val whnf        : Signature.t -> term -> term

(** Strong Normal Form *)
val snf         : Signature.t -> term -> term

(** Conversion Test *)
val are_convertible             : Signature.t -> term -> term -> bool

(**One Step Reduction*)
val one_step                    : Signature.t -> term -> term option
