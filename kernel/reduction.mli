open Term
(** Term reduction and conversion test. *)

(** Head Normal Form *)
val hnf         : LetCtx.t -> term -> term

(** Weak Head Normal Form *)
val whnf        : LetCtx.t -> term -> term

(** Strong Normal Form *)
val snf         : LetCtx.t -> term -> term

(** Conversion Test *)
val are_convertible             : LetCtx.t -> term -> term -> bool

(**One Step Reduction*)
val one_step                    : LetCtx.t -> term -> term option
