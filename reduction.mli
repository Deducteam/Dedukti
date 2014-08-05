(** Term reduction and conversion test. *)
open Types

(** Head Normal Form *)
val hnf         : ?let_ctx:LetCtx.t -> term -> term

(** Weak Head Normal Form *)
val whnf        : ?let_ctx:LetCtx.t -> term -> term

(** Strong Normal Form *)
val snf         : ?let_ctx:LetCtx.t -> term -> term

(** Conversion Test *)
val are_convertible       : ?let_ctx:LetCtx.t -> term -> term -> bool

(**One Step Reduction*)
val one_step              : ?let_ctx:LetCtx.t -> term -> term option
