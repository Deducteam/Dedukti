(** Term reduction and conversion test. *)
open Types

(** Head Normal Form *)
val hnf         : ?let_ctx:term subst -> term -> term

(** Weak Head Normal Form *)
val whnf        : ?let_ctx:term subst -> term -> term

(** Strong Normal Form *)
val snf         : ?let_ctx:term subst -> term -> term

(** Conversion Test *)
val are_convertible             : ?let_ctx:term subst -> term -> term -> bool

(**One Step Reduction*)
val one_step                    : ?let_ctx:term subst -> term -> term option
