(** Term reduction and conversion test. *)
open Types

(** Let-definitions *)
type let_ctx    = term option LList.t

(** Head Normal Form *)
val hnf         : ?let_ctx:let_ctx -> term -> term

(** Weak Head Normal Form *)
val whnf        : ?let_ctx:let_ctx -> term -> term

(** Strong Normal Form *)
val snf         : ?let_ctx:let_ctx -> term -> term

(** Conversion Test *)
val are_convertible             : ?let_ctx:let_ctx -> term -> term -> bool

(**One Step Reduction*)
val one_step                    : ?let_ctx:let_ctx -> term -> term option
