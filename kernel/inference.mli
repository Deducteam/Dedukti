(** Type inference/type checking. *)
open Preterm
open Term

val coc : bool ref

val infer : context -> term -> term
val check : context -> term -> term -> unit
val is_a_type : context -> term -> unit
val check_rule : Rule.rule -> unit

(** [Inference.infer pte] returns the term corresponding to the preterm [pte]
  * together with its type. *)
val infer2 : preterm -> term*term

(** [Inference.check pte pty] checks that [pte] has type [pty] and returns them
  as terms. *)
val check2 : preterm -> preterm -> term*term

(** [Inference.is_type pte] checks that [pte] is typed by a sort and returns it
 as a terms. *)
val is_a_type2 : preterm -> term

(** Checks that a rule is well-typed. *)
val check_prule : prule -> Rule.rule
