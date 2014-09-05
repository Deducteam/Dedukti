(** Type inference/type checking. *)
open Types

(** [Inference.infer pte] returns the term corresponding to the preterm [pte]
  * together with its type. *)
val infer : preterm -> term*term

(** [Inference.check pte pty] checks that [pte] has type [pty] and returns them
  as terms. *)
val check : preterm -> preterm -> term*term

(** [Inference.is_type pte] checks that [pte] is typed by a sort and returns it
 as a terms. *)
val is_a_type : preterm -> term

(** Checks that a rule is well-typed. *)
val check_rule : prule -> rule
