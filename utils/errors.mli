val errors_in_snf : bool ref

val color               : bool ref
(** Colored output. *)

(** Print an error message and exit. *)
val fail : Basic.loc -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Print a success message. *)
val success : ('a, Format.formatter, unit) format -> 'a

val fail_typing_error : Typing.typing_error -> 'a
val fail_signature_error : Signature.signature_error -> 'a
val fail_env_error : Env.env_error -> 'a
val fail_dtree_error : Dtree.dtree_error -> 'a
