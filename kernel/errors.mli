open Basic

val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

(** Print a success message. *)
val success : ('a, Format.formatter, unit) format -> 'a

(** Prints a message explaining the env_error then fails. *)
val fail_env_error : Env.env_error -> 'a

(** Either returns the encapsulated Env.err value or fails printing an error
    message. *)
val fail_if_err : 'a Env.err -> 'a
