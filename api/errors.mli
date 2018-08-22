open Basic

val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

(** Print a success message. *)
val success : ('a, Format.formatter, unit) format -> 'a

(** Print an error message with given code and and exit. *)
val fail_exit : int -> loc -> ('a, Format.formatter, unit) format -> 'a

(** Prints a message explaining the env_error then exits with code 3. *)
val fail_env_error : Env.env_error -> 'a

(** Print a system error message then exits with code 1. *)
val fail_sys_error : string -> 'a
