val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

val success : ('a, Format.formatter, unit) format -> 'a
(** Print a success message. *)

val fail_exit : int -> Basic.loc -> ('a, Format.formatter, unit) format -> 'a
(** Print an error message with given code and and exit. *)

val fail_env_error : Basic.loc -> Env.env_error -> unit
(** Prints a message explaining the env_error then exits with code 3. *)

val fail_sys_error : string -> 'a
(** Print a system error message then exits with code 1. *)
