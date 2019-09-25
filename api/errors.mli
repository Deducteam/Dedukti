(** Errors handling *)

open Basic

val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

val success : ('a, Format.formatter, unit) format -> 'a
(** Print a success message. *)

val fail_exit : string -> int -> string -> loc option
  -> ('a, Format.formatter, unit) format -> 'a
(** [fail_exit file c error_id lc "..."]
    Prints the given error message prefixed with module and location details
    (when provided) as well as the error ID then exits with the given code. *)

val fail_env_error : Env.t -> loc -> exn -> 'a
(** [fail_env_error env md lc err]
    Prints a message explaining the env_error then exits with code 3. *)

val fail_sys_error : string -> string -> 'a
(** Print a system error message then exits with code 1. *)
