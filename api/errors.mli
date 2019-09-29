(** Errors handling *)

open Basic

val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

val success : ('a, Format.formatter, unit) format -> 'a
(** Print a success message. *)

val fail_exit : string -> string -> loc option -> ('a, Format.formatter, unit) format -> 'a
(** [fail_exit file error_id lc "..."]
    Prints the given error message prefixed with module and location details
    (when provided) as well as the error ID then exits with the given code. *)

val fail_sys_error : string -> string -> 'a
(** Print a system error message then exits with code 1. *)

type error_msg = Basic.loc option * string

type error_handler = red:(Term.term -> Term.term) -> exn -> error_msg option

val register_exception : (red:(Term.term -> Term.term) -> exn -> error_msg option) -> unit

type error_code = int

val register_exception_code : (exn -> error_code option) -> unit
