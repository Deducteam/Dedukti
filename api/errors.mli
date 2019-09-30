(** Errors handling *)

open Basic

val color : bool ref
(** Flag to disable colored output. *)

val success : ('a, Format.formatter, unit) format -> 'a
(** Print a success message. *)

val fail_exit : file:string -> code:string -> loc option -> ('a, Format.formatter, unit) format -> 'a
(** [fail_exit file error_id lc "..."]
    Prints the given error message prefixed with module and location details
    (when provided) as well as the error ID then exits with the given code. *)

val fail_sys_error : file:string -> msg:string -> 'a
(** Print a system error message then exits with code 1. *)

type error_code = int

type error_msg = error_code * Basic.loc option * string

type error_handler = red:(Term.term -> Term.term) -> exn -> error_msg option

val register_exception : (red:(Term.term -> Term.term) -> exn -> error_msg option) -> unit

val string_of_exception :
  red:(Term.term -> Term.term) ->
  Basic.loc -> exn ->
  error_code * Basic.loc * string
