(** Errors handling *)

val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

module type ErrorHandler =
sig

val print_success : string option -> unit
(** [print_success] Prints a success message after handling the
    given file (or standard input in case of [None]). *)

val graceful_fail : string option -> exn -> 'a
(** [graceful_fail file err]
    Prints a message explaining the given error
    raised while handling the (optionnal) given file
    then exits with code 3. *)

end

module Make (E:Env.S) : ErrorHandler
