module Dep : sig
  type argument = Ignore

  val ko :
    ?regression:bool -> error:[`Cli] -> filename:string -> argument list -> unit
end

module Check : sig
  type argument =
    | Eta
    | Import of string
    | Sr_check of int
    | Export
    | Type_lhs
    | Left_linear
    | Standard

  (** [ok ?regression ~filename arguments] runs [dk check] on
     [filename] with [arguments].

      If [regression] is set to [true], the standard output of the
     test will be recorded into a file. This can be used to ensure the
     reduction strategy did not change for example.

     Any regression test is run in quiet mode and consequently
     [Cli.log_level] is ignored.

     If the [Cli.log_level] is:

     - [Report] (default), the test is run in quiet mode.

     - [Debug] (option "-v" (see {Tezt.Cli})), the test is run with
     argument "-d montru" (default value for verbose mode with
     [dk check]).

     - [Info] (option "-i" (see {Tezt.Cli})), the test is run with
     argument "-d n". Each top-level symbol is logged as well as the
     command run.*)
  val ok : ?regression:bool -> filename:string -> argument list -> unit

  (** [ko error ~filename arguments] is similar to [ok] but a failure
     is expected when running [dk check]. It is checked that the error
     returned by [dk check] is [error]. *)
  val ko :
    error:[`Code of int | `System] -> filename:string -> argument list -> unit
end

module Meta : sig
  type argument =
    | No_meta
    | No_beta
    | Meta of string
    | Import of string
    | Quoting of [`Prod]
    | No_unquoting

  (** [run ?dep ~filename arguments] runs [dk meta] on [filename] with
     [arguments].

      [dep] should contain files which are required for executing the
     command. Either because the module is a dependency of [filename],
     or because it is a dependency of a file given with the option
     [Meta]. For every file in [dep], their directory is imported
     using the option [Import].

     If [check_output] is provided, [dk check] is called on the output
     produced by [dk meta]. The test fails if the output does not type
     checks. *)
  val run :
    ?dep:string list ->
    ?check_output:bool ->
    filename:string ->
    argument list ->
    unit
end

module Pretty : sig
  type argument = |

  (** [run ~dep ~filename arguments] runs [dk beautify] on file [filename]
      passing arguments [arguments]. The resulting file is type checked. If the
      file depends on other Dedukti files, these files must appear in [dep] so
      that their directory is added to the load path. *)
  val run : ?dep:string list -> filename:string -> argument list -> unit
end

module Universo : sig
  type argument =
    | Config of string
    | Theory of string
    | Output_directory of string
    | Import of string
    | Simplify of string

  val run :
    ?fails:bool -> ?regression:bool -> filename:string -> argument list -> unit
end
