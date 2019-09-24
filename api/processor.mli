(** A process module processes entries. It is parameterized by an environment. *)
module type S =
sig
  type t

  val handle_entry : Env.t -> Entry.entry -> unit

  val get_data : unit -> t
end

(** Provide a type checker for entries *)
module TypeChecker      : S with type t = unit

(** Only build a signature without type checking the entries *)
module SignatureBuilder : S with type t = Signature.t

(** Pretty prints entries *)
module EntryPrinter     : S with type t = unit

(** Computes dependencies *)
module Dependencies     : S with type t = Dep.t

(** [handle_processor env P ic] parses the input [ic] in the environment [env],
    applies the processor P on the entries and returns the result. *)
val handle_processor : Env.t -> (module S) -> unit

val handle_input  :
  Parser.t ->
  ?hook_before:(Env.t -> unit) ->
  ?hook_after:(Env.t -> (Env.t * Basic.loc * exn) option -> unit) ->
  (module S with type t = 'a) -> 'a

val handle_files :
  string list ->
  ?hook_before:(Env.t -> unit) ->
  ?hook_after:(Env.t -> (Env.t * Basic.loc * exn) option -> unit) ->
  (module S with type t = 'a) -> 'a
