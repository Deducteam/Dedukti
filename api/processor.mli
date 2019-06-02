(** High level entry processors.
    Use these to quickly implement a Dedukti type checker.
*)

module type Processor =
sig
  module Printer      : Pp.Printer
  module ErrorHandler : Errors.ErrorHandler

  type t

  val handle_entry : Entry.entry -> unit

  val get_data : unit -> t
end

module TypeChecker (E:Env.S) : Processor

module DefaultTypeChecker : Processor

module SignatureBuilder (E:Env.S) : Processor with type t = Signature.t

module EntryPrinter : Processor
