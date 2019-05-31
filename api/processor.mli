(** High level entry processors.
    Use these to quickly implement a Dedukti type checker.
*)

module type Processor =
sig
  module Printer      : Pp.Printer
  module ErrorHandler : Errors.ErrorHandler

  val handle_entry : Entry.entry -> unit
end

module TypeChecker (E:Env.S) : Processor

module DefaultTypeChecker : Processor

module SignatureBuilder (E:Env.S) : Processor

module EntryPrinter : Processor
