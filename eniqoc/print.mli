open Ast

module type Printer =
sig
  val print_name : Format.formatter -> Basic.name -> unit

  val print_ident : Format.formatter -> Basic.ident -> unit

  val print_mident : Format.formatter -> Basic.mident -> unit

  val print_term : Format.formatter -> term -> unit

  val print_declaration : Format.formatter -> declaration -> unit

  val print_definition : Format.formatter -> definition -> unit

  val print_prelude : Format.formatter -> unit -> unit
end

module CoqPrinter:Printer

module AgdaPrinter:Printer

val set_printer : string -> unit

val print_ast : Format.formatter -> ast -> unit
