open Basics
open Term

module type Visitor = sig
   type 'a m
    type entry
    val return         : 'a -> 'a m
    val bind           : 'a m -> ('a -> 'b m) -> 'b m
end


type command =
  (* Reduction *)
  | Whnf of term
  | Hnf of term
  | Snf of term
  | OneStep of term
  | Conv of term*term
  (*Typing*)
  | Check of term*term
  | Infer of term
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*term list

module type C = sig
  type 'a m
  type entry
  val mk_command : loc -> command -> entry m
end

module Make : functor (M:Visitor) -> C with type 'a m = 'a M.m and type entry = Default.signature

val print_command : Format.formatter -> command -> unit
