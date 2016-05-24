(** Scope managmement: from preterms to terms. *)

module type Visitor = sig
   type 'a m
    type entry
    val return         : 'a -> 'a m
    val bind           : 'a m -> ('a -> 'b m) -> 'b m
    val mk_prelude     : Basics.loc -> Basics.ident -> entry m
    val mk_declaration : Basics.loc -> Basics.ident -> Term.term -> entry m
    val mk_definition  : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> entry m
    val mk_opaque      : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> entry m
    val mk_rules       : Rule.rule list -> entry m
    val mk_command     : Basics.loc -> Cmd.command -> entry m
    val mk_Type        : Basics.loc -> Term.term m
    val mk_DB          : Basics.loc -> Basics.ident -> int -> Term.term m
    val mk_Const       : Basics.loc -> Basics.ident -> Basics.ident -> Term.term m
    val mk_Lam         : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> Term.term m
    val mk_App         : Term.term  -> Term.term -> Term.term list -> Term.term m
    val mk_Pi          : Basics.loc -> Basics.ident -> Term.term -> Term.term -> Term.term m
    val mk_Arrow       : Basics.loc -> Term.term   -> Term.term -> Term.term m
    val mk_ending      : entry m -> unit
end


module type S = sig
  type 'a m 
  val scope_term : Term.context -> Preterm.preterm -> Term.term m
  val scope_rule : Preterm.prule -> Rule.rule m
end

module Make : functor (M:Visitor) -> S with type 'a m = 'a M.m

val name        : Basics.ident ref
