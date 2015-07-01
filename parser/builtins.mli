val modname : Basics.ident

val mk_num : Basics.loc * string -> Preterm.preterm
val mk_char : Basics.loc * char -> Preterm.preterm
val mk_string : Basics.loc * string -> Preterm.preterm

exception Not_atomic_builtin

val print_term : Format.formatter -> Term.term -> unit
