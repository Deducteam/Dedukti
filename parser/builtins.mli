val modname : Basic.ident

val mk_num : Basic.loc * string -> Preterm.preterm
val mk_char : Basic.loc * char -> Preterm.preterm
val mk_string : Basic.loc * string -> Preterm.preterm

exception Not_atomic_builtin

val print_term : Format.formatter -> Term.term -> unit
