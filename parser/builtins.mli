val modname : Basic.mident

val mk_num : Basic.loc * string -> Preterm.preterm
val mk_num_patt : Basic.loc * string -> Preterm.prepattern
val mk_char : Basic.loc * char -> Preterm.preterm
val mk_char_patt : Basic.loc * char -> Preterm.prepattern
val mk_string : Basic.loc * string -> Preterm.preterm
val mk_string_patt : Basic.loc * string -> Preterm.prepattern

exception Not_atomic_builtin

val print_term : Format.formatter -> Term.term -> unit
val print_pattern : Format.formatter -> Rule.pattern -> unit
