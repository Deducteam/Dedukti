val modname : Kernel.Basic.mident

val mk_num : Kernel.Basic.loc * string -> Preterm.preterm
val mk_num_patt : Kernel.Basic.loc * string -> Preterm.prepattern
val mk_char : Kernel.Basic.loc * char -> Preterm.preterm
val mk_char_patt : Kernel.Basic.loc * char -> Preterm.prepattern
val mk_string : Kernel.Basic.loc * string -> Preterm.preterm
val mk_string_patt : Kernel.Basic.loc * string -> Preterm.prepattern

exception Not_atomic_builtin

val print_term : Format.formatter -> Kernel.Term.term -> unit
val print_pattern : Format.formatter -> Kernel.Rule.pattern -> unit
