val modname : Basic.mident

val mk_num : Basic.loc * string -> Preterm.preterm
val mk_num_patt : Basic.loc * string -> Preterm.prepattern
val mk_char : Basic.loc * char -> Preterm.preterm
val mk_string : Basic.loc * string -> Preterm.preterm
