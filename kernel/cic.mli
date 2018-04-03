open Basic
open Term

val cic  : mident

val is_z    : term -> bool

val is_s    : term -> bool

val is_succ : term -> bool

val is_term : term -> bool

val is_univ : term -> bool

val is_cuni : term -> bool

val is_prop : term -> bool

val is_type : term -> bool

val is_lift : term -> bool

val is_max  : term -> bool

val is_rule : term -> bool

val is_prod : term -> bool

val is_var  : term -> bool


val extract_var  : term -> ident

val extract_type : term -> term

val extract_s    : term -> term

val extract_succ : term -> term

val extract_univ : term -> term

val extract_cuni : term -> term

val extract_term : term -> term * term

val extract_max  : term -> term * term

val extract_rule : term -> term * term

val extract_lift : term -> term * term

val extract_lift : term -> term * term * term

val extract_prod : term -> term * term * term * term


val mk_prop : term

val mk_z    : term

val mk_s    : term -> term

val mk_type : term -> term

val mk_succ : term -> term

val mk_univ : term -> term

val mk_cuni : term -> term

val mk_rule : term -> term -> term

val mk_max  : term -> term -> term

val mk_term : term -> term -> term

val mk_lift : term -> term -> term -> term

val mk_prod : term -> term -> term -> ident -> term -> term -> term

val assert_type_zero : term -> unit
