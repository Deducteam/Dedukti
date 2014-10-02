open Term
open Rule
open Basics

type context
val c_of_c : context -> Term.context
type judgment = private { ctx:context; te:term; ty:term; }
type pattern_judgment = private { pctx:context; pat:pattern; pty:term; }
type rule_judgment

val ctx_empty   : context
val ctx_add     : loc -> ident -> judgment -> context

val declare     : loc -> ident -> judgment -> unit
val define      : loc -> ident -> judgment -> unit
val define_op   : loc -> ident -> judgment -> unit
val add_rules   : rule_judgment list -> unit

val mk_Type     : context -> loc -> judgment
val mk_Const    : context -> loc -> ident -> ident -> judgment
val mk_Var      : context -> loc -> ident -> int -> judgment

val mk_App      : judgment -> judgment -> judgment
val mk_Pi       : judgment -> judgment
val mk_Lam      : judgment -> judgment
val mk_Conv     : judgment -> judgment -> judgment

val invert_Pi_judgment : judgment -> judgment*judgment

val get_expected_arg_type       : judgment -> judgment
val get_expected_arg_type_p     : pattern_judgment -> judgment

val mk_Pattern_Var      : context -> loc -> ident -> int   -> pattern_judgment
val mk_Pattern_Const    : context -> loc -> ident -> ident -> pattern_judgment
val mk_Pattern_App      : pattern_judgment -> pattern_judgment -> pattern_judgment
val mk_Pattern_Lambda   : pattern_judgment -> pattern_judgment
val mk_Pattern_Brackets : judgment -> pattern_judgment

val mk_Rule     : pattern_judgment -> judgment -> rule_judgment

val whnf        : judgment -> judgment
val hnf         : judgment -> judgment
val snf         : judgment -> judgment
val one         : judgment -> judgment
val conv        : judgment -> judgment -> bool
val check       : judgment -> judgment -> bool
