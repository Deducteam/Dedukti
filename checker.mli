
open Types

val mk_prelude          : (loc*string) -> unit

val mk_require          : (loc*string) -> unit 

val mk_declaration      : ((loc*string)*pterm) -> unit

val mk_definition       : ((loc*string)*pterm*pterm) -> unit

val mk_infered_def      : ((loc*string)*pterm) -> unit

val mk_opaque           : ((loc*string)*pterm*pterm) -> unit

val mk_typecheck        : (loc*pterm*pterm) -> unit

val mk_normalize        : pterm -> unit 

val mk_rules            : prule list -> unit

val mk_ending           : unit -> unit
