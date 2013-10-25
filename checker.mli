
open Types

val mk_prelude          : (loc*ident) -> unit

val mk_require          : (loc*ident) -> unit 

val mk_declaration      : ((loc*ident)*pterm) -> unit

val mk_definition       : ((loc*ident)*pterm*pterm) -> unit

val mk_infered_def      : ((loc*ident)*pterm) -> unit

val mk_opaque           : ((loc*ident)*pterm*pterm) -> unit

val mk_typecheck        : (loc*pterm*pterm) -> unit

val mk_normalize        : pterm -> unit 

val mk_rules            : prule list -> unit

val mk_ending           : unit -> unit
