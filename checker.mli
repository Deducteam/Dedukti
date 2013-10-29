
open Types

val add_decl    : ((loc*ident)*pterm) -> unit

val add_def     : ((loc*ident)*pterm*pterm) -> unit

val add_idef    : ((loc*ident)*pterm) -> unit

val add_odef    : ((loc*ident)*pterm*pterm) -> unit

val typecheck   : (loc*pterm*pterm) -> unit

val add_rules   : loc -> ident -> prule list -> unit

