
open Types

val get_rw : ident -> rule list -> (int*gdt)

val add_rw : (int*gdt) -> rule list -> (int*gdt)
