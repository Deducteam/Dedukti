
open Types

val get_rw : rule list -> (int*gdt)

val add_rw : (int*gdt) -> rule list -> (int*gdt) 
