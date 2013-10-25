
open Types

val of_pterm    : ident list -> pterm -> term
  
val pat_of_ppat : ident list -> ppattern -> pattern 
  
val top_of_ptop : ident list -> ptop -> top 
