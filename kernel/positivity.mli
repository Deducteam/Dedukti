open Basic
open Term

exception Coc

val  must_be_str_after : (name, (name * name) list) Hashtbl.t

val after : (name, name list) Hashtbl.t

val updateHT : ('a,'b list) Hashtbl.t -> 'a -> 'b -> unit

val tarjan : (name, name list) Hashtbl.t -> name list list

val right_most : term -> term

type position =  Global | Argument | Negative

val pp_HT : ('a printer) -> ('b printer) -> ('a,'b) Hashtbl.t printer

val constructors_infos : position -> name -> term -> term -> unit
  
val str_positive :
  name list list -> (name, (name * name) list) Hashtbl.t -> unit
