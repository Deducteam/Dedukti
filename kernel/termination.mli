open Basic
open Term
open Rule
open Callgraph

type global_result=Terminating | G_SelfLooping
                  | G_UsingBrackets | G_NonPositive 
                  | G_NotHandledRewritingTypeLevel
                  | G_Coc


val table_result : (global_result, name list) Hashtbl.t

val list_SelfLooping : (name * index list) list ref

val initialize : unit -> unit

val add_rules : rule_infos list -> unit

val add_constant : name -> staticity -> term -> unit

val termination_check : unit -> bool
  
val pp_list_of_self_looping_rules : (name *index list) printer
