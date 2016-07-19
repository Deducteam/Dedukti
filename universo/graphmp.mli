(* 'a is the type of vertices *)
type t

val empty : t
              
val add_vertex : t -> int -> t
                               
val add_edge : t -> int -> int -> t
                                    
(* return nodes that do not have edge that comes in *)
val roots : t -> int list

val shortest_path : t -> int -> (int, int) Hashtbl.t                     
                     
