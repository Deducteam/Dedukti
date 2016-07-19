open Graph
open Semiring


module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module IntInt = struct
    type t = int * int
    let compare = compare
  end
	       
module IntMap = Map.Make(Int)
module IntIntSet = Set.Make(IntInt)
       
module type Queue = sig
    type 'a t	   
    exception Empty
    val empty : 'a t
    val enqueue : 'a -> 'a t -> 'a t
    val dequeue : 'a t -> ('a * 'a t)
    val mem : 'a -> 'a t -> bool
    val iter : ('a -> unit) -> 'a t -> unit
  end

       
module Stack =
  struct
    type 'a t = 'a list
		   
    exception Empty
    let empty = []
    let enqueue  a l = a::l
    let dequeue l =
      match l with
      | [] -> raise Empty
      | x::l -> (x,l)
    let mem = List.mem
    let iter = List.iter
  end

module RandomQueue =
  struct
    type 'a t = 'a list
    exception  Empty
    let empty = []
    let enqueue a l = a::l
    let dequeue l =
      match l with
      | [] -> raise Empty
      | _::l' ->
	 let n = Random.int (List.length l) in
	 let elt = List.nth l n in
	 (*Assumption : no doublons*)
	 let l' = List.filter (fun x -> x<>elt) l in
	 (elt,l')
    let mem = List.mem
    let iter = List.iter
  end

    
module DefaultDotAttr(S:Semiring)(G:Sig.G with type V.t = int
			   and type V.label = int
			   and type E.t = int * S.t * int
			   and type E.label = S.t) =
  struct
    include G
    let graph_attributes _ =
      [`BgcolorWithTransparency(Int32.of_int 0)]
    let default_vertex_attributes _ = []
    let vertex_name i = string_of_int i
    let vertex_attributes _ = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes (_,l,_) =
      [`Label(S.string l)]
  end



    
(*
    module X =
      struct
	include G
	let graph_attributes _ =
	  [`BgcolorWithTransparency(Int32.of_int 0)]
(*	  [`BgcolorWithTransparency(Graphviz.color_to_color_with_transparency())]*)
	let default_vertex_attributes _ = []
	let vertex_name i = string_of_int i
	let vertex_attributes _ = []
	let get_subgraph _ = None
	let default_edge_attributes _ = []
	let edge_attributes (_,l,_) =
	  [`Label(S.string l)]
      end
    module Dot = Graphviz.Dot(X)
		 *)
       
module PrintGraph(S:Semiring)(G:Sig.G  with type V.t = int
			   and type V.label = int
			   and type E.t = int * S.t * int
			   and type E.label = S.t) =
  struct

    module X =
      struct
	include G
	let graph_attributes _ =
	  [`BgcolorWithTransparency(Int32.of_int 0)]

	let default_vertex_attributes _ = []
	let vertex_name i = string_of_int i
	let vertex_attributes _ = []
	let get_subgraph _ = None
	let default_edge_attributes _ = []
	let edge_attributes (_,l,_) =
	  [`Label(S.string l)]
      end
    module Dot = Graphviz.Dot(X)
    let print_graph ?(format="pdf") g =
      let fname, cout = Filename.open_temp_file "graph" ".dot" in
      Dot.output_graph cout g;
      ignore (Sys.command("cat "^fname^" > ~/tmp/truc.dot"))
     (* ignore (Sys.command ("dot -T"^format^" "^fname ^ " -o ~/tmp/graph."^format)); 
      if format="pdf" then
	ignore (Sys.command("evince ~/tmp/graph."^format^"&"))
      else if format="png" then
	ignore (Sys.command("feh ~/tmp/graph."^format^"&")) *)
	     
  end
