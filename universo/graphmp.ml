module Vertex = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

type 'a cloture = Val of 'a | Inf

module MaxPlus =
  struct
    type t = int cloture
    (* plus n m = max n m *)
    let plus n m =
      match n,m with
      | Inf, Inf -> Inf
      | Val n, Inf
      | Inf, Val n -> Val n
      | Val n, Val m -> Val (max n m)

    (* times n m = n + m *)
    let times n m =
      match n, m with
      | _, Inf
      | Inf, _ -> Inf
      | Val n, Val m -> Val (n + m)

    let neutral_plus = Inf
    let neutral_times = Val 0
    let string n =
      match n with
      | Inf -> "-inf"
      | Val n -> string_of_int n
  end

module Edge = struct
  type t = MaxPlus.t
  let compare = compare
  let default = MaxPlus.neutral_plus
end


module G = Graph.Persistent.Digraph.ConcreteLabeled(Vertex)(Edge)
type t = G.t

let empty = G.empty
let add_vertex g i = G.add_vertex g i
let add_edge g i j = G.add_edge_e g (G.E.create i (Val 1) j)

let roots g =
  let filter_vertex p g = G.fold_vertex
                            (fun v l -> if p g v then v::l else l) g [] in
  filter_vertex (fun g v -> G.in_degree g v = 0) g


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

module ShortestPath = Shortestpath.MakeSR(MaxPlus)(G)(RandomQueue)

let shortest_path g start =
  let sol = ShortestPath.stepper_algorithm ShortestPath.mohri_stepper g start in
  (fun x -> match ShortestPath.result sol x with Inf -> assert false | Val n -> n)
