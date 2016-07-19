module type CostStructure =
  sig
    type t
    val times : t -> t -> t
    val neutral_times : t
    val neutral_plus : t
    val compare : t -> t-> int
    val string : t -> string
  end


module type Semiring =
  sig
    type t
    val plus : t -> t -> t
    val times : t -> t -> t
    val neutral_plus : t
    val neutral_times : t
    val string : t -> string
  end


module type Dioid =
  sig
    include Semiring
    val compare : t -> t -> int
  end

module type Int =
  sig
    val int : int
  end

    
module Product(S:Semiring)(T:Semiring) : (Semiring with type t = (S.t * T.t))
    
module Boolean:Semiring with type t = bool
    
module Countable:Semiring

type 'a cloture = Infinity | N of 'a 
		   
module Tropical: (Dioid with type t = int cloture)

module Discounting : (Semiring with type t = (int * int) cloture * int cloture)
		   
module KShortest(K:Int)(S:Dioid): (Semiring with type t = S.t list)
module KDiffShortest(K:Int)(S:Dioid):(Semiring with type t = S.t list)

