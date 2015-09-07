type coeff = Coefficient of int list
type equat = Equation of coeff * coeff
val mk_Equat : int list -> int list -> equat
module VectMod :
  sig type t = Vect of int list * int list val compare : 'a -> 'a -> int end
module VectSet :
  sig
    type elt = VectMod.t
    type t = Set.Make(VectMod).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
val canonique : int -> int -> int list
val somme : VectMod.t -> VectMod.t -> VectMod.t
val d : VectMod.t -> equat -> int
val mk_Vect_g : int -> equat -> VectMod.t
val mk_Vect_d : int -> equat -> VectMod.t
val eq_Vect : VectMod.t -> VectMod.t -> bool
val plus_grand' : 'a list -> 'a list -> bool
val plus_grand : VectMod.t -> VectMod.t -> bool
val not_plus_grand : VectMod.t -> VectMod.t -> bool
val is_minimal_in : VectMod.t -> VectSet.t -> bool
val q1 : VectMod.t list -> int -> equat -> VectMod.t list
val q2 : VectMod.t list -> int -> equat -> VectMod.t list
val fq : VectMod.t list -> equat -> VectMod.t list
val ensemble_Qk : VectSet.t -> equat -> VectSet.t
val fm : VectSet.t -> equat -> VectSet.t
val ensemble_Mk : VectSet.t -> equat -> VectSet.t
val fp : VectSet.t -> VectSet.t -> VectSet.t
val ensemble_Pk : VectSet.t -> VectSet.t -> VectSet.t
val init_P1 : equat -> VectSet.t
val step_kp1 : VectSet.t -> VectSet.t -> equat -> VectSet.t * VectSet.t
val step : VectSet.t -> VectSet.t -> equat -> VectSet.t
val procedure : equat -> VectSet.t
val equ : equat
