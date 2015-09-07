type acterm =
    AC_var of Basics.loc * Basics.ident * int * Basics.ident
  | AC_const of Basics.loc * Basics.ident * Basics.ident
  | AC_app of acterm * acterm list
  | AC_app2 of acterm * acterm Multi_set.ms
  | AC_lam of Basics.loc * Basics.ident * acterm option * acterm
  | AC_pi of Basics.loc * Basics.ident * acterm * acterm
  | AC_kind
  | AC_type of Basics.loc
val acterm_eq : acterm -> acterm -> bool
val acterm_cmpr : acterm -> acterm -> int
val lexico_rec : acterm list -> acterm list -> int
val string_of_acterm : acterm -> string
val mk_AC_var : Basics.loc -> Basics.ident -> int -> acterm
val mk_AC_varspe : Basics.ident -> int -> acterm
val mk_AC_const : Basics.loc -> Basics.ident -> Basics.ident -> acterm
val mk_AC_app : acterm -> acterm list -> acterm
val mk_AC_app2 : acterm -> acterm list -> acterm
val mk_AC_lam :
  Basics.loc -> Basics.ident -> acterm option -> acterm -> acterm
val mk_AC_pi : Basics.loc -> Basics.ident -> acterm -> acterm -> acterm
val is_acu : Term.term -> bool
val acterm_of_term : Term.term -> acterm
val acterm_of_pattern : Rule.pattern -> acterm
val term_of_acterm : acterm -> Term.term
val app_of_acapp2 : Term.term -> acterm list -> Term.term
val cut : acterm list -> acterm list -> int -> acterm list * acterm list
val is_occurs : acterm -> acterm -> bool
module AssocMap :
  sig type t = acterm val compare : acterm -> acterm -> int end
module Si :
  sig
    type key = AssocMap.t
    type 'a t = 'a Map.Make(AssocMap).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
val sub_term : Si.key Si.t -> Si.key -> Si.key
val sub_si : Si.key Si.t -> Si.key Si.t -> Si.key Si.t
val string_of_si : acterm Si.t -> string
val fusion_si : 'a Si.t -> 'a Si.t -> 'a Si.t
val diff_si : 'a Si.t -> 'b Si.t -> 'a Si.t
val eq2 : Si.key Si.t -> Si.key -> Si.key -> bool
val is_special_var : acterm -> bool
val put_voidAC : acterm -> acterm Si.t -> acterm Si.t
val permut_list :
  ('a * acterm) * acterm -> acterm Si.t -> (('a * acterm) * acterm Si.t) list
val notonlyVi_in : acterm -> bool
exception CantPurify
val purify : acterm -> acterm -> acterm Si.t * (acterm * acterm)
