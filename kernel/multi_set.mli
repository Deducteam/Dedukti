(** definiton des types **)
type 'a elem = Elem of int * 'a
type 'a ms = Multiset of 'a elem list

(** taille d'un multiset **)
val len_Multiset : 'a ms -> int

(** compare deux multiset **)
val cmpr_Multiset : ('a -> 'b -> int) -> 'a ms -> 'b ms -> int

(** effectue la difference entre deux multiset **)
val diff : ('a -> 'a -> int) -> 'a ms -> 'a ms -> 'a ms

(** renvoie deux multiset tel que il n'ont aucun element en commun **)
val del_same : ('a -> 'a -> int) -> 'a ms -> 'a ms -> 'a ms * 'a ms

(** fusionne deux multiset **)
val fusion : ('a -> 'a -> int) -> 'a ms -> 'a ms -> 'a ms

(** scinde un multiset en deux **)
val scinde : 'a ms -> 'a ms * 'a ms

(** fusionne recursivement les elements commun d'un multiset **)
val fusion_rec : ('a -> 'a -> int) -> 'a ms -> 'a ms

(** construit un multiset **)
val mk_Multiset : ('a -> 'a -> int) -> 'a list -> 'a ms

(** construit une liste delement a partir d'un multiset **)
val list_of_multiset : 'a ms -> 'a list


