(** Basic Datatypes *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

(** type of identifiers (hash-consing) *)
type ident

(** pp_ident [fmt] [id] print the identifier [id] on the formatter [fmt] *)
val pp_ident : Format.formatter -> ident -> unit

(** mkd_ident [str] casts a string [str] to an identifier *)
val mk_ident : string -> ident

(** ident_eq [id] [id'] checks if the two identifiers [id] and [id'] are equals *)
val ident_eq : ident -> ident -> bool

(** string_of_ident [id] returns a string of the identifier [id] *)
val string_of_ident : ident -> string

(** type of module identifers *)
type mident

(** pp_ident [fmt] [id] print the identifier [id] on the formatter [fmt] *)
val pp_mident : Format.formatter -> mident -> unit

(** mk_ident [str] casts a string [str] to an module identifier *)
val mk_mident : string -> mident

(** mident_eq [md] [md'] checks if the two modules identifiers [mid] and [mid'] are equals *)
val mident_eq : mident -> mident -> bool

(** string_of_ident [id] returns a string of the identifier [id] *)
val string_of_mident : mident -> string

(** type for constant names such as [foo.bar] *)
type name

(** md [foo.bar] returns foo *)
val md : name -> mident

(** id [foo.bar] returns bar *)
val id : name -> ident

(** mk_name foo bar returns the foo.bar *)
val mk_name : mident -> ident -> name

(** name_eq [n] [n'] checks if the two names [n] and [n'] are equals *)
val name_eq : name -> name -> bool

(** pp_name [fmt] [n] print the name [n] on the formatter [fmt] *)
val pp_name : Format.formatter -> name -> unit

(** qmark is a special identifier for unification variables *)
val qmark : ident

(** dmark is a meaningless identifier *)
val dmark : ident
(** The kernel may introduce such identifiers when creating new de Bruijn indices *)

(** {2 Lists with Length} *)

module LList : sig
  type +'a t = private {
    len : int;
    lst : 'a list;
  }

  val cons : 'a -> 'a t -> 'a t
  val nil : 'a t
  val is_empty : _ t -> bool
  val len : _ t -> int
  val lst : 'a t -> 'a list
  val of_list : 'a list -> 'a t

  val make : len:int -> 'a list -> 'a t

  (** make_unsafe [n] [l] is as make [n] [l] without checking that the length of [l] is [n] *)
  val make_unsafe : len:int -> 'a list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val append_l : 'a t -> 'a list -> 'a t
  val nth : 'a t -> int -> 'a
  val remove : int -> 'a t -> 'a t
end

(** {2 Localization} *)

(** type of locations *)
type loc

(** dloc is the default location *)
val dloc                : loc

(** mk_loc l c build the location where [l] is the line and [c] the column *)
val mk_loc              : int -> int -> loc

val of_loc              : loc -> (int*int)

val add_path       : string -> unit
val get_path       : unit -> string list

(** {2 Error Datatype} *)

type ('a,'b) error =
  | OK of 'a
  | Err of 'b

val map_error : ('a -> 'b) -> ('a,'c) error -> ('b,'c) error
val bind_error : ('a -> ('b,'c) error) -> ('a,'c) error -> ('b,'c) error
val map_error_list : ('a -> ('b,'c) error) -> 'a list -> ('b list,'c) error

(** {2 Debug} *)

(** print informations on the standard error channel *)
val debug_mode : int ref

val set_debug_mode : int -> unit

val debug : int -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** {2 Misc} *)

val fold_map : ('b->'a-> ('c*'b)) -> 'b -> 'a list -> ('c list*'b)

val bind_opt : ('a -> 'b option) -> 'a option -> 'b option

val map_opt : ('a -> 'b) -> 'a option -> 'b option

val array_for_all : ('a -> bool) -> 'a array -> bool

type 'a printer = Format.formatter -> 'a -> unit

val string_of : (Format.formatter -> 'a -> unit) -> 'a -> string

(** pp_list [sep] [fp] [l] print a list [\[l1 ; ... ln\]] by applying [fp] on each element and use se separator [sep] between elements *)
val pp_list   : string -> 'a printer -> 'a list printer
val pp_arr    : string -> 'a printer -> 'a array printer
val pp_option : string -> 'a printer -> 'a option printer
