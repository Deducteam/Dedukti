(** Basic Datatypes *)

(** {2 Identifiers (hashconsed strings)} *)

(** Internal representation of identifiers as hashconsed strings. *)

(** Type of identifiers (hash-consing) *)
type ident

val mk_ident : string -> ident
(** [mkd_ident str] casts a string [str] to an identifier *)

val ident_eq : ident -> ident -> bool
(** [ident_eq id id'] checks if the two identifiers [id] and [id'] are equals *)

val string_of_ident : ident -> string
(** [string_of_ident id] returns a string of the identifier [id] *)

(** type of module identifers *)
type mident

val mk_mident : string -> mident
(** [mk_ident str] casts a string [str] to an module identifier *)

val mident_eq : mident -> mident -> bool
(** [mident_eq md md'] checks if the two modules identifiers [mid] and [mid'] are equals *)

val string_of_mident : mident -> string
(** [string_of_ident id] returns a string of the identifier [id] *)

(** type for constant names such as [foo.bar] *)
type name

val md : name -> mident
(** [md foo.bar] returns foo *)

val id : name -> ident
(** [id foo.bar] returns bar *)

val mk_name : mident -> ident -> name
(** [mk_name foo bar] returns the identifier foo.bar *)

val name_eq : name -> name -> bool
(** [name_eq n n'] checks if the two names [n] and [n'] are equals *)

val dmark : ident
(** qmark is a special identifier for unification variables *)

(** The kernel may introduce such identifiers when creating new de Bruijn indices *)

(** {2 Lists with Length} *)

module LList : sig
  type +'a t = private {len: int; lst: 'a list}

  val cons : 'a -> 'a t -> 'a t

  val nil : 'a t

  val make : len:int -> 'a list -> 'a t

  val of_list : 'a list -> 'a t

  val of_array : 'a array -> 'a t

  val is_empty : _ t -> bool

  val len : _ t -> int

  val lst : 'a t -> 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t

  val append_l : 'a t -> 'a list -> 'a t

  val nth : 'a t -> int -> 'a

  val remove : int -> 'a t -> 'a t
end

(** {2 Localization} *)

(** type of locations *)
type loc

val dloc : loc
(** a dummy location *)

val mk_loc : int -> int -> loc
(** [mk_loc l c] builds the location where [l] is the line and [c] the column *)

val of_loc : loc -> int * int

val add_path : string -> unit

val get_path : unit -> string list

(** {2 Error Datatype} *)

type ('a, 'b) error = OK of 'a | Err of 'b

val map_error : ('a -> 'b) -> ('a, 'c) error -> ('b, 'c) error

val bind_error : ('a -> ('b, 'c) error) -> ('a, 'c) error -> ('b, 'c) error

val map_error_list : ('a -> ('b, 'c) error) -> 'a list -> ('b list, 'c) error

(** {2 Debug} *)

val set_debug_mode : int -> unit
(** Sets the level of information printing on the standard error channel
   <0 is quiet mode (no printing)
   0 is warning level (default)
   1 is verbose mode
   >1 is debugging mode  *)

val debug : int -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [debug i] prints information on the standard error channel
    if the selected debugging level is at least [i]. *)

val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Prints a warning when debugging level is at least 0. *)

(** {2 Misc} *)

val fold_map : ('b -> 'a -> 'c * 'b) -> 'b -> 'a list -> 'c list * 'b

val bind_opt : ('a -> 'b option) -> 'a option -> 'b option

val map_opt : ('a -> 'b) -> 'a option -> 'b option

(** Functions printing objects on the given formatter. *)
type 'a printer = Format.formatter -> 'a -> unit

val string_of : 'a printer -> 'a -> string
(** Prints to a string *)

val pp_ident : ident printer
(** Printing identifiers and names *)

val pp_mident : mident printer

val pp_name : name printer

val pp_loc : loc printer

val pp_list : string -> 'a printer -> 'a list printer
(** Printing each elements of arrays / lists using the separator [sep] between elements. *)

val pp_arr : string -> 'a printer -> 'a array printer

val pp_option : string -> 'a printer -> 'a option printer
(** Printing object with printer or default string when None. *)
