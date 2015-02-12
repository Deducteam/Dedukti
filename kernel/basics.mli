(** Basic Datatypes *)

val pp_list     : string -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val string_of_ident : ident -> string
val pp_ident : out_channel -> ident -> unit
val print_ident : Format.formatter -> ident -> unit
val hstring : string -> ident
val ident_eq : ident -> ident -> bool
val qmark : ident

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
  val make_unsafe : len:int -> 'a list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val append_l : 'a t -> 'a list -> 'a t
  val nth : 'a t -> int -> 'a
  val remove : int -> 'a t -> 'a t
end

(** {2 Localization} *)

type loc
val dloc                : loc
val mk_loc              : int -> int -> loc
(** mk_loc [line] [column] *)
val of_loc              : loc -> (int*int)

val add_path       : string -> unit
val get_path       : unit -> string list

type ('a,'b) error =
  | OK of 'a
  | Err of 'b

val map_error : ('a -> 'b) -> ('a,'c) error -> ('b,'c) error
val bind_error : ('a -> ('b,'c) error) -> ('a,'c) error -> ('b,'c) error
val map_error_list : ('a -> ('b,'c) error) -> 'a list -> ('b list,'c) error
(** Print in stderr *)
val debug : ('a, out_channel, unit) format -> 'a
