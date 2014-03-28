open Types

(* *** Marshalisation *)

type dko = {
  version:string;
  dependencies:string list;
  table: rw_infos H.t;
}

val marshal  : loc -> string -> rw_infos H.t -> string list -> unit

val unmarshal : loc -> string -> dko
