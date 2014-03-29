open Types

(* *** Marshalisation *)

type dko = {
  name:string;
  version:string;
  dependencies:string list;
  table: rw_infos H.t;
}

val marshal  : loc -> string -> rw_infos H.t -> string list -> unit

val unmarshal : loc -> string -> dko
