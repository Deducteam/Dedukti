open Basics
open Term

type pattern =
  | Var         of loc*ident*int*pattern list
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term
  | Joker       of loc

val get_loc_pat : pattern -> loc

val pattern_to_term : pattern -> term

type top = ident*pattern array

type rule = context * pattern * term
type frule = {
  l:loc; ctx:context; md:ident; id:ident; args:pattern list; rhs:term; }

type case =
  | CConst of int*ident*ident
  | CDB    of int*int
  | CLam

(* Abstract (from a stack (or a term list)) matching problem *)
type abstract_pb = { position2:int (*c*) ; dbs:int LList.t (*(k_i)_{i<=n}*) ; depth2:int }
(* It corresponds to the following matching problem (modulo beta):
 * stck.(c) ~? F( (DB k_0) ... (DB k_n) )
 * where F is the variable
 * *)

type pos = { position:int; depth:int }

(* Infos to build the context from the stack *)
type pre_context =
  | Syntactic of pos LList.t
  (* the list of positions in the stack corresponding to the context. *)
  | MillerPattern of abstract_pb LList.t
  (* the list of abstract problem which list of solutions gives the context. *)

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_context * (term*term) list * term * dtree option
