open Basic
open Term

(** Rewrite rules *)

(** {2 Patterns} *)

type pattern =
  | Var         of loc * ident * int * pattern list
      (** l x i [x1 ; x2 ; ... ; xn ] where [i] is the position of x inside the context
          of the rule *)
  | Pattern     of loc * ident * ident * pattern list
      (** l md id [p1 ; p2 ; ... ; pn ] where [md.id] is a constant *)
  | Lambda      of loc * ident * pattern
      (** lambda abstraction *)
  | Brackets    of term
      (** te where [te] is convertible to the pattern matched *)

val get_loc_pat : pattern -> loc

val pattern_to_term : pattern -> term

val pp_pattern  : out_channel -> pattern -> unit

type pattern2 =
  | Joker2
  | Var2         of ident * int * int list
  | Lambda2      of ident * pattern2
  | Pattern2     of ident * ident * pattern2 array
  | BoundVar2    of ident * int * pattern2 array

(** {2 Contexts} *)

type untyped_context = (loc * ident) list

type typed_context = ( loc * ident * term ) list

val pp_context  : out_channel -> typed_context -> unit

(** {2 Rewrite Rules} *)

type 'a rule = 'a * pattern * term

type untyped_rule = untyped_context rule

type typed_rule = typed_context rule

type constr =
  | Linearity of int * int (* change to int*int ? *)
  | Bracket of int * term (* change to int*term ? *)

type rule_infos = {
  l:loc;
  ctx:typed_context;
  md:ident;
  id:ident;
  args:pattern list;
  rhs:term;
  esize:int;
  l_args:pattern2 array;
  constraints:constr list;
}

val pp_untyped_rule  : out_channel -> untyped_rule -> unit
val pp_typed_rule    : out_channel -> typed_rule -> unit
val pp_rule_infos    : out_channel -> rule_infos -> unit

(** {2 Decision Trees} *)

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
  | Test    of pre_context * constr list * term * dtree option

val pp_dtree    : int -> out_channel -> dtree -> unit
val pp_rw       : out_channel -> (ident*ident*int*dtree) -> unit
