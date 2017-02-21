open Basic
open Term

(** Rewrite rules *)

(** {2 Patterns} *)

type pattern =
  | Var         of loc*ident*int*pattern list
      (** l x i [x1 ; x2 ; ... ; xn ] where [i] is the position of x inside the context
          of the rule *)
  | Pattern     of loc*ident*ident*pattern list
      (** l md id [p1 ; p2 ; ... ; pn ] where [md.id] is a constant *)
  | Lambda      of loc*ident*pattern
      (** lambda abstraction *)
  | Brackets    of term
      (** te where [te] is convertible to the pattern matched *)

val get_loc_pat : pattern -> loc

val pattern_to_term : pattern -> term

val pp_pattern  : out_channel -> pattern -> unit

type top = ident*pattern array

type pattern2 =
  | Joker2
  | Var2         of ident*int*int list
  | Lambda2      of ident*pattern2
  | Pattern2     of ident*ident*pattern2 array
  | BoundVar2    of ident*int*pattern2 array

(** {2 Contexts} *)

type context = ( loc * ident * term ) list

val pp_context  : out_channel -> context -> unit

(** {2 Rewrite Rules} *)

type rule = (loc*ident) list * pattern * term
(** type of untyped rules *)

type rule2 = context * pattern * term
(** type of typed rules : the variables in the context are typed *)

type constr =
  | Linearity of term*term (* change to int*int ? *)
  | Bracket of term*term (* change to int*term ? *)

type rule_infos = {
  l:loc;
  ctx:context;
  md:ident;
  id:ident;
  args:pattern list;
  rhs:term;
  esize:int;
  l_args:pattern2 array;
  constraints:constr list;
}

val pp_rule     : out_channel -> rule -> unit
val pp_rule2    : out_channel -> rule2 -> unit
val pp_frule    : out_channel -> rule_infos -> unit

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
