open Basic
open Term

(** Rewrite rules *)

(** {2 Patterns} *)

(** Basic representation of pattern. This representation does not ensure that it is a Miller's pattern. *)
type pattern =
  | Var      of loc * ident * int * pattern list
  (** DB variable. If the pattern list is not empty it is a Higher-Order variable. *)
  | Pattern  of loc * name * pattern list        (** Applied constant.    *)
  | Lambda   of loc * ident * pattern            (** Lambda abstraction.  *)
  | Brackets of term                             (** Bracket of a term.   *)


val get_loc_pat : pattern -> loc
(** [get_loc_pat pat] returns the location of a pattern. *)

val pattern_to_term : pattern -> term
(** [pattern_to_term pat] returns the term representation of [pat]. *)

(** {2 Rewrite Rules} *)

type rule_name =
  | Beta
  | Delta of name
  (** Rules associated to the definition of a constant *)
  | Gamma of bool * name
  (** User-defined rewrite rules. The first parameter indicates
     whether the name of the rule has been given by the user. *)

val rule_name_eq : rule_name -> rule_name -> bool

type 'a rule =
  {
    name: rule_name;
    (** name of the rule *)
    ctx : 'a context;
    (** context of the rule *)
    pat : pattern;
    (** pattern of the rule *)
    rhs : term
    (** Right-hand side of the rule *)
  }

val get_loc_rule : 'a rule -> loc
(** [get_loc_rule rule] returns the loc of the rule. *)

type untyped_rule = term option rule
(** Rule where the context is partially annotated with types. *)

type typed_rule = term rule
(** Rule where context is fully annotated with types *)

(** {2 Printing} *)

val pp_rule_name       : rule_name       printer
val pp_untyped_rule    : 'a rule         printer
val pp_typed_rule      : typed_rule      printer
val pp_part_typed_rule : untyped_rule    printer
val pp_pattern         : pattern         printer
