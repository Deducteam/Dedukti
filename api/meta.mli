(** The [dkmeta] library is an extension of [dedukti] that allows the
   user to normalize terms according to meta rules. Meta rules are
   written in the [dedukti] syntax. [dkmeta] also offers a way to
   reify the syntax of [dedukti] in [dedukti]. This allows you to get
   around the limitations of the rewrite engine offers par Dedukti:
   you can rewrite products, static symbols... The overhead introduced
   by this reification is linear if you choose the [LF]
   encoding. However, you might create your own encoding for your own
   goals. *)

open Kernel
open Parsers

module type ENCODING = sig
  (** module name of the encoding *)
  val md : Basic.mident

  (** List of declarations *)
  val entries : unit -> Entry.entry list

  (** If [safe], the encoding needs type checking. Type checking is done before encoding. *)
  val safe : bool

  (** Signature of the encoding. Redudant with [entries] *)
  val signature : Signature.t

  (** [encode_term sg ctx t] encodes a term [t]. [sg] and [ctx] are used only if [safe] is true *)
  val encode_term :
    ?sg:Signature.t -> ?ctx:Term.typed_context -> Term.term -> Term.term

  (** [decode_term t] decodes a term [t] *)
  val decode_term : Term.term -> Term.term

  (** [encode_rule sg r] encodes a rule [r]. [sg] is used only if [safe] is true *)
  val encode_rule : ?sg:Signature.t -> 'a Rule.rule -> 'a Rule.rule
end

module RNS : Set.S with type elt = Rule.rule_name

(** [cfg] configures [dkmeta]. [meta_rules] contains all the rules
   used for normalization. If [meta_rules] = None, then all the rules
   in the signature [sg] are used. It is left to the user to add the
   [meta_rules] in the signature. If you use an encoding, you have to
   add manually the signature of the encoding in the meta signature
   [sg]. Besides, all the meta_rules should be encoded beforethey are
   added in the signature. At the moment, [dkmeta] offers only one
   encoding. [dkmeta] nor [dedukti] won't check that the [meta rules]
   you add can be applied or not. If the normalization succeeded but
   you did not get the exepected result, probably that the rule was
   not in the signature. Be careful that the signature you use to
   normalize terms is not the same than the one you will use to type
   check your terms. *)
type cfg = {
  env : Env.t option;
  mutable meta_rules : RNS.t option;  (** Contains all the meta_rules. *)
  beta : bool;  (** If off, no beta reduction is allowed *)
  register_before : bool;
      (** entries are registered before they have been normalized *)
  encoding : (module ENCODING) option;
      (** Set an encoding before normalization *)
  decoding : bool;  (** If false, the term is not decoded after normalization *)
}

(** Initliaze a configuration with the following parameters:
    [env]        = None
    [meta_rules] = None
    [beta]       = true
    [encoding]   = None
    [decoding]   = true
    [register_before] = true *)
val default_config :
  ?env:Env.t ->
  ?meta_rules:RNS.t ->
  ?beta:bool ->
  ?encoding:(module ENCODING) ->
  ?decoding:bool ->
  ?register_before:bool ->
  unit ->
  cfg

(** Transform a [dkmeta] cfg to a [red_cfg] that can be used by the
   Rewrite Engine of Dedukti. *)
val red_cfg : cfg -> Reduction.red_cfg

(** Prefix each subterm with its construtor *)
module LF : ENCODING

(** Encodes products with HOAS *)
module PROD : ENCODING

(** Same as [LF] with type informations for application on product
   only. *)
module APP : ENCODING

val debug_flag : Basic.Debug.flag

val log : ('a, Format.formatter, unit, unit) format4 -> 'a

module MetaConfiguration :
  Processor.S with type t = Rule.partially_typed_rule list

(** [meta_of_rules rs cfg] adds the meta_rules [rs] in the
   configuration [cfg] *)
val meta_of_rules : Env.t -> Rule.partially_typed_rule list -> RNS.t

(** [meta_of_files ?env files] returns a set of rules declares in the
   files [files]. [env] is the environment that should contain the
   [meta] rewrite rules. If no environment is provided, an empty one
   with the mident ["meta"] is created. *)
val meta_of_files : ?env:Env.t -> string list -> RNS.t

val make_meta_processor :
  cfg ->
  post_processing:(Env.t -> Entry.entry -> unit) ->
  (module Processor.S with type t = unit)

(** [mk_term cfg ?env term] normalize a term according to the
   configuration [cfg] *)
val mk_term : cfg -> Env.t -> Term.term -> Term.term

(** [mk_entry env cfg entry] processes an entry according the meta
   configuration [cfg] and the current environment [env] *)
val mk_entry : Env.t -> cfg -> Entry.entry -> Entry.entry

type _ Processor.t += MetaRules : Rule.partially_typed_rule list Processor.t
