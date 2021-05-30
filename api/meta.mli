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

module type ENCODING =
sig
  val md : Basic.mident
  (** module name of the encoding *)

  val entries : unit -> Entry.entry list
  (** List of declarations *)

  val safe : bool
  (** If [safe], the encoding needs type checking. Type checking is done before encoding. *)

  val signature : Signature.t
  (** Signature of the encoding. Redudant with [entries] *)

  val encode_term : ?sg:Signature.t -> ?ctx:Term.typed_context -> Term.term -> Term.term
  (** [encode_term sg ctx t] encodes a term [t]. [sg] and [ctx] are used only if [safe] is true *)

  val decode_term : Term.term -> Term.term
  (** [decode_term t] decodes a term [t] *)

  val encode_rule : ?sg:Signature.t -> 'a Rule.rule -> 'a Rule.rule
  (** [encode_rule sg r] encodes a rule [r]. [sg] is used only if [safe] is true *)
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
  mutable meta_rules  : RNS.t list option;
  (** Contains all the meta_rules. *)
  beta                : bool;
  (** If off, no beta reduction is allowed *)
  register_before     : bool;
  (** entries are registered before they have been normalized *)
  encode_meta_rules   : bool;
  (** The encoding is used on the meta rules first except for products *)
  encoding            : (module ENCODING) option;
  (** Set an encoding before normalization *)
  decoding            : bool;
  (** If false, the term is not decoded after normalization *)
  env                 : Env.t
}

(** Initliaze a configuration with the following parameters:
    [meta_rules] = None
    [beta]       = true
    [encoding]   = None
    [env]        = empty_signature (in particular the name is the empty string) *)
val default_config : cfg

(** Transform a [dkmeta] cfg to a [red_cfg] that can be used by the
   Rewrite Engine of Dedukti. *)
val red_cfg : cfg -> Reduction.red_cfg list

module LF : ENCODING
(** Prefix each subterm with its construtor *)

module PROD : ENCODING
(** Encodes products with HOAS *)

module APP : ENCODING
(** Same as [LF] with type informations for application on product
   only. *)

val debug_flag : Basic.Debug.flag

module MetaConfiguration : Processor.S with type t = Rule.partially_typed_rule list

val meta_of_rules: ?staged:bool -> Rule.partially_typed_rule list -> cfg -> cfg
(** [meta_of_rules rs cfg] adds the meta_rules [rs] in the
   configuration [cfg] *)

val meta_of_files : ?cfg:cfg -> string list -> cfg
(** [meta_of_files ?cfg files] returns a configuration from the meta
   rules declares in the files [files]*)

val make_meta_processor :
  cfg -> post_processing:(Env.t -> Entry.entry -> unit) ->
  (module Processor.S with type t = unit)

val mk_term      : cfg -> ?env:Env.t -> Term.term -> Term.term
(** [mk_term cfg ?env term] normalize a term according to the
   configuration [cfg] *)

val mk_entry : Env.t -> cfg -> Entry.entry -> Entry.entry
(** [mk_entry env cfg entry] processes an entry according the meta
   configuration [cfg] and the current environment [env] *)

type _ Processor.t += MetaRules : Rule.partially_typed_rule list Processor.t
