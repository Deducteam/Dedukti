(** {2 Meta Dedukti}

   This file declares utilities which allows to use the rewrite rules
   of [dedukti] as a language to transform [dedukti] terms.

   [dkmeta] also offers a way to reify the syntax of [dedukti] in
   [dedukti]. This refication can be used to get around the
   limitations of the rewrite engine offers par [dedukti]. Using a
   reification, one can rewrite a product for example. The reification
   process is extensible in a way that any user of the library can
   write its own reification function. *)

open Kernel
open Parsers

(** {2 Reification } *)

(** The signature which implements a reification function. *)
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

(** {2 Meta } *)

(** The type of set which aims to contain meta rewrite rules. *)
module RNS : Set.S with type elt = Rule.rule_name

(** The [cfg] type parameterise how the meta rewrite rules will act on
   [dedukti] terms.

   If [meta_rules] is [None] then there is no meta rewrite rule but
   the strong normal form of every term will be computed.

   If [meta-rules] is [Some rules] then all the terms will be
   normalised according to this set of rewrite rules.

   Beta-reduction can be deactivated by setting [beta] to [false].

   If [encoding] is [Some (module E)] then the term will be reified
   according the function [E.encode_term] before being normalised.

   If [decoding] is [false] then after normalising terms, the term
   won't be unreified.

   If [register_before] is set to [false], terms will be registered in
   the signatured only after being normalised and reified if
   applicable. This aims to be used for terms which are not well-typed
   before normalisation but are after normalisation. This can be used
   to implement macros for example.

   The environment contains the internal representation of all the
   meta rewrite rules. This module maintains an invariant that the
   environment is consistent with the set of rewrite rules. *)
type cfg = private {
  env : Env.t option;
  mutable meta_rules : RNS.t option;  (** Contains all the meta_rules. *)
  beta : bool;  (** If off, no beta reduction is allowed *)
  register_before : bool;
      (** Entries are registered before they have been normalized *)
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

(** [red_cfg cfg] computes a rewrite engine configuration from the
   meta configuration [cfg]. This function is called every time we
   normalise a term. *)
val red_cfg : cfg -> Reduction.red_cfg

(** Reification which prefixes all the terms with its constructor. *)
module LF : ENCODING

(** A shallow encoding of products. This encoding allows to rewrite
   products. *)
module PROD : ENCODING

(** Same as [LF] with type informations for application on product
   only. *)
module APP : ENCODING

(** A debug flag to register logs specific for [dkmeta]. *)
val debug_flag : Basic.Debug.flag

(** A logging function for [dkmeta]. *)
val log : ('a, Format.formatter, unit, unit) format4 -> 'a

(** A processor which processes files containing the meta
   rewrite-rules. It is assumed that such file contains only rewrite
   rules. *)
module MetaConfiguration :
  Processor.S with type t = Rule.partially_typed_rule list

(** The processor associated to [MetaConfiguration]. *)
type _ Processor.t += MetaRules : Rule.partially_typed_rule list Processor.t

(** [meta_of_rules env rs] adds the meta_rules [rs] to the environment
   and returns the name of those rules as a set. *)
val meta_of_rules : Env.t -> Rule.partially_typed_rule list -> RNS.t

(** [meta_of_files ?env files] returns a set of rules declares in the
   files [files]. [env] is the environment that should contain the
   [meta] rewrite rules. If no environment is provided, an empty one
   with the mident ["meta"] is created. *)
val meta_of_files : ?env:Env.t -> string list -> RNS.t

(** [make_meta_processor cfg ~post_processing] returns a processor
   which will normalise every entry and for each entry after being
   processed, the function [post_processing] will be called. The
   normalisation is done according to the configuration [cfg]. *)
val make_meta_processor :
  cfg ->
  post_processing:(Env.t -> Entry.entry -> unit) ->
  (module Processor.S with type t = unit)

(** [mk_term cfg env term] normalize a term according to the
   configuration [cfg]. [env] is the environment used for type
   checking the term. *)
val mk_term : cfg -> Env.t -> Term.term -> Term.term

(** [mk_entry cfg env entry] processes an entry according the meta
   configuration [cfg] and the current environment [env] *)
val mk_entry : cfg -> Env.t -> Entry.entry -> Entry.entry
