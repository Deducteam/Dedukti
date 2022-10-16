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

(** A shallow encoding of products. This encoding allows to rewrite
   products. *)
module PROD : ENCODING

(** Reification which prefixes all the terms with its constructor. *)
module LF : ENCODING

(** Same as [LF] with type informations for application on product
   only. *)
module APP : ENCODING

(** {2 Meta configuration } *)

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
type cfg

(** Initliaze a configuration with the following parameters:
    [meta_rules] = None
    [beta]       = true
    [encoding]   = None
    [decoding]   = true
    [register_before] = true *)
val default_config :
  ?meta_rules:Rule.partially_typed_rule list ->
  ?beta:bool ->
  ?encoding:(module ENCODING) ->
  ?decoding:bool ->
  ?register_before:bool ->
  load_path:Files.t ->
  unit ->
  cfg

(** [add_rules cfg rules] add the rules to the meta configuration. *)
val add_rules : cfg -> Rule.partially_typed_rule list -> unit

(** A processor which processes files containing the meta
   rewrite-rules. It is assumed that such file contains only rewrite
   rules. *)
module MetaConfiguration :
  Processor.S with type t = Rule.partially_typed_rule list

(** The processor associated to [MetaConfiguration]. *)
type _ Processor.t += MetaRules : Rule.partially_typed_rule list Processor.t

(** [parse_meta_files files] returns the list of rules declares in the
   files [files].  *)
val parse_meta_files : string list -> Rule.partially_typed_rule list

(** {2 Meta processing } *)

(** [make_meta_processor cfg ~post_processing] returns a processor
   which will normalise every entry according to the configuration
   [cfg].

   The function [post_processing] is called on each entry after being
   processed. *)
val make_meta_processor :
  cfg ->
  post_processing:(Env.t -> Entry.entry -> unit) ->
  (module Processor.S with type t = unit)

(** [mk_term ?env cfg term] normalize a term according to the
   configuration [cfg]. [env] is the environment used for type
   checking the term.

   The [env] argument is mandatory if a safe encoding is used or if
   the normalisation strategy is done via the type checking
   environment (i.e. no meta rules were provided). *)
val mk_term : ?env:Env.t -> cfg -> Term.term -> Term.term

(** [mk_entry cfg env entry] processes an entry according the meta
   configuration [cfg] and the current environment [env] *)
val mk_entry : cfg -> Env.t -> Entry.entry -> Entry.entry

(** {2 Debugging/Logging } *)

(** A debug flag to register logs specific for [dkmeta]. *)
val debug_flag : Basic.Debug.flag

(** A logging function for [dkmeta]. *)
val log : ('a, Format.formatter, unit, unit) format4 -> 'a
