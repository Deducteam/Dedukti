(** A process module processes entries. It is parameterized by an environment. *)
module type S =
sig
  type t

  val handle_entry : Entry.entry -> unit

  val get_data : unit -> t
end


(** Provide a type checker for entries *)
module TypeChecker (E:Env.S)      : S with type t = unit

(** Only build a signature without type checking the entries *)
module SignatureBuilder (E:Env.S) : S with type t = Signature.t

(** Pretty prints entries *)
module EntryPrinter (E:Env.S)     : S with type t = unit

(** Computes dependencies *)
module Dependencies (E:Env.S)     : S with type t = Dep.t
