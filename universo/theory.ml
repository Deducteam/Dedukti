module type THEORY =
sig

  val elaboration : Basic.mident -> Entry.entry -> Entry.entry
  val reconstruction : Export.model -> Entry.entry -> Entry.entry
  val univ_convertible :
    Signature.t ->
    term_convertible:(Term.term -> Term.term -> bool) ->
    Term.term ->
    Term.term ->
    Cfg.cstr option
end

module Matita = Matita

let to_theory s =
  if s = "cic" then
    (module Matita:THEORY)
  else
    failwith "unrecognized theory"
