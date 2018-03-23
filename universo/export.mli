module Z3 :
sig

  val solve : Constraints.ConstraintsSet.t -> int * Reconstruction.model

end
