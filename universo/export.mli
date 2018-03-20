module Z3 :
sig

  val solve : Constraints.Naive.ConstraintsSet.t -> int * Reconstruction.model

end
