module Z3 :
sig

  val solve : Constraints.BasicConstraints.ConstraintsSet.t -> Constraints.Reconstruction.model

end
