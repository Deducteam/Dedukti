let bind_opt f = function
  | None -> None
  | Some x -> f x

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)
