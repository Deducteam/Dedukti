
(* Options *)

let name                        = ref ""
let quiet                       = ref false
let export                      = ref false

(* Debug *)

let msg str  = if !quiet then () else ( prerr_string str ; flush stderr )
