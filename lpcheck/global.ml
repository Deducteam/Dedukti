
(* Sharing string *)

module S = Weak.Make(struct type t = string let equal a b = a = b let hash = Hashtbl.hash end)
let shash = S.create 251
let hstring s = S.merge shash s

(* Options *)

let name                        = ref ""
let quiet                       = ref false
let export                      = ref false

let set_name s = name := (hstring s)

(* Debug *)

let msg str  = if !quiet then () else ( prerr_string str ; flush stderr )
