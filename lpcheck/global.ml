
(* String sharing *)

module WS = Weak.Make(struct type t = string let equal a b = a = b let hash = Hashtbl.hash end)
let shash = WS.create 251
let hstring s = WS.merge shash s

(* Options *)

let name                        = ref ""
let quiet                       = ref true
let export                      = ref false

let set_name s = name := (hstring s)

(* Info messages *)

let print str  = prerr_string str ; flush stderr

let print_v str  = 
  if !quiet then () else ( prerr_string str ; flush stderr )
