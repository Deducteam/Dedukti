
(* *** String sharing *** *)

module WS = Weak.Make(struct type t = string let equal a b = a = b let hash = Hashtbl.hash end)
let shash = WS.create 251
let hstring s = WS.merge shash s

(* *** Global Options *** *)

let name                        = ref ""
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let out                         = ref stdout (* for dk2mmt *)

let set_name s = 
  name := (hstring s)

let set_out file = 
  out := open_out file

(* *** Info messages *** *)

(* Print a string on standard output *)           
let sprint str  = print_string str ; flush stdout
                                       
(* Print a string on standard error *)           
let eprint str  = prerr_string str ; flush stderr

(* Print a string on standard error if in verbose mode *)           
let vprint str  = 
  if not !quiet then ( prerr_string str ; flush stderr )

(* Print an error message and exit *)                           
let error lc e str = 
  eprint ("\n\027[31m["^e^"]\027[m" ^ Error.string_of_loc lc ^ " " ^ str ^ "\n");
  exit 1 


