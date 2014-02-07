
open Types

(* *** Global Options *** *)

let name                        = ref empty
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let color                       = ref true
let out                         = ref stdout (* for dk2mmt *)
let filename                    = ref None
let unsafe_mode                 = ref false

let set_name s =
  name := s

let set_out file =
  out := open_out file

let set_filename s =
  filename := Some s

(* *** Info messages *** *)

let string_of_loc lc =
  match !filename with
    | None      -> string_of_loc lc
    | Some fn   -> "file:" ^ fn ^ " " ^ string_of_loc lc 

let sprint = print_endline
let eprint = prerr_endline

let print_out s = 
  output_string !out s ; 
  output_string !out "\n" 

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s

let green = colored 2
let orange = colored 3
let red = colored 1

let vprint lc str  =
  if not !quiet then prerr_endline ( "[" ^ string_of_loc lc ^ "] " ^ Lazy.force str )

let vprint2 str  =
  if not !quiet then prerr_endline ( Lazy.force str )


let print_ok _ =
  match !filename with
    | None      -> eprint (green "Checked")
    | Some fn   -> eprint ("File " ^ fn ^ " was successfully " ^ green "Checked" ^ ".")

let warning lc str =
  eprint ( orange "WARNING " ^ string_of_loc lc ^ " " ^ str )

let error lc str =
  eprint ( red "ERROR " ^ string_of_loc lc ^ " " ^ str ) ;
  exit 1

(* *** Infos about the Rewrite System *** *)

let linearity = ref true
let constant_applicative = ref true

let unset_linearity lc =
  (*warning lc "Non-linearity detected" ;*)
  if !constant_applicative || !unsafe_mode then linearity := false
  else
    raise (MiscError ( lc , "The rewrite system should be either linear or constant-applicative at type level." ))

let unset_constant_applicative lc = 
  (*warning lc "Type-level Constant-application detected" ;*)
  if !linearity || !unsafe_mode then constant_applicative := false
  else
    raise (MiscError ( lc , "The rewrite system should be either linear or constant-applicative at type level." ))


