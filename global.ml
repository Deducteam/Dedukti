
open Types



(* *** Global Options *** *)

let name                        = ref empty
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let color                       = ref true
let out                         = ref stdout
let filename                    = ref None
(*let unsafe_mode                 = ref false*)
let display_db                   = ref false 

let set_name s = name := s
let set_out file = out := open_out file
let set_filename s = filename := Some s

(* *** Infos about the Rewrite System *** *)

(*
let linearity = ref true
let constant_applicative = ref true

let unset_linearity lc =
  if !constant_applicative || !unsafe_mode then linearity := false
  else
    raise (MiscError ( lc , "The rewrite system should be either linear or \
                              constant-applicative at type level." ))

let unset_constant_applicative lc =
  if !linearity || !unsafe_mode then constant_applicative := false
  else
    raise (MiscError ( lc , "The rewrite system should be either linear or\
                              constant-applicative at type level." ))
 *)

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
  if not !quiet then
    prerr_endline ( "[" ^ string_of_loc lc ^ "] " ^ Lazy.force str )

let vprint2 str  =
  if not !quiet then prerr_endline ( Lazy.force str )

let print_ok _ =
  match !filename with
    | None      -> eprint (green "Checked")
    | Some fn   ->
        eprint ("File " ^ fn ^ " was successfully " ^ green "Checked" ^ ".")

let warning lc str =
  eprint ( orange "WARNING " ^ string_of_loc lc ^ " " ^ str )

let error lc str =
  eprint ( red "ERROR " ^ string_of_loc lc ^ " " ^ str ) ;
  exit 1

let print_version _  = 
  sprint "Dedukti v2.2.1" (*FIXME do not forget to update*)

(* Commands *)

let get_one l str = function
  | [t] -> t
  | _   ->
      raise (MiscError (l,"Wrong number of\
                            arguments for command '#"^str^"' (Expected:1)"))

let get_two l str = function
  | [t1;t2]     -> (t1,t2)
  | _           ->
      raise (MiscError (l,"Wrong number of\
                            arguments for command '#"^str^"' (Expected:2)"))

let get_string l str = function
  | [PreId (_,p)] -> string_of_ident p
  | _             -> raise (MiscError (l,"Bad argument for command '#"^str^"'"))

let get_idents l str = function
  | [PreId (_,v)]    -> ( !name , v )
  | [PreQId (_,m,v)] -> ( m , v )
  | _                ->
      raise (MiscError (l,"Bad argument for command '#"^str^"'"))

let parse_cmd l str lst =
  if      str="WHNF"  then Whnf (get_one l str lst)
  else if str="HNF"   then Hnf (get_one l str lst)
  else if str="SNF"   then Snf (get_one l str lst)
  else if str="STEP"  then OneStep (get_one l str lst)
  else if str="CONV"  then let (t1,t2) = get_two l str lst in Conv (t1,t2)
  else if str="CHECK" then let (te,ty) = get_two l str lst in Check (te,ty)
  else if str="INFER" then Infer (get_one l str lst)
  else if str="PRINT" then Print (get_string l str lst)
  else if str="GDT"   then let (m,v) = get_idents l str lst in Gdt (m,v)
  else Other

