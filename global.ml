open Types

let name                = ref empty
let run_on_stdin        = ref false
let file                = ref "no_file"
let version             = "2.2.1"

let debug_level         = ref 0
let out                 = ref stdout
let export              = ref false
let ignore_redecl       = ref false
let color               = ref true
let autodep             = ref false

let print_std = Printf.printf
let print_out fmt = Printf.fprintf !out fmt

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s
let green = colored 2
let orange = colored 3
let red = colored 1

let success fmt = 
  prerr_string (green "SUCCESS ") ;
  Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt

let prerr_loc lc = 
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c

let debug lvl lc fmt = 
  if lvl <= !debug_level then ( 
    prerr_loc lc ;  
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt )
  else Printf.ifprintf stderr fmt

let debug_no_loc lvl fmt = 
  if lvl <= !debug_level then
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  else Printf.ifprintf stderr fmt

let warning lc fmt = 
  prerr_string (orange "WARNING ") ; 
  prerr_loc lc ;
  Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt

let fail lc fmt = 
  prerr_string (red "ERROR ") ;
  prerr_loc lc;
  Printf.kfprintf (fun _ -> prerr_newline () ; raise Exit) stderr fmt

(* Commands *)
(*FIXME*)
let get_one l str = function
  | [t] -> t
  | _   ->
      fail l "Wrong number of arguments for command '#%s' (Expected:1)." str

let get_two l str = function
  | [t1;t2]     -> (t1,t2)
  | _           ->
      fail l "Wrong number of arguments for command '#%s' (Expected:2)." str

let get_string l str = function
  | [PreId (_,p)] -> string_of_ident p
  | _             -> fail l "Bad argument for command '#%s'." str

let get_idents l str = function
  | [PreId (_,v)]    -> ( !name , v )
  | [PreQId (_,m,v)] -> ( m , v )
  | _                -> fail l "Bad argument for command '#%s'" str

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
