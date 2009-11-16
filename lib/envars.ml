(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* This file gathers environment variables needed by Coq to run (such
   as coqinelib) *)

let coqinebin () = 
  if !Flags.boot || Coqine_config.local
  then Filename.concat Coqine_config.coqinesrc "bin"
  else System.canonical_path_name (Filename.dirname Sys.executable_name)

let guess_coqinelib () = 
  let file = "states/initial.coq" in
    if Sys.file_exists (Filename.concat Coqine_config.coqinelib file) 
    then Coqine_config.coqinelib
    else 
      let coqbin = System.canonical_path_name (Filename.dirname Sys.executable_name) in
      let prefix = Filename.dirname coqbin in
      let rpath = if Coqine_config.local then [] else 
	  (if Coqine_config.arch = "win32" then ["lib"] else ["lib";"coq"]) in
      let coqinelib = List.fold_left Filename.concat prefix rpath in
	if Sys.file_exists (Filename.concat coqinelib file) then coqinelib else
	  Util.error "cannot guess a path for Coq libraries; please use -coqinelib option"
	  
let coqinelib () = 
  if !Flags.coqinelib_spec then !Flags.coqinelib else
    (if !Flags.boot then Coqine_config.coqinesrc else guess_coqinelib ())

let path_to_list p =
  let sep = if Sys.os_type = "Win32" then ';' else ':' in
    Util.split_string_at sep p 

let rec which l f =
  match l with
    | [] -> raise Not_found
    | p :: tl -> 
	if Sys.file_exists (Filename.concat p f) 
	then p 
	else which tl f
 
let guess_camlbin () = 
  let path = try Sys.getenv "PATH" with _ -> raise Not_found in 
  let lpath = path_to_list path in
    which lpath "ocamlc"

let guess_camlp4bin () = 
  let path = try Sys.getenv "PATH" with _ -> raise Not_found in 
  let lpath = path_to_list path in
    which lpath Coqine_config.camlp4

let camlbin () = 
  if !Flags.camlbin_spec then !Flags.camlbin else
    if !Flags.boot then Coqine_config.camlbin else
      try guess_camlbin () with _ -> Coqine_config.camlbin

let camllib () = 
  if !Flags.boot
  then Coqine_config.camllib
  else 
    let camlbin = camlbin () in 
    let com = (Filename.concat camlbin "ocamlc") ^ " -where" in
    let _,res = System.run_command (fun x -> x) (fun _ -> ()) com in
    Util.strip res

(* TODO : essayer aussi camlbin *)
let camlp4bin () = 
  if !Flags.camlp4bin_spec then !Flags.camlp4bin else
    if !Flags.boot then Coqine_config.camlp4bin else
      try guess_camlp4bin () with _ -> Coqine_config.camlp4bin

let camlp4lib () = 
  if !Flags.boot
  then Coqine_config.camlp4lib
  else 
    let camlp4bin = camlp4bin () in 
    let com = (Filename.concat camlp4bin Coqine_config.camlp4) ^ " -where" in
    let _,res = System.run_command (fun x -> x) (fun _ -> ()) com in
    Util.strip res

    
