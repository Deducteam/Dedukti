open EuTerms
open Coqine
open Declarations
open Term
open Check

let translate filename =
  let channel = open_in_bin filename in
    ignore (input_binary_int channel); (* magic number *)
    let (md:Check.library_disk) = Marshal.from_channel channel in
      close_in channel;
      let (_,mb,_,_) = md.md_compiled in
	match mb.mod_expr with 
	    Some (SEBstruct (label, declarations)) ->
	      List.iter (sb_decl_trans label) declarations;
	      print_endline (";Finished module " ^ match label with _,l,_ -> l)
	  | _ -> ()
	      
let _ =  try Arg.parse [] translate
  "CoqInE\nUsage: coqine filenames\n\tfilenames: coq binary files (.vo)" 
with e -> flush_decl stdout; raise e
