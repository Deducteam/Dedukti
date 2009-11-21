open Dkterm
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
	      let stmts = List.concat (List.map (sb_decl_trans label) declarations) in
		output_module stdout stmts;
		print_endline (";Finished module " ^ match label with _,l,_ -> l)
	  | _ -> ()
	      
let _ =  try Arg.parse [] translate
  "CoqInE\nUsage: coqine filenames\n\tfilenames: coq binary files (.vo)" 
with e -> output_module stderr (flush_decl ()); raise e
