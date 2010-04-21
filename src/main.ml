open Dkterm
open Coqine
open Names
open Declarations
open Term
open Check

let add_rec_path ~unix_path:dir ~coq_root:coq_dirpath =
  let dirs = System.all_subdirs dir in
  let prefix = repr_dirpath coq_dirpath in
  let convert_dirs (lp,cp) =
    (lp,make_dirpath (List.rev cp@prefix)) in
  let dirs = Util.map_succeed convert_dirs dirs in
    List.iter Check.add_load_path dirs;
    Check.add_load_path (dir,coq_dirpath)
      

let prefix = ref ""

let add_path p = 
  add_rec_path p (if !prefix = "" then [] else [!prefix])

let pp_obj = ref new prefix_pp

let pp_prefix () = pp_obj := new prefix_pp

let pp_external () = pp_obj := new external_pp


let speclist = Arg.align 
  [ "-r", Arg.Set_string prefix, "Id";
    "--root", Arg.Set_string prefix, "Id set the dirpath root as Id\n";
    "-I", Arg.String add_path, "path";
    "--include", Arg.String add_path, "path add path using the current dirpath root\n";
    "-p", Arg.Unit pp_prefix, "";
    "--prefix-notation", Arg.Unit pp_prefix, " use Dedukti prefix syntax (faster parsing, default)\n";
    "-h", Arg.Unit pp_external, "";
    "--external", Arg.Unit pp_external, " use Dedukti external syntax (more human readable)\n";
]

let translate filename =
  let channel = open_in_bin filename in
    ignore (input_binary_int channel); (* magic number *)
    let (md:Check.library_disk) = Marshal.from_channel channel in
      close_in channel;
      let ml = (md.md_name, filename) in
	(* Putting dependancies in the environment *)
      let needed = List.rev (intern_library LibrarySet.empty ml []) in
	List.iter 
	  (fun (dir,m) -> 
	     if dir <> md.md_name then
	       Safe_typing.unsafe_import
                 m.library_filename m.library_compiled m.library_digest)
	  needed;
	Coqine.base_env := Safe_typing.get_env ();
      let (_,mb,_,_) = md.md_compiled in
	match mb.mod_expr with 
	    Some (SEBstruct (label, declarations)) ->
	      let stmts = List.concat (List.map (sb_decl_trans label) declarations) in
		!pp_obj#output_module stdout stmts;
		(*print_endline (";Finished module " ^ match label with _,l,_ -> l);
		print_endline (";Local Variables:\n;  mode: tuareg\n;End:")*)
	  | _ -> ()
	      
let _ =  
(*  add_rec_path "/usr/lib/coq/theories" ["Coq"];*)
  Arg.parse speclist translate
    "CoqInE\nUsage: coqine [options] filenames\n\nIf you want to use the coq library,\nuse --root Coq -I path_to_coq_dir_theories\n\nfilenames:\tcoq binary files (.vo)\n\noptions:" 
