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

let root = ref ""

let add_path p =
  if !prefix = "Coq" then root := p;
  add_rec_path p (if !prefix = "" then [] else [!prefix])

let speclist = Arg.align
  [ "-r", Arg.Set_string prefix, "Id";
    "--root", Arg.Set_string prefix, "Id set the dirpath root as Id\n";
    "-I", Arg.String add_path, "path";
    "--include", Arg.String add_path, "path add path using the current dirpath root\n"; ]

let rec path_to_string name suffixe = match name with
    [] -> failwith "empty path"
  | [n] -> n ^ suffixe
  | n::q -> path_to_string q ("_" ^ n ^ suffixe)

let translate filename =
  let channel = open_in_bin filename in
    ignore (input_binary_int channel); (* magic number *)
    let (md:Check.library_disk) = Marshal.from_channel channel in
      close_in channel;
      (* Putting dependancies in the environment *)
      print_string  (path_to_string md.md_name ".lua" ^ ":");
      let needed = md.md_deps in
	List.iter
	  (fun (dir,m) ->
	     if dir <> md.md_name then print_string (" " ^ path_to_string dir ".lua"))
	  needed;
      print_newline ();
      print_endline ("all: " ^ path_to_string md.md_name ".lua")


let _ =
  (*  add_rec_path "/usr/lib/coq/theories" ["Coq"];*)
  Arg.parse speclist translate
    "Show dependancies of Coq libraries\nUsage: coqine [options] filenames\n\nIf you want to use the coq library,\nuse --prefix Coq -I path_to_coq_dir_theories\n\nfilenames:\tcoq binary files (.vo)\n\nOptions:"
