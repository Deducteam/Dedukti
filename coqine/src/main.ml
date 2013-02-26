open Dkterm
open Coqine
open Names
open Declarations
open Term

let version () = print_endline "CoqInE version 0.0.1"; exit 0

let add_rec_path ~unix_path:dir ~coq_root:coq_dirpath =
  let dirs = System.all_subdirs dir in
  let prefix = repr_dirpath coq_dirpath in
  let convert_dirs (lp,cp) =
    (lp,make_dirpath (List.rev_map id_of_string cp@prefix)) in
  let dirs = Util.map_succeed convert_dirs dirs in
    List.iter Check.add_load_path dirs;
    Check.add_load_path (dir,coq_dirpath)


let prefix = ref "Coq"

let add_path p =
  add_rec_path p (make_dirpath (if !prefix = "" then [] else [id_of_string !prefix]))


let speclist = Arg.align
 [ "-r", Arg.Set_string prefix, "Id";
    "--root", Arg.Set_string prefix, "Id set the dirpath root as Id (default : Coq)\n";
    "-I", Arg.String add_path, "path";
    "--include", Arg.String add_path, "path add path using the current dirpath root\n";
    "-p", Arg.Unit pp_prefix, "";
    "--prefix-notation", Arg.Unit pp_prefix, " use Dedukti prefix syntax (faster parsing)\n";
    "-h", Arg.Unit pp_external, "";
    "--external", Arg.Unit pp_external, " use Dedukti external syntax (more human readable, default)\n";
    "--version", Arg.Unit version, " display version information"
]


let translate filename =
  let channel = open_in_bin filename in
  let i = input_binary_int channel in
  if i <> 8300 then Printf.fprintf stderr "Warning: the magic number of %s, which is %d, does not correspond to the version of Coq handled by CoqInE, which is 8.3. Expect strange behaviours." filename i;
  let (md:Check.library_disk) = Marshal.from_channel channel in
  close_in channel;
  let ml = (md.Check.md_name, filename) in
      (* Putting dependancies in the environment *)
  let needed = List.rev (Check.intern_library Check.LibrarySet.empty ml []) in
  let imports,env =
    List.fold_left
    (fun (im,env) (dir,m) ->
      if dir <> md.Check.md_name then
	try
	  dir::im,
	  Safe_typing.unsafe_import
	    m.Check.library_compiled m.Check.library_digest env
	with e ->
	  failwith ("unable to import module " ^ string_of_dirpath dir)
      else im,env )
    ([], Environ.empty_env) needed in
  let path,mb = Safe_typing.path_mb_compiled_library md.Check.md_compiled in
  print_decls (path_to_string path) imports (List.rev (mb_trans
				       {env = env;
					decls = [];
					functors = [];
					functor_parameters = [];
					mp_file = MPfile path;
					mp_nested = MPfile path;
					applied_modules = [];
					nested_modules = ""
				       } mb).decls)

let _ =
  (try
     let coqlib = Sys.getenv "COQLIB" in
     add_rec_path coqlib (make_dirpath ["Coq"]);
     Printf.fprintf stderr "Using %s as Coq standard library (set by $COQLIB)\n" coqlib
   with Not_found ->
     if Coqine_config.coq_library_path <> "" then
       (add_rec_path Coqine_config.coq_library_path (make_dirpath ["Coq"]);
	Printf.fprintf stderr "Using %s as Coq standard library (set at compile time)\n" Coqine_config.coq_library_path)
  );
  add_rec_path "./" (make_dirpath []);
  Arg.parse speclist translate
    "CoqInE\nUsage: coqine [options] filenames\n\nIf you want to use the coq library,\nuse --root Coq -I path_to_coq_dir_theories\n\nfilenames:\tcoq binary files (.vo)\n\noptions:"
