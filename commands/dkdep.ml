open Basic
open Term
open Rule
open Parser
open Entry



(** [deps] contains the dependencies found so far, reset before each file. *)
let current_mod  : string                 ref = ref "<not initialised>"
let current_deps : (string * string) list ref = ref []
let ignore       : bool                   ref = ref false

let in_deps : string -> bool = fun n ->
  List.mem_assoc n !current_deps

let add_dep : string -> string option -> unit = fun name file ->
  let cmp (s1,_) (s2,_) = String.compare s1 s2 in
  match file with
  | None -> ()
  | Some file ->
    current_deps := List.sort cmp ((name, file) :: !current_deps)

(** [locate name path] looks for the ".dk" file corresponding to the module
    named [name] in the directories of [path]. If no corresponding file is
    found, or if there are several possibilities, the program fails with a
    graceful error message. *)
let find_dk : string -> string list -> string option = fun name path ->
  let file_name = name ^ ".dk" in
  let path = Filename.current_dir_name :: path in
  let path = List.sort_uniq String.compare path in
  let add_dir dir =
    if dir = Filename.current_dir_name then file_name
    else Filename.concat dir file_name
  in
  let files = List.map add_dir path in
  match List.filter Sys.file_exists files with
  | []  ->
    if !ignore then None
    else (Printf.eprintf "No file for module %S in path...\n%!" name; exit 1)
  | [f] -> Some f
  | fs  -> Printf.eprintf "Several files correspond to module %S...\n" name;
           List.iter (Printf.eprintf "  - %s\n%!") fs; exit 1

(** [add_dep name] adds the module named [name] to the list of dependencies if
    no corresponding ".dko" file is found in the load path. The dependency is
    not added either if it is already present. *)
let add_dep : string -> unit = fun name ->
  if name = !current_mod || in_deps name then () else
  add_dep name (find_dk name (get_path ()))

(** Term / pattern / entry traversal commands. *)

let mk_name c =
  add_dep (string_of_mident (md c))

let rec mk_term t =
  match t with
  | Kind | Type _ | DB _ -> ()
  | Const(_,c)           -> mk_name c
  | App(f,a,args)        -> List.iter mk_term (f::a::args)
  | Lam(_,_,None,te)     -> mk_term te
  | Lam(_,_,Some(ty),te) -> mk_term ty; mk_term te
  | Pi (_,_,a,b)         -> mk_term a; mk_term b

let rec mk_pattern p =
  match p with
  | Var(_,_,_,args)   -> List.iter mk_pattern args
  | Pattern(_,c,args) -> mk_name c; List.iter mk_pattern args
  | Lambda(_,_,te)    -> mk_pattern te
  | Brackets(t)       -> mk_term t

let mk_rule r =
  mk_pattern r.pat; mk_term r.rhs

let handle_entry e =
  match e with
  | Decl(_,_,_,te)              -> mk_term te
  | Def(_,_,_,None,te)          -> mk_term te
  | Def(_,_,_,Some(ty),te)      -> mk_term ty; mk_term te
  | Rules(rs)                   -> List.iter mk_rule rs
  | Eval(_,_,te)                -> mk_term te
  | Infer (_,_,te)              -> mk_term te
  | Check(_,_,_,Convert(t1,t2)) -> mk_term t1; mk_term t2
  | Check(_,_,_,HasType(te,ty)) -> mk_term te; mk_term ty
  | DTree(_,_,_)                -> ()
  | Print(_,_)                  -> ()
  | Name(_,_)                   -> ()
  | Require(_,md)               -> add_dep (string_of_mident md)

type dep_data = string * (string * (string * string) list)

let handle_file : string -> dep_data = fun file ->
  try
    (* Initialisation. *)
    let md = Basic.mk_mident file in
    let name = string_of_mident md in
    current_mod := name; current_deps := [];
    (* Actully parsing and gathering data. *)
    let input = open_in file in
    Parser.handle_channel md handle_entry input;
    close_in input;
    (name, (file, !current_deps))
  with
  | Parse_error(loc,msg)      -> Printf.eprintf "Parse error...\n%!"; exit 1
  | Sys_error err             -> Printf.eprintf "ERROR %s.\n%!" err; exit 1
  | Exit                      -> exit 3

(** Output main program. *)

let output_deps : out_channel -> dep_data list -> unit = fun oc data ->
  let objfile src = Filename.chop_extension src ^ ".dko" in
  let output_line : dep_data -> unit = fun (name, (file, deps)) ->
    let deps = List.map (fun (_,src) -> objfile src) deps in
    let deps = String.concat " " deps in
    Printf.fprintf oc "%s : %s %s\n" (objfile file) file deps
  in
  List.iter output_line data

let topological_sort graph =
  let rec explore path visited node =
    if List.mem node path then
      begin
        Printf.eprintf "Dependecies are circular...";
        exit 1
      end;
    if List.mem node visited then visited else
      let edges = try List.assoc node graph with Not_found ->
        if !ignore then
          []
        else
         (Printf.eprintf "Cannot compute dependencies for the file %S... (maybe you forgot to put it on the command line?)\n%!" node; exit 1)
      in
      node :: List.fold_left (explore (node :: path)) visited edges
  in
  List.fold_left (fun visited (n,_) -> explore [] visited n) [] graph

let output_sorted : out_channel -> dep_data list -> unit = fun oc data ->
  let deps = List.map (fun (_,(f,deps)) -> (f, List.map snd deps)) data in
  let deps = List.rev (topological_sort deps) in
  Printf.printf "%s\n" (String.concat " " deps)

let _ =
  (* Parsing of command line arguments. *)
  let output  = ref stdout in
  let sorted  = ref false  in
  let args = Arg.align
    [ ( "-o"
      , Arg.String (fun n -> output := open_out n)
      , "FILE Outputs to file FILE" )
    ; ( "-s"
      , Arg.Set sorted
      , " Sort the source files according to their dependencies" )
    ; ( "--ignore"
      , Arg.Set ignore
      , " If some dependencies are not found, ignore them" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Add the directory DIR to the load path" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse args (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  (* Actual work. *)
  let dep_data = List.map handle_file files in
  let output_fun = if !sorted then output_sorted else output_deps in
  output_fun !output dep_data;
  close_out !output
