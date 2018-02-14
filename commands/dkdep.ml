open Basic
open Term
open Rule
open Parser

(** [find_dko name path] answers whether there is a ".dko" file corresponding
    to the module [name] in the path. If found, the file is left untouched. *)
let find_dko : string -> string list -> bool = fun name ->
  let name = name ^ ".dko" in
  List.exists (fun dir -> Sys.file_exists (Filename.concat dir name))

(** [deps] contains the dependencies found so far. *)
let deps : (string * string list) list ref = ref []

(** [add_dep name] adds the module named [name] to the list of dependencies if
    no corresponding ".dko" file is found in the load path. The dependency is
    not added either if it is already present. *)
let add_dep : string -> unit = fun name ->
  if find_dko name (get_path ()) then () else
  match !deps with
  | []                              -> assert false (* impossible *)
  | (m,ds)::l when name = m         -> () (* Current module. *)
  | (m,ds)::l when List.mem name ds -> () (* Already a dependency. *)
  | (m,ds)::l                       ->
      deps := (m, List.sort String.compare (name::ds))::l

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

(** ... TODO clean from here *)


let out = ref stdout
let md_to_file = ref []
let filename = ref ""
let verbose = ref false
let sorted = ref false

let dfs graph visited start_node =
  let rec explore path visited node =
    if List.mem node path then failwith "Circular dependencies";
    if List.mem node visited then visited else
    let new_path = node :: path in
    let edges    = List.assoc node graph in
    node :: List.fold_left (explore new_path) visited edges
  in explore [] visited start_node

let topological_sort graph =
  List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph

let finalize () =
  let name, deps = List.hd !deps in
  md_to_file := (name, !filename)::!md_to_file;
  if not !sorted then
    Printf.fprintf !out "%s.dko : %s %s\n" name !filename
      (String.concat " " (List.map (fun s -> s ^ ".dko") deps))

let sort () = List.map (fun md -> List.assoc md !md_to_file) (List.rev (topological_sort !deps))

let run_on_file file =
  if !(verbose) then Printf.eprintf "Running Dkdep on file \"%s\".\n" file;
  flush stderr;
  let input = open_in file in
  filename := file;
  let md =  Basic.mk_mident file in
  deps := (string_of_mident md, [])::!deps;
  Parser.handle_channel md handle_entry input;
  finalize ();
  close_in input

let args =
  [ ("-o", Arg.String (fun fi -> out := open_out fi), "Output file"  );
    ("-v", Arg.Set verbose, "Verbose mode" );
    ("-s", Arg.Set sorted, "Sort file with respect to their dependence");
    ("-I", Arg.String Basic.add_path, "Add a directory to load path, dependencies to files found in load path are not printed");
  ]

let _ =
  try
    Arg.parse args run_on_file "Usage: dkdep [options] files";
    if !sorted then Printf.printf "%s\n" (String.concat " " (sort ()))
  with
    | Sys_error err             -> Printf.eprintf "ERROR %s.\n" err; exit 1
    | Exit                      -> exit 3
