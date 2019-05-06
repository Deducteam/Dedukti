open Basic

type path = string

type data = mident * path

type mdep_data =  data * data list

(** [deps] contains the dependencies found so far, reset before each file. *)
let current_mod  : mident                 ref = ref (mk_mident "<not initialised>")
let current_deps : (mident * path) list   ref = ref []
let ignore       : bool                   ref = ref false

type dep_error =
  | ModuleNotFound of string
  | MultipleModules of string * string list
  | CircularDependencies of string * string list

exception Dep_error of dep_error

let in_deps : mident -> bool = fun n ->
  List.mem_assoc n !current_deps

let add_dep : mident -> path option -> unit = fun name file ->
  let cmp (s1,_) (s2,_) = compare s1 s2 in
  match file with
  | None -> ()
  | Some file ->
    current_deps := List.sort cmp ((name, file) :: !current_deps)

(** [find_dk md path] looks for the ".dk" file corresponding to the module
    named [name] in the directories of [path]. If no corresponding file is
    found, or if there are several possibilities, the program fails with a
    graceful error message. *)
let find_dk : mident -> path list -> path option = fun md path ->
  let name = string_of_mident md in
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
    else
      raise @@ Dep_error(ModuleNotFound name)
  | [f] -> Some f
  | fs  ->
    raise @@ Dep_error(MultipleModules(name,fs))

(** [add_dep name] adds the module named [name] to the list of dependencies if
    no corresponding ".dko" file is found in the load path. The dependency is
    not added either if it is already present. *)
let add_dep : mident -> unit = fun md ->
  if md <> !current_mod && not (in_deps md)
  then add_dep md (find_dk md (get_path ()))

(** Term / pattern / entry traversal commands. *)

let mk_name c =
  add_dep (md c)

let rec mk_term t =
  let open Term in
  match t with
  | Kind | Type _ | DB _ -> ()
  | Const(_,c)           -> mk_name c
  | App(f,a,args)        -> List.iter mk_term (f::a::args)
  | Lam(_,_,None,te)     -> mk_term te
  | Lam(_,_,Some(ty),te) -> mk_term ty; mk_term te
  | Pi (_,_,a,b)         -> mk_term a; mk_term b

let rec mk_pattern p =
  let open Rule in
  match p with
  | Var(_,_,_,args)   -> List.iter mk_pattern args
  | Pattern(_,c,args) -> mk_name c; List.iter mk_pattern args
  | Lambda(_,_,te)    -> mk_pattern te
  | Brackets(t)       -> mk_term t

let mk_rule r =
  let open Rule in
  mk_pattern r.pat; mk_term r.rhs

let handle_entry e =
  let open Entry in
  match e with
  | Decl(_,_,_,te)              -> mk_term te
  | Def(_,_,_,None,te)          -> mk_term te
  | Def(_,_,_,Some(ty),te)      -> mk_term ty; mk_term te
  | Rules(_,rs)                 -> List.iter mk_rule rs
  | Eval(_,_,te)                -> mk_term te
  | Infer (_,_,te)              -> mk_term te
  | Check(_,_,_,Convert(t1,t2)) -> mk_term t1; mk_term t2
  | Check(_,_,_,HasType(te,ty)) -> mk_term te; mk_term ty
  | DTree(_,_,_)                -> ()
  | Print(_,_)                  -> ()
  | Name(_,_)                   -> ()
  | Require(_,md)               -> add_dep md

let make ((md,file) as data) entries =
  current_mod :=md; current_deps := [];
  List.iter handle_entry entries;
  data, !current_deps

let handle ((md,file) as data) process =
  current_mod := md; current_deps := [];
  process handle_entry;
  data, !current_deps

let topological_sort dep_data =
  let graph = List.map (fun ((_,f),deps) -> (f, List.map snd deps)) dep_data in
  let rec explore path visited node =
    if List.mem node path then
      raise @@ Dep_error (CircularDependencies(node,path));
    if List.mem node visited then visited else
      let edges =
        try List.assoc node graph
        with Not_found ->
          if !ignore
          then []
          else
            raise @@ Dep_error (ModuleNotFound node)
      in
      node :: List.fold_left (explore (node :: path)) visited edges
  in
  List.rev @@ List.fold_left (fun visited (n,_) -> explore [] visited n) [] graph
