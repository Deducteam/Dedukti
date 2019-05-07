open Basic

type path = string

module MDepSet = Set.Make(struct type t = Basic.mident * path let compare = compare end)

module NameSet = Set.Make(struct type t = Basic.name let compare = compare end)

type data = {up: NameSet.t ; down: NameSet.t}
(** up dependencies are the name that requires the current item.
    down dependencies are the name that are required by the current item. *)

type ideps = (ident, data) Hashtbl.t

type deps =
  {
    file:path; (** path associated to the module *)
    deps: MDepSet.t; (** pairs of module and its associated path *)
    ideps: ideps; (** up/down item dependencies *)
}

type t = (mident, deps) Hashtbl.t

let compute_ideps = ref false

let empty_deps () = {file="<not initialized>"; deps = MDepSet.empty; ideps=Hashtbl.create 81}

let deps = Hashtbl.create 11


(** [deps] contains the dependencies found so far, reset before each file. *)
let current_mod   : mident    ref = ref (mk_mident "<not initialised>")
let current_name  : name      ref = ref (mk_name (!current_mod) (mk_ident  "<not initialised>"))
let current_deps  : deps      ref = ref (empty_deps ())
let ignore        : bool      ref = ref false
let process_items : bool      ref = ref false

type dep_error =
  | ModuleNotFound of string
  | MultipleModules of string * string list
  | CircularDependencies of string * string list

exception Dep_error of dep_error

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

(** [add_dep md] adds the module [md] to the list of dependencies if
    no corresponding ".dko" file is found in the load path. The dependency is
    not added either if it is already present. *)
let add_mdep : mident -> unit = fun md ->
  if mident_eq md !current_mod then () else
    match (find_dk md (get_path ())) with
    | None -> ()
    | Some file ->
      current_deps :=
        {!current_deps with deps = MDepSet.add (md, file) !current_deps.deps}

let update_up ideps item up =
  if Hashtbl.mem ideps item then
    let idep = Hashtbl.find ideps item in
    Hashtbl.replace ideps item {idep with up=NameSet.add up idep.up}
  else
    Hashtbl.add ideps item {up=NameSet.singleton up;down=NameSet.empty}

let update_down ideps item down =
  if Hashtbl.mem ideps item then
    let idep = Hashtbl.find ideps item in
    Hashtbl.replace ideps item {idep with down=NameSet.add down idep.down}
  else
    Hashtbl.add ideps item {down=NameSet.singleton down;up=NameSet.empty}

let find deps md =
  if Hashtbl.mem deps md then
    Hashtbl.find deps md
  else
    begin
      Hashtbl.add deps md (empty_deps ());
      Hashtbl.find deps md
    end


let update_ideps item dep =
  let ideps = (find deps (md item)).ideps in
  update_down ideps (id item) dep;
  let ideps = (find deps (md dep)).ideps in
  update_up ideps (id dep) item


let add_idep : name -> unit = fun dep_name ->
  update_ideps !current_name dep_name

(** Term / pattern / entry traversal commands. *)

let mk_name c =
  add_mdep (md c);
  if !compute_ideps then
    add_idep c

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
  | Require(_,md)               -> add_mdep md

let initialize : mident -> path -> unit = fun md file ->
  current_mod :=md;
  current_deps := find deps md;
  current_deps := {!current_deps with file}

let make : mident -> path -> Entry.entry list -> unit = fun md file entries ->
  initialize md file;
  List.iter handle_entry entries;
  Hashtbl.replace deps md !current_deps

let handle : mident -> path -> ((Entry.entry -> unit) -> unit) -> unit = fun md file process ->
  initialize md file;
  process handle_entry;
  Hashtbl.replace deps md !current_deps

let topological_sort deps =
  let to_graph _ deps graph =
    (deps.file, MDepSet.elements deps.deps)::graph
  in
  let graph = Hashtbl.fold to_graph deps [] in
  let rec explore path visited node =
    if List.mem node path then
      raise @@ Dep_error (CircularDependencies(node,path));
    if List.mem node visited then visited else
      let edges =
        try List.assoc node graph with Not_found ->
          if !ignore then []
          else
            raise @@ Dep_error (ModuleNotFound node)
      in
      node :: List.fold_left (explore (node :: path)) visited (List.map snd edges)
  in
  List.rev @@ List.fold_left (fun visited (n,_) -> explore [] visited n) [] graph
