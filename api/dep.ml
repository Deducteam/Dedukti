open Kernel
open Parsing
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

type dep_error =
  | ModuleNotFound of mident
  | MultipleModules of string * string list
  | CircularDependencies of string * string list
  | NameNotFound of name
  | NoDep of mident

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
      raise @@ Dep_error(ModuleNotFound md)
  | [f] -> Some f
  | fs  ->
    raise @@ Dep_error(MultipleModules(name,fs))

let get_file md =
  match find_dk md (get_path ()) with
  | None -> raise @@ Dep_error(ModuleNotFound md)
  | Some f -> f

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
  if not @@ Basic.name_eq dep_name !current_name then
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

let find_rule_name r =
  let open Rule in
  match r.pat with
  | Pattern(_,n,_) -> n
| _ -> assert false

let handle_entry e =
  let open Entry in
  let name_of_id id = Basic.mk_name !current_mod id in
  match e with
  | Decl(_,id,_,_,te)           -> current_name := name_of_id id; mk_term te
  | Def(_,id,_,_,None,te)       -> current_name := name_of_id id; mk_term te
  | Def(_,id,_,_,Some(ty),te)   -> current_name := name_of_id id; mk_term ty; mk_term te
  | Rules(_,[])                 -> ()
  | Rules(_,(r::_ as rs))       -> current_name := find_rule_name r; List.iter mk_rule rs
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

let make : mident -> Entry.entry list -> unit = fun md entries ->
  let file = get_file md in
  initialize md file;
  List.iter handle_entry entries;
  Hashtbl.replace deps md !current_deps

let handle : mident -> ((Entry.entry -> unit) -> unit) -> unit = fun md process ->
  let file = get_file md in
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
            raise @@ Dep_error (NoDep (mk_mident node))
      in
      node :: List.fold_left (explore (node :: path)) visited (List.map snd edges)
  in
  List.rev @@ List.fold_left (fun visited (n,_) -> explore [] visited n) [] graph

let get_data : Basic.name -> data = fun name ->
  try
    let md = Basic.md name in
    let id = Basic.id name in
    Hashtbl.find (Hashtbl.find deps md).ideps id
  with Not_found ->
    raise @@ Dep_error(NameNotFound name)

let rec transitive_closure_down =
  let computed = ref NameSet.empty in
  fun name ->
    if not (NameSet.mem name !computed) then
      let md = Basic.md name in
      let id = Basic.id name in
      let ideps = get_data name in
      computed := NameSet.add name !computed;
      NameSet.iter transitive_closure_down ideps.down;
      let down = NameSet.fold
          (fun name_dep down -> NameSet.union down (get_data name_dep).down) ideps.down ideps.down in
      let ideps' = {ideps with down} in
      let md_deps = Hashtbl.find deps md in
      Hashtbl.replace md_deps.ideps id ideps'

let rec transitive_closure_up =
  let computed = ref NameSet.empty in
  fun name ->
    if not (NameSet.mem name !computed) then
      let md = Basic.md name in
      let id = Basic.id name in
      let ideps = get_data name in
      computed := NameSet.add name !computed;
      NameSet.iter transitive_closure_up ideps.up;
      let up = NameSet.fold
          (fun name_dep up -> NameSet.union up (get_data name_dep).down) ideps.up ideps.up in
      let ideps' = {ideps with up} in
      let md_deps = Hashtbl.find deps md in
      Hashtbl.replace md_deps.ideps id ideps'

let transitive_closure : Basic.name -> unit = fun name ->
  transitive_closure_down name;
  transitive_closure_up name
