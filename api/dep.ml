open Kernel
open Parsers
open Basic

module NameSet = Basic.NameSet

module MSet    = Basic.MidentSet

type data = {
  up: NameSet.t ; (** dependencies are the name that requires the current item. *)
  down: NameSet.t ; (** down dependencies are the name that are required by the current item. *)
}

type name_deps = (ident, data) Hashtbl.t

type file_deps =
  {
    file:string; (** path associated to the module *)
    deps: MSet.t; (** pairs of module and its associated path *)
    name_deps: name_deps; (** up/down item dependencies *)
}

type dep_error =
  | CircularDependencies of string * string list
  | NameNotFound of name


exception Dep_error of dep_error

type t = (mident, file_deps) Hashtbl.t

let fail_dep_error err =
  match err with
  | CircularDependencies (s,ss) ->
    602, None, Format.asprintf "Circular Dependency dectected for module %S...%a" s
      (pp_list "@." (fun fmt s -> Format.fprintf fmt " -> %s" s)) ss
  | NameNotFound n ->
    603, None, Format.asprintf "No dependencies computed for name %a...@." pp_name n

let fail_dep_error ~red:_ exn =
  match exn with
  | Dep_error err -> Some(fail_dep_error err)
  | _ -> None

let _ = Errors.register_exception fail_dep_error

let compute_all_deps = ref false

let empty_deps () = {file="<not initialized>"; deps = MSet.empty; name_deps=Hashtbl.create 81}

let deps = Hashtbl.create 11

(** [deps] contains the dependencies found so far, reset before each file. *)
let current_mod   : mident      ref = ref (mk_mident "<not initialised>")
let current_name  : name option ref = ref None
let current_deps  : file_deps   ref = ref (empty_deps ())
let ignore        : bool        ref = ref false

(** [add_dep md] adds the module [md] to the list of dependencies if
    no corresponding ".dko" file is found in the load path. The dependency is
    not added either if it is already present. *)
let add_mdep : mident -> unit = fun md ->
  if mident_eq md !current_mod then () else
    match (Files.find_dk ~ignore:!ignore md (Files.get_path ())) with
    | None -> ()
    | Some _ ->
      current_deps  :=
        {!current_deps with deps = MSet.add md !current_deps.deps}

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
  let ideps = (find deps (md item)).name_deps in
  update_down ideps (id item) dep;
  let ideps = (find deps (md dep)).name_deps in
  update_up ideps (id dep) item


let add_idep : name -> unit = fun dep_name ->
  match !current_name with
  | None -> ()
  | Some current_name ->
    if not @@ Basic.name_eq dep_name current_name then
      update_ideps current_name dep_name

(** Term / pattern / entry traversal commands. *)

let mk_name c =
  add_mdep (md c);
  if !compute_all_deps then add_idep c

let rec mk_term t =
  let open Term in
  match t with
  | Kind | Type _ | DB _ -> ()
  | Const(_,c)           -> mk_name c
  | App(f,a,args)        -> List.iter mk_term (f::a::args)
  | Lam(_,_,None,te)     -> mk_term te
  | Lam(_,_,Some(ty),te) -> mk_term ty; mk_term te
  | Pi (_,_,a,b)         -> mk_term a; mk_term b

let mk_rule r =
  let open Rule in
  mk_term r.lhs; mk_term r.rhs

let handle_entry e =
  let open Entry in
  let name_of_id id = Basic.mk_name !current_mod id in
  let update_name_with_rule r =
    let open Rule in
    match r.lhs with
    | App(Const(_,n) ,_, _) -> current_name := Some n
    | _ -> current_name := None
  in
  match e with
  | Decl(_,id,_,_,te)           -> current_name := Some (name_of_id id); mk_term te
  | Def(_,id,_,_,None,te)       -> current_name := Some (name_of_id id); mk_term te
  | Def(_,id,_,_,Some(ty),te)   -> current_name := Some (name_of_id id); mk_term ty; mk_term te
  | Rules(_,[])                 -> ()
  | Rules(_,(r::_ as rs))       -> update_name_with_rule r; List.iter mk_rule rs
  | Eval(_,_,te)                -> current_name := None; mk_term te
  | Infer (_,_,te)              -> current_name := None; mk_term te
  | Check(_,_,_,Convert(t1,t2)) -> current_name := None; mk_term t1; mk_term t2
  | Check(_,_,_,HasType(te,ty)) -> current_name := None; mk_term te; mk_term ty
  | DTree(_,_,_)                -> ()
  | Print(_,_)                  -> ()
  | Name(_,_)                   -> ()
  | Require(_,md)               -> add_mdep md

let initialize : mident -> string -> unit = fun md file ->
  current_mod :=md;
  current_deps := find deps md;
  current_deps := {!current_deps with file}

let make : mident -> Entry.entry list -> unit = fun md entries ->
  let file = Files.get_file md in
  initialize md file;
  List.iter handle_entry entries;
  Hashtbl.replace deps md !current_deps

let handle : mident -> ((Entry.entry -> unit) -> unit) -> unit = fun md process ->
  let file = Files.get_file md in
  initialize md file;
  process handle_entry;
  Hashtbl.replace deps md !current_deps

let topological_sort deps =
  let to_graph _ deps graph =
    (deps.file, MSet.elements deps.deps)::graph
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
            raise @@ Files.Files_error (ObjectFileNotFound (mk_mident node))
      in
      node :: List.fold_left (explore (node :: path)) visited (List.map Files.get_file edges)
  in
  List.rev @@ List.fold_left (fun visited (n,_) -> explore [] visited n) [] graph

let get_data : Basic.name -> data = fun name ->
  try
    let md = Basic.md name in
    let id = Basic.id name in
    Hashtbl.find (Hashtbl.find deps md).name_deps id
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
      Hashtbl.replace md_deps.name_deps id ideps'

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
      Hashtbl.replace md_deps.name_deps id ideps'

let transitive_closure : Basic.name -> unit = fun name ->
  transitive_closure_down name;
  transitive_closure_up name
