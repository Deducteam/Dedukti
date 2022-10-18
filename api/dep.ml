type mident = Kernel.Basic.mident

module MidentSet = Kernel.Basic.MidentSet

type name = Kernel.Basic.name

module NameSet = Kernel.Basic.NameSet

module Md_map = Map.Make (struct
  type t = mident

  let compare = compare
end)

module Name_map = Map.Make (struct
  type t = name

  let compare = compare
end)

type t = {
  module_dependencies : MidentSet.t Md_map.t;
  name_dependencies : NameSet.t Name_map.t;
  transitive : bool;
  reverse : bool;
}

type deps = {modules : Kernel.Basic.MidentSet.t; names : Kernel.Basic.NameSet.t}

let empty = {modules = MidentSet.empty; names = NameSet.empty}

let merge left right =
  {
    modules = MidentSet.union left.modules right.modules;
    names = NameSet.union left.names right.names;
  }

let mk_name c =
  {
    modules = MidentSet.singleton (Kernel.Basic.md c);
    names = NameSet.singleton c;
  }

let rec mk_term t =
  let open Kernel.Term in
  match t with
  | Kind | Type _ | DB _ -> empty
  | Const (_, c) -> mk_name c
  | App (f, a, args) ->
      List.fold_left
        (fun deps t -> merge deps (mk_term t))
        empty (f :: a :: args)
  | Lam (_, _, None, te) -> mk_term te
  | Lam (_, _, Some ty, te) -> merge (mk_term ty) (mk_term te)
  | Pi (_, _, a, b) -> merge (mk_term a) (mk_term b)

let rec mk_pattern p =
  let open Kernel.Rule in
  match p with
  | Var (_, _, _, args) ->
      List.fold_left (fun deps args -> merge deps (mk_pattern args)) empty args
  | Pattern (_, c, args) ->
      List.fold_left (fun deps args -> merge deps (mk_pattern args)) empty args
      |> merge (mk_name c)
  | Lambda (_, _, _, te) -> mk_pattern te
  | Brackets t -> mk_term t

let mk_rule r =
  let open Kernel.Rule in
  merge (mk_pattern r.pat) (mk_term r.rhs)

let dep_of_entry md entry =
  let open Parsers.Entry in
  match entry with
  | Decl (_, _, _, _, te) -> mk_term te
  | Def (_, _, _, _, None, te) -> mk_term te
  | Def (_, _, _, _, Some ty, te) -> merge (mk_term ty) (mk_term te)
  | Rules (_, rs) ->
      List.fold_left (fun deps r -> merge deps (mk_rule r)) empty rs
  | Eval (_, _, te) -> mk_term te
  | Infer (_, _, te) -> mk_term te
  | Check (_, _, _, Convert (t1, t2)) -> merge (mk_term t1) (mk_term t2)
  | Check (_, _, _, HasType (te, ty)) -> merge (mk_term te) (mk_term ty)
  | DTree (_, md_opt, id) ->
      let md = Option.value md_opt ~default:md in
      mk_name (Kernel.Basic.mk_name md id)
  | Print (_, _) -> empty
  | Name (_, _) -> empty
  | Pragma _ -> empty
  | Require (_, md) -> {modules = MidentSet.singleton md; names = NameSet.empty}

let dep_of_entry :
    ?strict:bool -> Kernel.Basic.mident -> Parsers.Entry.entry -> deps =
 fun ?(strict = true) md entry ->
  let name_of_id id = Kernel.Basic.mk_name md id in
  let name_opt =
    match entry with
    | Decl (_, id, _, _, _) | Def (_, id, _, _, _, _) -> Some (name_of_id id)
    | Rules (_, {pat = Pattern (_, name, _); _} :: _) -> Some name
    | DTree (_, md_opt, id) ->
        let md = Option.value md_opt ~default:md in
        Some (Kernel.Basic.mk_name md id)
    | _ -> None
  in
  let deps = dep_of_entry md entry in
  (* If [strict] we ensure the dependency relation is not reflexive. *)
  if strict then
    {
      modules = MidentSet.remove md deps.modules;
      names =
        Option.fold ~none:deps.names
          ~some:(fun name -> NameSet.remove name deps.names)
          name_opt;
    }
  else deps

let empty ?(reverse = false) ?(transitive = false) () =
  {
    module_dependencies = Md_map.empty;
    name_dependencies = Name_map.empty;
    transitive;
    reverse;
  }

module type PARAMETERS = sig
  type elt

  module Set : Set.S with type elt = elt

  module Map : Map.S with type key = elt

  val get : t -> Set.t Map.t

  val update : t -> Set.t Map.t -> t
end

module type S = sig
  type elt

  module Set : Set.S with type elt = elt

  val add : t -> elt -> Set.t -> t

  val sort : t -> (elt list, [`Circular of elt * elt list]) Result.t

  val get : t -> elt -> Set.t
end

module Make (P : PARAMETERS) : S with type elt = P.elt = struct
  type elt = P.elt

  module Set = P.Set
  module Map = P.Map

  let sort t =
    let dependencies = P.get t in
    let find elt =
      match Map.find_opt elt dependencies with
      | None -> Set.empty
      | Some deps -> deps
    in
    (* Compute the list (in reverse) order from [node]. [path] is the
       current [path] explored in the implicit graph represented by
       the map [t.module_dependencies]. [visited] is the current list
       of sorted nodes (in the reverse order). A node is added into
       this [list] once all its downward dependencies were visited. *)
    let rec explore path visited node =
      if List.mem node path then Error (`Circular (node, path))
      else
        let step visited =
          if List.mem node visited then Ok visited
          else
            Set.fold
              (fun node visited ->
                explore (node :: path) visited node
                |> Result.map (fun visited -> node :: visited))
              (find node) (Ok visited)
        in
        Result.bind visited step
    in
    (* Compute the dependency list for all the modules and reverse the
       list to get the modules in the correct order. *)
    Map.to_seq dependencies |> Seq.map fst
    |> Seq.fold_left (fun visited node -> explore [] visited node) (Ok [])
    |> Result.map List.rev

  let get t elt = Map.find_opt elt (P.get t) |> Option.value ~default:Set.empty

  let rec add t elt deps =
    let updater deps = function
      | None -> Some deps
      | Some old_deps -> Set.union old_deps deps |> Option.some
    in
    let t =
      match t.reverse with
      | false ->
          (* The only dependency to store is [elt -> deps] *)
          P.get t |> Map.update elt (updater deps) |> P.update t
      | true ->
          (* The dependencies to store are [dep -> elt]  *)
          let reverse_deps = Set.singleton elt in
          P.get t
          |> Set.fold
               (fun dep map -> Map.update dep (updater reverse_deps) map)
               deps
          |> P.update t
    in
    match t.transitive with
    | false -> t
    | true ->
        let folder dep t =
          match Map.find_opt dep (P.get t) with
          | None -> t
          | Some deps -> add t elt deps
        in
        Set.fold folder deps t
end

module Modules = Make (struct
  type elt = mident

  module Set = Kernel.Basic.MidentSet
  module Map = Md_map

  let get t = t.module_dependencies

  let update t module_dependencies = {t with module_dependencies}
end)

module Names = Make (struct
  type elt = name

  module Set = Kernel.Basic.NameSet
  module Map = Name_map

  let get t = t.name_dependencies

  let update t name_dependencies = {t with name_dependencies}
end)
