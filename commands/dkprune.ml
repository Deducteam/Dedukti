open Kernel
open Parsing
open Api

open Basic

exception NoDirectory
exception EntryNotHandled of Entry.entry
exception BadFormat
exception NoPruneFile

let output_directory : string option ref = ref None

module E = Env.Make(Reduction.Default)

module ErrorHandler = Errors.Make(E)

module CustomSig =
struct
  let current = ref (Basic.mk_mident "")
  let set md = current := md
  let get_name () = !current
end

module Printer = Pp.Make(CustomSig)


module D = Basic.Debug
type D.flag += D_prune
let _ = D.register_flag D_prune "Dkprune"
let enable_log : unit -> unit = fun () -> D.enable_flag D_prune

let gre fmt = "\027[32m" ^^ fmt ^^ "\027[0m%!"

let log fmt = D.debug D_prune (gre fmt)

type constraints =
  {
    names:Dep.NameSet.t;
    mds: mident list
  }

let _ =
  Dep.ignore := true;
  Dep.compute_ideps := true

let rec handle_file : string -> unit =
  let computed = ref Dep.MDepSet.empty in
  fun file ->
    let md = Env.Default.init file in
    if not @@ Dep.MDepSet.mem (md,file) !computed then
      begin
        computed := Dep.MDepSet.add (md,file) !computed;
        log "[COMPUTE DEP] %s" file;
        let input = open_in file in
        begin
          try Dep.handle md (fun f -> Parser.Parse_channel.handle md f input)
          with e -> ErrorHandler.graceful_fail (Some file) e
        end;
        close_in input;
        let md_deps = Hashtbl.find Dep.deps md in
        Dep.MDepSet.iter
          (fun (md,_) ->
            try handle_file (Dep.get_file md)
            with _ -> ())
          Dep.(md_deps.deps)
      end

let handle_name : Basic.name -> unit = fun name ->
  let md = Basic.md name in
  let file = Dep.get_file md in
  handle_file file

let handle_constraints names =
  Dep.NameSet.iter handle_name names

let output_file : Dep.path -> Dep.path = fun file ->
  let basename = Filename.basename file in
  match !output_directory with
  | None -> raise @@ NoDirectory
  | Some dir -> Filename.concat dir basename

let get_files : unit -> (mident * Dep.path * Dep.path) list = fun () ->
  Hashtbl.fold (fun md _ l ->
      try
        let f = Dep.get_file md in
        (md, f, output_file f)::l
    with _ -> l) Dep.deps []

let name_of_entry md = function
  | Entry.Decl(_,id,_,_) ->
    mk_name md id
  | Entry.Def(_,id,_,_,_) ->
    mk_name md id
  | Entry.Rules(_,r::_) ->
    let open Rule in
    let r' = to_rule_infos r in
    r'.cst
  | _ as e -> raise @@ EntryNotHandled e

let is_empty deps md in_file =
  let input = open_in in_file in
  let empty = ref true in
  let mk_entry e =
    let name = name_of_entry md e in
    if Dep.NameSet.mem name deps then
      empty := false;
  in
  Parser.Parse_channel.handle md mk_entry input;
  close_in input;
  !empty

let mk_file deps (md,in_file,out_file) =
  log "[WRITING FILE] %s" out_file;
  if not (is_empty deps md in_file)
  then
    let input = open_in in_file in
    let output = open_out out_file in
    let fmt = Format.formatter_of_out_channel output in
    let handle_entry e =
      let name = name_of_entry md e in
      CustomSig.set md;
      if Dep.NameSet.mem name deps
      then Format.fprintf fmt "%a" Printer.print_entry e
    in
    Parser.Parse_channel.handle md handle_entry input;
    close_out output;
    close_in input

let print_dependencies names =
  let open Dep in
  NameSet.iter Dep.transitive_closure names;
  let down = NameSet.fold
      (fun name dependencies ->
         NameSet.union (get_data name).down dependencies) names names in
  let files = get_files () in
  List.iter (mk_file down) files

(* This opens a module and returns all the names of symbols declared inside *)
let names_of_md md =
  let file = Dep.get_file md in
  let input = open_in file in
  let names = ref Dep.NameSet.empty in
  let mk_entry e =
    let n = name_of_entry md e in
    names := Dep.NameSet.add n !names
  in
  Parser.Parse_channel.handle md mk_entry input;
  close_in input;
  !names

(* This gather all symbols *)
let gather_names =
  function
  | Entry.Require(_,md) -> names_of_md md
  | Entry.DTree(_,Some md, id) -> Dep.NameSet.singleton (mk_name md id)
  | _ -> raise BadFormat

let parse_constraints file =
  let md = mk_mident file in
  let input = open_in file in
  let pcstr = List.map gather_names (Parser.Parse_channel.parse md input) in
  close_in input;
  List.fold_left Dep.NameSet.union Dep.NameSet.empty pcstr

let _ =
  let args = Arg.align
    [ ( "-l"
      , Arg.Unit enable_log
      , " Print log")
    ; ( "-I"
      , Arg.String add_path
      , " DIR Add the directory DIR to the load path" )
    ; ( "-o"
      , Arg.String (fun s -> output_directory := Some s)
      , " Set the output directory" ) ]
  in
  let usage = Format.sprintf "Usage: %s [OPTION]... [FILE]...
Compute the dependencies of the given Dedukti FILE(s).
For more information see https://github.com/Deducteam/Dedukti.
Available options:" Sys.argv.(0) in
  let files =
    let files = ref [] in
    Arg.parse args (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  let open Dep in
  let cstr = List.fold_left NameSet.union NameSet.empty (List.map parse_constraints files) in
  handle_constraints cstr;
  print_dependencies cstr
