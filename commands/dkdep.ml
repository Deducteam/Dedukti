open Basic
open Term
open Rule
open Parser

(** [find_dko name path] answers whether there is a ".dko" file corresponding
    to the module [name] in the path. If found, the file is left untouched. *)
let find_dko : string -> string list -> bool = fun name ->
  let name = name ^ ".dko" in
  List.exists (fun dir -> Sys.file_exists (Filename.concat dir name))

(** [deps] contains the dependencies found so far, reset before each file. *)
let current_mod  : string      ref = ref "<not initialised>"
let current_deps : string list ref = ref []

(** [add_dep name] adds the module named [name] to the list of dependencies if
    no corresponding ".dko" file is found in the load path. The dependency is
    not added either if it is already present. *)
let add_dep : string -> unit = fun name ->
  if find_dko name (get_path ()) then ()      (* The ".dko" exists.    *)
  else if name = !current_mod then ()         (* Current module.       *)
  else if List.mem name !current_deps then () (* Already a dependency. *)
  else current_deps := List.sort String.compare (name :: !current_deps)

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

type dep_data = string * (string * string list)

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
  let objfile n =
    let src = fst (List.assoc n data) in
    (try Filename.chop_extension src with _ -> src) ^ ".dko"
  in
  let output_line (name, (file, deps)) =
    let deps = List.map objfile deps in
    let deps = String.concat " " deps in
    Printf.fprintf oc "%s : %s %s\n" (objfile name) file deps
  in
  List.iter output_line data

let topological_sort graph =
  let rec explore path visited node =
    if List.mem node path then
      begin
        Printf.eprintf "Circular dependencies...";
        exit 1
      end;
    if List.mem node visited then visited else
    let edges = List.assoc node graph in
    node :: List.fold_left (explore (node :: path)) visited edges
  in
  List.fold_left (fun visited (n,_) -> explore [] visited n) [] graph

let output_sorted : out_channel -> dep_data list -> unit = fun oc data ->
  let deps = List.map (fun (n,(_,ds)) -> (n,ds)) data in
  let filename n = fst (List.assoc n data) in
  let res = List.map filename (List.rev (topological_sort deps)) in
  Printf.printf "%s\n" (String.concat " " res)

let _ =
  (* Parsing of command line arguments. *)
  let output  = ref stdout in
  let sorted  = ref false in
  let args = Arg.align
    [ ( "-o"
      , Arg.String (fun n -> output := open_out n)
      , "FILE Outputs to file FILE" )
    ; ( "-s"
      , Arg.Set sorted
      , " Sort the source files according to their dependencies" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Add the directory DIR to the load path" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Existing \".dko\" files are not included in deps.\n" in
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
