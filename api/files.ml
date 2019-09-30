type files_error =
  | ModuleNotFound     of Basic.mident
  | MultipleModules    of string * string list
  | ObjectFileNotFound of Basic.mident

exception Files_error of files_error

let fail_file_error err =
  match err with
  | ModuleNotFound md ->
    900, None, Format.asprintf "No file for module %a in path...@." Basic.pp_mident md
  | MultipleModules(s,ss) ->
    901, None, Format.asprintf "Several files correspond to module %s...@. %a" s
      (Basic.pp_list "@." (fun fmt s -> Format.fprintf fmt " - %s" s)) ss
  | ObjectFileNotFound md ->
    902, None, Format.asprintf "No object file (.dko) found for module %a@." Basic.pp_mident md

let fail_file_error ~red:_ exn =
  match exn with
  | Files_error err -> Some(fail_file_error err)
  | _ -> None

let _ = Errors.register_exception fail_file_error


let path = ref []

let get_path () = !path

let add_path s = path := s :: !path

let file_extension        = ".dk"

let object_file_extension = ".dko"

let rec find_dko_in_path lc basename = function
  | [] -> raise @@ Files_error (ObjectFileNotFound (Basic.mk_mident basename))
  | dir :: path ->
    let filename = dir ^ "/" ^ basename ^ object_file_extension in
    if Sys.file_exists filename
    then filename
    else find_dko_in_path lc basename path

let find_object_file lc md =
  let basename = Basic.string_of_mident md in
  let filename = basename ^ object_file_extension in
  if Sys.file_exists filename (* First check in the current directory *)
  then filename
  else find_dko_in_path lc basename (get_path())
  (* If not found in the current directory, search in load-path *)

let object_file_of_input input =
    let filename =
    match Parser.file_of_input input with
    | None ->
      Basic.string_of_mident (Parser.md_of_input input)
    | Some f -> Filename.chop_extension f
  in
  filename ^ ".dko"

let find_dk : ignore:bool -> Basic.mident -> string list -> string option = fun ~ignore md path ->
  let name = Basic.string_of_mident md in
  let file_name = name ^ file_extension in
  let path = Filename.current_dir_name :: path in
  let path = List.sort_uniq String.compare path in
  let add_dir dir =
    if dir = Filename.current_dir_name then file_name
    else Filename.concat dir file_name
  in
  let files = List.map add_dir path in
  match List.filter Sys.file_exists files with
  | []  ->
    if ignore then None
    else
      raise @@ Files_error (ModuleNotFound md)
  | [f] -> Some f
  | fs  ->
    raise @@ Files_error(MultipleModules(name,fs))

let get_file md =
  match find_dk ~ignore:false md (get_path ()) with
  | None -> raise @@ Files_error(ModuleNotFound md)
  | Some f -> f
