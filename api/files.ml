type path = string

let md file =
  let open Filename in
  basename file |> remove_extension |> Kernel.Basic.mk_mident

type load_path = path list

type file = File of string [@@unboxed]

let empty = []

let add_path load_path path = path :: load_path

let pp fmt = function
  | [] -> Format.fprintf fmt "<empty load path>"
  | _ as load_path ->
      Format.pp_print_list ~pp_sep:Format.pp_print_newline
        Format.pp_print_string fmt load_path

let regular_file_extension = "dk"

let object_file_extension = "dko"

type status =
  | File_not_found
  | File_found_once of file
  | File_found_multiple_time of file list

let basename_of_mident md =
  Kernel.Basic.string_of_mident md ^ "." ^ regular_file_extension

let rec file_status load_path md candidates =
  match load_path with
  | [] -> (
      (* Look into the current directory *)
      let file = basename_of_mident md in
      let candidates =
        if Sys.file_exists file then File file :: candidates else candidates
      in
      match candidates with
      | [] -> File_not_found
      | [file] -> File_found_once file
      | files -> File_found_multiple_time files)
  | path :: load_path ->
      let basename = basename_of_mident md in
      let candidate = Filename.concat path basename in
      if Sys.file_exists candidate then
        file_status load_path md (File candidate :: candidates)
      else file_status load_path md candidates

let file_status load_path md = file_status load_path md []

let as_object_file ?prefix (File file) =
  let basename = Filename.chop_extension file ^ ".dko" in
  let filename =
    match prefix with
    | None -> basename
    | Some path -> Filename.concat path basename
  in
  File filename

let input_as_file ?(default_path = Filename.current_dir_name) input =
  match Parsers.Parser.file_of_input input with
  | None ->
      let md = Parsers.Parser.md_of_input input in
      File (Filename.concat default_path (basename_of_mident md))
  | Some file -> File file

type file_error =
  | Error_file_not_found of {
      loc : Kernel.Basic.loc;
      load_path : load_path;
      md : Kernel.Basic.mident;
    }
  | Error_file_found_multiple_times of {
      loc : Kernel.Basic.loc;
      md : Kernel.Basic.mident;
      candidates : file list;
    }
  | Object_file_not_found of {
      loc : Kernel.Basic.loc;
      md : Kernel.Basic.mident;
      path : path;
    }

let string_of_error = function
  | Error_file_not_found {loc; load_path; md} ->
      ( 900,
        Some loc,
        Format.asprintf "No file for module %a in path.@.Load path:@.%a@."
          Kernel.Basic.pp_mident md pp load_path )
  | Error_file_found_multiple_times {loc; md; candidates} ->
      ( 901,
        Some loc,
        Format.asprintf
          "Several files correspond to module %a.@.Candidates are:@.%a@."
          Kernel.Basic.pp_mident md pp
          (List.map (fun (File x) -> x) candidates) )
  | Object_file_not_found {loc; md; path} ->
      ( 902,
        Some loc,
        Format.asprintf
          "No object file (.dko) found for module %a located at %s@."
          Kernel.Basic.pp_mident md path )

exception File_error of file_error

let fail_file_error ~reduce:_ exn =
  match exn with File_error err -> Some (string_of_error err) | _ -> None

let _ = Errors.register_exception fail_file_error

let get_file_exn load_path loc md =
  match file_status load_path md with
  | File_not_found ->
      raise @@ File_error (Error_file_not_found {loc; load_path; md})
  | File_found_multiple_time candidates ->
      raise
      @@ File_error (Error_file_found_multiple_times {loc; md; candidates})
  | File_found_once file -> file

let find_object_file_exn ?prefix load_path loc md =
  let file = get_file_exn load_path loc md in
  let (File path) = as_object_file ?prefix file in
  if Sys.file_exists path then path
  else raise @@ File_error (Object_file_not_found {loc; md; path})
