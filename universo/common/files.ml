module B = Kernel.Basic
module S = Kernel.Signature
module P = Parsers.Parser
module Processor = Api.Processor

type path = string
(** path to a file *)

type cin

type cout

(** Gather out_channel and in_channel in a GADT for input and output files. In the case of an output file, we store also the formatter associated with. *)
type _ channel =
  | In : in_channel -> cin channel
  | Out : out_channel * Format.formatter -> cout channel

type 'a t = {
  path : path;  (** Path to the current file *)
  md : B.mident;  (** Md of this file *)
  channel : 'a channel;  (** OCaml channel to this file *)
}

(** Output directory where output files are created *)
let output_directory : string option ref = ref None

(** Simplify directory where simplified files are created. *)
let simplify_directory : string option ref = ref None

(** Suffix used for files containing universe declarations *)
let elaboration_suffix = "_univ"

(** Suffix used for files containing universe constraints *)
let checking_suffix = "_cstr"

(** Suffix used for files containing universe solution *)
let solution_suffix = "_sol"

(** Suffix used for elaborated file where sorts are replaced by fresh variables *)
let normal_suffix = ""

type step =
  [ `Input  (** Input module *)
  | `Output  (** Output module *)
  | `Elaboration  (** File with universe declarations *)
  | `Checking  (** File with constraints *)
  | `Solution  (** File containing the solution *)
  | `Simplify
    (** Output file where the variables are replaced by the solution *) ]
(** The steps used to refer the files used by Universo *)

(** [add_sufix file suffix] returns the string [file'] where suffix is_added at then end of [file] *)
let add_suffix : path -> string -> string =
 fun file suffix ->
  let ext = Filename.extension file in
  let name = Filename.chop_extension file in
  name ^ suffix ^ ext

(** [add_dir dir file] prefix the filename [file] with the directory [dir] *)
let add_dir : string -> string -> string =
 fun dir file -> dir ^ Filename.dir_sep ^ Filename.basename file

let mk_dir : string option ref -> string -> unit =
 fun rf dir ->
  (try ignore (Unix.stat dir) with _ -> Unix.mkdir dir 0o755);
  rf := Some dir

(** return the suffix according to the step [s] *)
let suffix_of_step : step -> string = function
  | `Elaboration -> elaboration_suffix
  | `Checking -> checking_suffix
  | `Solution -> solution_suffix
  | `Input | `Simplify | `Output -> normal_suffix

let theory : string option ref = ref None

let mk_theory : path -> unit = fun path -> theory := Some path

let get_theory : unit -> string =
 fun () ->
  match !theory with None -> failwith "Theory not given" | Some t -> t

(** [get_out_path p s] returns the path that corresponds to the step [s] for path [p] *)
let get_out_path : path -> step -> path =
 fun path step ->
  let file_suffix = add_suffix path (suffix_of_step step) in
  match step with
  | `Input -> file_suffix
  | `Simplify -> (
      match !simplify_directory with
      | None -> assert false
      | Some dir -> add_dir dir file_suffix )
  | _ -> (
      match !output_directory with
      | None ->
          failwith
            "Output_directory must be set. See --help for more information"
      | Some dir -> add_dir dir file_suffix )

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let out_from_string : path -> step -> cout t =
 fun path step ->
  let path = get_out_path path step in
  let md = P.md_of_file path in
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  { path; md; channel = Out (oc, fmt) }

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let in_from_string : path -> step -> cin t =
 fun path step ->
  let path = get_out_path path step in
  let md = P.md_of_file path in
  let ic = open_in path in
  { path; md; channel = In ic }

(* (\** [signature_of_file f] returns a signature that contains all the declarations in [f]. *\) *)
(* let signature_of_file : path -> S.t = *)
(*  fun file -> Api.Processor.handle_files [ file ] Api.Processor.SignatureBuilder *)

(** [fmt_of_file out_file] returns the formatter associated to an [out_file] *)
let fmt_of_file : cout t -> Format.formatter =
 fun file -> match file.channel with Out (_, fmt) -> fmt

(** [in_channel_of_file in_file] returns the channel associated to an [in_file] *)
let in_channel_of_file : cin t -> in_channel =
 fun file -> match file.channel with In ic -> ic

(** [close file] closes [file] *)
let close : type a. a t -> unit =
 fun file ->
  match file.channel with Out (oc, _) -> close_out oc | In ic -> close_in ic

(** [md_of path step] returns the mident associated to the Universo file [file] for step [step]. *)
let md_of : path -> step -> B.mident =
 fun in_path step -> P.md_of_file (get_out_path in_path step)

let add_requires : Format.formatter -> B.mident list -> unit =
 fun fmt mds ->
  List.iter
    (fun md ->
      Format.fprintf fmt "#REQUIRE %a.@." Api.Pp.Default.print_mident md)
    mds

type _ Processor.t += FilterSignature : Api.Env.t Processor.t

let export : path -> step -> unit =
 fun in_path step ->
  let out_file = get_out_path in_path step in
  let is_eq_rule r =
    let open Kernel.Rule in
    match r.pat with Pattern (_, _, []) -> true | _ -> false
  in
  let (module SB) = Processor.get_processor Processor.SignatureBuilder in
  let module P = struct
    type t = Api.Env.t

    let handle_entry env entry =
      let (module SB) = Processor.get_processor Processor.SignatureBuilder in
      match (step, entry) with
      (* TODO: don't remember why only equality constraints are exported *)
      | `Checking, Parsers.Entry.Rules (_, r :: _) when is_eq_rule r ->
          SB.handle_entry env entry
      | `Checking, _ -> ()
      | _, _ -> SB.handle_entry env entry

    let get_data env = env
  end in
  let env = Api.Processor.T.handle_files [ out_file ] (module P) in
  Api.Env.export env
