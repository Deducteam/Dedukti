val genv : Environ.env ref
val reset : unit -> unit
val get_env : unit -> Environ.env
val set_engagement : Declarations.engagement -> unit
val full_add_module :
  Names.dir_path -> Declarations.module_body -> Digest.t -> unit
val check_engagement : Environ.env -> Declarations.engagement option -> unit
val report_clash :
  (Pp.std_ppcmds -> 'a) -> Names.dir_path -> Names.dir_path -> 'a
val check_imports :
  (Pp.std_ppcmds -> unit) ->
  Names.dir_path -> Environ.env -> (Names.dir_path * Digest.t) list -> unit
val lighten_module : Declarations.module_body -> Declarations.module_body
val lighten_struct :
  Declarations.structure_body -> Declarations.structure_body
val lighten_modexpr :
  Declarations.struct_expr_body -> Declarations.struct_expr_body
val lighten_library :
  'a * Declarations.module_body * 'b * 'c ->
  'a * Declarations.module_body * 'b * 'c
type compiled_library =
    Names.dir_path * Declarations.module_body *
    (Names.dir_path * Digest.t) list * Declarations.engagement option
val val_deps : Obj.t -> unit
val val_vo : Obj.t -> unit
val stamp_library : 'a -> 'b -> unit
val import :
  'a ->
  Names.dir_path * Declarations.module_body *
  (Names.dir_path * Digest.t) list * Declarations.engagement option ->
  Digest.t -> unit
val unsafe_import :
  'a ->
  Names.dir_path * Declarations.module_body *
  (Names.dir_path * Digest.t) list * Declarations.engagement option ->
  Digest.t -> unit
