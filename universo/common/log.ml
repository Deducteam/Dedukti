module D = Kernel.Basic.Debug

let debug_flag = D.register_flag "Universo"

(** Format transformers (colors). *)
let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!"

let gre fmt = "\027[32m" ^^ fmt ^^ "\027[0m%!"

let yel fmt = "\027[33m" ^^ fmt ^^ "\027[0m%!"

let blu fmt = "\027[34m" ^^ fmt ^^ "\027[0m%!"

let mag fmt = "\027[35m" ^^ fmt ^^ "\027[0m%!"

let cya fmt = "\027[36m" ^^ fmt ^^ "\027[0m%!"

let log color fmt = D.debug debug_flag (color fmt)

let log_check fmt = log mag fmt

let log_elab fmt = log cya fmt

let log_solver fmt = log yel fmt

let log_univ fmt = log gre fmt

let error s = Format.eprintf "\027[31m%s\027[0m%!@." s

(** [enable_flag str] actives flags present in [str] *)
let enable_flag : string -> unit = fun str -> Api.Env.set_debug_mode str

let enable_universo_flag : unit -> unit = fun () -> D.enable_flag debug_flag
