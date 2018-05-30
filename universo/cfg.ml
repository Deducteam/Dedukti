open Basic

type univ = Var of string | Prop | Type of int | Succ of univ | Max of univ * univ | Rule of univ * univ

type cstr = univ * univ

let rec pp_univ fmt = function
  | Var s -> Format.fprintf fmt "!%s" s
  | Prop -> Format.fprintf fmt "P"
  | Type i -> Format.fprintf fmt "T%d" i
  | Succ u -> Format.fprintf fmt "S(%a)" pp_univ u
  | Max(ul,ur) -> Format.fprintf fmt "M(%a,%a)" pp_univ ul pp_univ ur
  | Rule(ul,ur) -> Format.fprintf fmt "R(%a,%a)" pp_univ ul pp_univ ur

let pp_cstr fmt (l,r) = Format.fprintf fmt "%a =?= %a" pp_univ l pp_univ r


module ConstraintsSet = Set.Make(struct type t = cstr let compare = compare end)

type t =
  {
    mutable solving:bool;
    mutable debug:int;
    mutable sg:Signature.t;
    mutable names: name list;
    mutable univ_max:int;
    output_file: (mident, string) Hashtbl.t;
    uvars: (name, ISet.t) Hashtbl.t;
    constraints: (name, ConstraintsSet.t) Hashtbl.t
  }

let env = {
            solving = true;
            debug = 0;
            sg = Signature.make "noname";
            univ_max = 6;
            output_file = Hashtbl.create 23;
            names = [];
            uvars = Hashtbl.create 503;
            constraints = Hashtbl.create 11;
          }

let set_solving b = env.solving <- b

let set_debug b = env.debug <- b

let set_signature sg = env.sg <- sg

let get_signature () = env.sg

let get_solving () = env.solving

let add_name name = env.names <- name::env.names

let add_fmt md str = Hashtbl.add env.output_file md str

let add_uvars name uvars = Hashtbl.add env.uvars name uvars

let get_uvars name =
  try
    Hashtbl.find env.uvars name
  with _ -> assert false

let univ_max () = env.univ_max

let add_constraints name constraints = Hashtbl.add env.constraints name constraints

let get_constraints name =
  try
    Hashtbl.find env.constraints name
  with _ -> assert false
