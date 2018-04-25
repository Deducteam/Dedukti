open Basic

type t =
  {
    mutable checking:bool;
    mutable solving:bool;
    mutable debug:int;
    mutable sg:Signature.t;
    mutable names: name list;
    mutable cpt_uvars: int;
    output_file: (mident, string) Hashtbl.t;
    uvars: (name, ISet.t) Hashtbl.t;
  }

let env = { checking = true;
            solving = true;
            debug = 0;
            cpt_uvars = 0;
            sg = Signature.make "noname";
            output_file = Hashtbl.create 23;
            names = [];
            uvars = Hashtbl.create 503
          }

let set_checking b = env.checking <- b

let set_solving b = env.solving <- b

let set_debug b = env.debug <- b

let set_signature sg = env.sg <- sg

let get_signature () = env.sg

let get_checking () = env.checking

let add_name name = env.names <- name::env.names

let add_fmt md str = Hashtbl.add env.output_file md str

let add_uvars name uvars = Hashtbl.add env.uvars name uvars

let incr_cpt_uvars () = env.cpt_uvars <- env.cpt_uvars + 1

let get_uvars name =
  try
    Hashtbl.find env.uvars name
  with _ -> assert false
