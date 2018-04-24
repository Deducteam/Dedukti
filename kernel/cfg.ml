type t =
  {
    mutable checking:bool;
    mutable solving:bool;
    mutable debug:int;
    mutable sg:Signature.t;
  }

let env = { checking = true;
            solving = true;
            debug = 0;
            sg = Signature.make "noname"
          }

let set_checking b = env.checking <- b

let set_solving b = env.solving <- b

let set_debug b = env.debug <- b

let make () = env

let get_signature env = env.sg
