(** Basic Datatypes *)

(** {2 Identifiers (hashconsed strings)} *)

type ident = string
let string_of_ident s = s
let ident_eq s1 s2 = s1==s2 || s1=s2
let pp_ident = output_string
let print_ident = Format.pp_print_string

module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash       = WS.create 251
let hstring     = WS.merge shash
let qmark       = hstring "?"

(** {2 Lists with Length} *)

module LList = struct
  type 'a t= {
    len : int;
    lst : 'a list;
  }

  let cons x {len;lst} = {len=len+1; lst=x::lst}
  let nil = {len=0;lst=[];}
  let len x = x.len
  let lst x = x.lst
  let is_empty x = x.len = 0

  let of_list lst = {len=List.length lst;lst}

  let make ~len lst =
    assert (List.length lst = len);
    {lst;len}

  let make_unsafe ~len lst = {len;lst}

  let map f {len;lst} = {len; lst=List.map f lst}
  let append_l {len;lst} l = {len=len+List.length l; lst=lst@l}

  let nth l i = assert (i<l.len); List.nth l.lst i

  let remove i {len;lst} =
    let rec aux c lst = match lst with
      | []        -> assert false
      | x::lst'   -> if c==0 then lst' else x::(aux (c-1) lst')
    in
    {len=len-1; lst=aux i lst}
end

(** {2 Localization} *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (l,c)
let of_loc l = l

let path = ref []
let get_path () = !path
let add_path s = path := s :: !path
