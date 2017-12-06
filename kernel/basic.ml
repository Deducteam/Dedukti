(** Basic Datatypes *)


(** {2 Identifiers (hashconsed strings)} *)

type ident = string

let pp_ident fmt id = Format.fprintf fmt "%s" id

type mident = string

let pp_mident fmt md = Format.fprintf fmt "%s" md

type name = mident * ident

let pp_name fmt (md,id) = Format.fprintf fmt "%s.%s" md id

let md = fst

let id = snd

let mk_name md id = (md,id)

let string_of_ident s = s

let string_of_mident s = s

let ident_eq s1 s2 = s1==s2 || s1=s2

let mident_eq = ident_eq

let name_eq n n' = ident_eq (md n) (md n') && ident_eq (id n) (id n')


module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash       = WS.create 251

let mk_ident    = WS.merge shash

let mk_mident   = mk_ident

let qmark       = mk_ident "?"

let dmark       = mk_ident "$"

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

let pp_loc    fmt (l,c) = Format.fprintf fmt "line:%i column:%i" l c

let path = ref []
let get_path () = !path
let add_path s = path := s :: !path

(** {2 Errors} *)

type ('a,'b) error =
  | OK of 'a
  | Err of 'b

let map_error f = function
  | Err c -> Err c
  | OK a -> OK (f a)

let bind_error f = function
  | Err c -> Err c
  | OK a -> f a

let map_error_list (f:'a -> ('b,'c) error) (lst:'a list) : ('b list,'c) error =
  let rec aux = function
    | [] -> OK []
    | hd::lst ->
        ( match f hd with
            | Err c -> Err c
            | OK hd -> ( match aux lst with
                           | Err c -> Err c
                           | OK lst -> OK (hd::lst) )
        )
  in
    aux lst

let debug_mode = ref 0

let set_debug_mode i = debug_mode := i

let debug i fmt = Format.(
    if !debug_mode >= i then
      kfprintf (fun _ -> pp_print_newline err_formatter ()) err_formatter fmt
  else ifprintf err_formatter fmt
  )

let bind_opt f = function
  | None -> None
  | Some x -> f x

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let fold_map (f:'b->'a->('c*'b)) (b0:'b) (alst:'a list) : ('c list*'b) =
  let (clst,b2) =
    List.fold_left (fun (accu,b1) a -> let (c,b2) = f b1 a in (c::accu,b2))
      ([],b0) alst in
    ( List.rev clst , b2 )

let string_of fp = Format.asprintf "%a" fp

let format_of_sep str fmt () : unit =
  Format.fprintf fmt "%s" str

let pp_list sep pp fmt l = Format.pp_print_list ~pp_sep:(format_of_sep sep) pp fmt l
