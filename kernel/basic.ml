(** Basic Datatypes *)


(** {2 Identifiers (hashconsed strings)} *)

type ident = string

let string_of_ident s = s

let ident_eq s1 s2 = s1==s2 || s1=s2


type mident = string

let string_of_mident s = s

let mident_eq = ident_eq


(* TODO: rename ident *)
type name = mident * ident

let mk_name md id = (md,id)

let name_eq (m,s) (m',s') = mident_eq m m' && ident_eq s s'

let md = fst
let id = snd



module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash        = WS.create 251

let mk_ident     = WS.merge shash

let mk_mident md =
  let base = Filename.basename md in
  try Filename.chop_extension base with _ -> base

let dmark       = mk_ident "$"

type polarity = P | N

let notp = function P -> N | N -> P

type polar = Pos | Neg | Both


(** {2 Lists with Length} *)

module LList = struct
  type 'a t= {
    len : int;
    lst : 'a list;
  }

  let cons x {len;lst} = {len=len+1; lst=x::lst}
  let nil = {len=0;lst=[];}

  let make ~len lst =
    assert (List.length lst = len);
    {lst;len}

  let make_unsafe ~len lst = {len;lst}
  (** make_unsafe [n] [l] is as make [n] [l] without checking that the length of [l] is [n] *)

  let of_list  lst = {len=List.length lst ; lst}
  let of_array arr = {len=Array.length arr; lst=Array.to_list arr}

  let len x = x.len
  let lst x = x.lst
  let is_empty x = x.len = 0

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

let pp_loc fmt (l,c) = Format.fprintf fmt "line:%i column:%i" l c

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

(** {2 Debugging} *)

module Debug = struct

  type flag = int
  let d_warn         : flag = 0
  let d_notice       : flag = 1
  let d_module       : flag = 2
  let d_confluence   : flag = 3
  let d_rule         : flag = 4
  let d_typeChecking : flag = 5
  let d_reduce       : flag = 6
  let d_matching     : flag = 7

  let nb_flags = 7

  (* Default mode is to debug only [d_std] messages. *)
  let default_flags = [d_warn]

  (* Headers for debugging messages *)
  let headers =
    [| "Warning"
     ; "Notice"
     ; "Module"
     ; "Confluence"
     ; "Rule"
     ; "TypeChecking"
     ; "Reduce"
     ; "Matching"
    |]

  (* Array of activated flags. Initialized with [false]s except at [default_flags] indices. *)
  let active = Array.init nb_flags (fun f -> List.mem f default_flags)

  let  enable_flag f = active.(f) <- true
  let disable_flag f = active.(f) <- false

  exception DebugFlagNotRecognized of char

  let set_debug_mode =
    String.iter (function
        | 'q' -> disable_flag d_warn
        | 'n' -> enable_flag  d_notice
        | 'o' -> enable_flag  d_module
        | 'c' -> enable_flag  d_confluence
        | 'u' -> enable_flag  d_rule
        | 't' -> enable_flag  d_typeChecking
        | 'r' -> enable_flag  d_reduce
        | 'm' -> enable_flag  d_matching
        | c -> raise (DebugFlagNotRecognized c)
      )

  let do_debug fmt =
    Format.(kfprintf (fun _ -> pp_print_newline err_formatter ()) err_formatter fmt)

  let ignore_debug fmt =
    Format.(ifprintf err_formatter) fmt

  let debug f =
    if active.(f)
    then
      match headers.(f) with
      | "" -> do_debug
      | h -> (fun fmt -> do_debug ("[%s] " ^^ fmt) h)
    else ignore_debug
  [@@inline]

  let debug_eval f clos = if active.(f) then clos ()

end

(** {2 Misc functions} *)

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

let rec add_to_list2 l1 l2 lst =
  match l1, l2 with
  | [], [] -> Some lst
  | s1::l1, s2::l2 -> add_to_list2 l1 l2 ((s1,s2)::lst)
  | _,_ -> None

let rec split_list i l =
  if i = 0 then ([],l)
  else
    let s1, s2 = split_list (i-1) (List.tl l) in
    (List.hd l)::s1, s2


(** {2 Printing functions} *)

type 'a printer = Format.formatter -> 'a -> unit

let string_of fp = Format.asprintf "%a" fp

let pp_ident  fmt id      = Format.fprintf fmt "%s" id
let pp_mident fmt md      = Format.fprintf fmt "%s" md
let pp_name   fmt (md,id) = Format.fprintf fmt "%a.%a" pp_mident md pp_ident id
let pp_loc    fmt (l,c)   = Format.fprintf fmt "line:%i column:%i" l c

let format_of_sep str fmt () : unit = Format.fprintf fmt "%s" str

let pp_list sep pp fmt l = Format.pp_print_list ~pp_sep:(format_of_sep sep) pp fmt l
let pp_arr  sep pp fmt a = pp_list sep pp fmt (Array.to_list a)

let pp_option def pp fmt = function
  | None   -> Format.fprintf fmt "%s" def
  | Some a -> Format.fprintf fmt "%a" pp a

let pp_pol fmt = function
  | P -> Format.fprintf fmt "%s" "+"
  | N -> Format.fprintf fmt "%s" "-"
