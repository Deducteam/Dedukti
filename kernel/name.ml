type mident = string

type ident = {
  index:int; (* unique identifier *)
  md:mident;
  id:string;
}

let fresh_id =
  let state = ref (-1) in
  fun () -> state := !state + 1; !state

let make md id =
  {
    md=md;
    id=id;
    index=fresh_id()
  }

let make2 = make

let id id = id.id

let md id = id.md

let string_of_mident mid = mid

let make_mident md = md

let pp_ident fmt ident =
  Format.fprintf fmt "%s.%s" ident.md ident.id

let pp_mident fmt mident =
  Format.fprintf fmt "%s" mident

let pp_ident_confluence fmt ident =
  Format.fprintf fmt "%s_%s" ident.md ident.id

let equal id id' = id.index == id'.index

let mequal = (=)

let gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name = if i=0
      then Printf.sprintf "@%c" names.[j]
      else Printf.sprintf "@%c%d" names.[j] i
    in
    incr r;
    make name name
