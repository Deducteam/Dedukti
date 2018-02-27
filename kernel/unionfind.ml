(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Persistent union-find = Tarjan's algorithm with persistent arrays *)

(* persistent arrays; see the module [Parray] for explanations *)
module Pa = struct

  type ('a,'b) t = ('a,'b) data ref
  and ('a,'b) data =
    | Array of ('a,'b) Hashtbl.t
    | Diff of 'a * 'b * ('a,'b) t

  let create () = ref (Array (Hashtbl.create 10007))
  let init () = ref (Array (Hashtbl.create 10007))

  (* reroot t ensures that t becomes an Array node *)
  let rec reroot t = match !t with
    | Array _ -> ()
    | Diff (k, v, t') ->
      reroot t';
      begin match !t' with
	| Array h as n ->
          let v' = try Hashtbl.find h k with _ -> v in
	  Hashtbl.replace h k v;
	  t := n;
	  t' := Diff (k, v', t)
	| Diff _ -> assert false
      end

  let rec rerootk t k =
    match !t with
    | Array _ -> k ()
    | Diff (key, v, t') ->
      rerootk t' (fun () ->
          begin
            match !t' with
	    | Array h as n ->
       let v' = try Hashtbl.find h key with _ -> v in
       Hashtbl.replace h key v;
       t := n;
       t' := Diff (key, v', t)
     | Diff _ -> assert false end; k())

  let reroot t = rerootk t (fun () -> ())

  let rec get ?default:(default=None) t k = match !t with
    | Array h ->
      begin
        match default with
        | None -> Hashtbl.find h k
        | Some a -> try Hashtbl.find h k with _ -> a
      end
    | Diff _ ->
      reroot t;
      begin match !t with Array h -> Hashtbl.find h k | Diff _ -> assert false end

  let set t k v =
    reroot t;
    match !t with
    | Array h as n ->
      let old = try Hashtbl.find h k with _ -> v in
      if old = v then
	t
      else begin
        Hashtbl.replace h k v;
	let res = ref n in
	t := Diff (k, old, res);
	res
      end
    | Diff _ ->
      assert false

end

(* Tarjan's algorithm *)

type 'a t = {
  mutable father: ('a,'a) Pa.t; (* mutable to allow path compression *)
  c: ('a,int) Pa.t; (* ranks *)
}

let create () =
  { c = Pa.create ();
    father = Pa.init () }

let rec find_aux f i =
  let fi = Pa.get ~default:(Some i) f i in
  if fi = i then
    f, i
  else
    let f, r = find_aux f fi in
    let f = Pa.set f i r in
    f, r

let find h x =
  let f,rx = find_aux h.father x in h.father <- f; rx

let union h x y =
  let rx = find h x in
  let ry = find h y in
  if rx != ry then begin
    let rxc = Pa.get ~default:(Some 0) h.c rx in
    let ryc = Pa.get ~default:(Some 0) h.c ry in
    if rxc > ryc then
      { h with father = Pa.set h.father ry rx }
    else if rxc < ryc then
      { h with father = Pa.set h.father rx ry }
    else
      { c = Pa.set h.c rx (rxc + 1);
	father = Pa.set h.father ry rx }
  end else
    h


(* tests *)
(***
   let t = create 10
   let () = assert (find t 0 <> find t 1)
   let t = union t 0 1
   let () = assert (find t 0 = find t 1)
   let () = assert (find t 0 <> find t 2)
   let t = union t 2 3
   let t = union t 0 3
   let () = assert (find t 1 = find t 2)
   let t = union t 4 4
   let () = assert (find t 4 <> find t 3)
 ***)
