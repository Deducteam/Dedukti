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

(* Persistent arrays implemented using Backer's trick.

   A persistent array is a usual array (node Array) or a change into 
   another persistent array (node Diff). Invariant: any persistent array is a
   (possibly empty) linked list of Diff nodes ending on an Array node.

   As soon as we try to access a Diff, we reverse the linked list to move
   the Array node to the position we are accessing; this is achieved with
   the reroot function.
*)

type 'a t = 'a data ref
and 'a data =
  | Array of 'a array 
  | Diff of int * 'a * 'a t


let create n v = ref (Array (Array.create n v))			  
let make n v = ref (Array (Array.make n v))

let init n f = ref (Array (Array.init n f))

(* reroot t ensures that t becomes an Array node *)
let rec reroot t = match !t with
  | Array _ -> ()
  | Diff (i, v, t') -> 
      reroot t';
      begin match !t' with
        | Array a as n ->
            let v' = a.(i) in
            a.(i) <- v;
            t := n;
            t' := Diff (i, v', t)
        | Diff _ -> assert false
      end
  
(* we rewrite it using CPS to avoid a possible stack overflow *)
let rec rerootk t k = match !t with
  | Array _ -> k ()
  | Diff (i, v, t') -> 
      rerootk t' (fun () -> begin match !t' with
                   | Array a as n ->
                       let v' = a.(i) in
                       a.(i) <- v;
                       t := n;
                       t' := Diff (i, v', t)
                   | Diff _ -> assert false end; k())

let reroot t = rerootk t (fun () -> ())

let rec get t i = match !t with
  | Array a -> 
      a.(i)
  | Diff _ -> 
      reroot t; 
      begin match !t with Array a -> a.(i) | Diff _ -> assert false end
      
let set t i v = 
  reroot t;
  match !t with
  | Array a as n ->
      let old = a.(i) in
      if old == v then
        t
      else begin
        a.(i) <- v;
        let res = ref n in
        t := Diff (i, old, res);
        res
      end
  | Diff _ ->
      assert false

(* wrappers to apply an impure function from Array to a persistent array *)
let impure f t =
  reroot t;
  match !t with Array a -> f a | Diff _ -> assert false

let length t = impure Array.length t

let to_list t = impure Array.to_list t

let iter f t = impure (Array.iter f) t
let iteri f t = impure (Array.iteri f) t

let fold_left f acc t = impure (Array.fold_left f acc) t
let fold_right f t acc = impure (fun a -> Array.fold_right f a acc) t

let equal t t' = reroot t; reroot t'; t = t'		
