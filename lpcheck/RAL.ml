(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Random-Access Lists} *)

(** A complete binary tree *)
type +'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

and +'a t = (int * 'a tree) list
  (** Functional array of complete trees *)


(** {2 Functions on trees} *)

(* lookup [i]-th element in the tree [t], which has size [size] *)
let rec tree_lookup size t i = match t, i with
  | Leaf x, 0 -> x
  | Leaf _, _ -> raise (Invalid_argument "RAL.get: wrong index")
  | Node (x, _, _), 0 -> x
  | Node (_, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size'
      then tree_lookup size' t1 (i-1)
      else tree_lookup size' t2 (i-1-size')

(* replaces [i]-th element by [v] *)
let rec tree_update size t i v =match t, i with
  | Leaf _, 0 -> Leaf v
  | Leaf _, _ -> raise (Invalid_argument "RAL.set: wrong index")
  | Node (_, t1, t2), 0 -> Node (v, t1, t2)
  | Node (x, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size'
      then Node (x, tree_update size' t1 (i-1) v, t2)
      else Node (x, t1, tree_update size' t2 (i-1-size') v)

(** {2 Functions on lists of trees} *)

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

let rec get l i = match l with
  | [] -> raise (Invalid_argument "RAL.get: wrong index")
  | (size,t) :: _ when i < size -> tree_lookup size t i
  | (size,_) :: l' -> get l' (i - size)

let rec set l i v = match l with
  | [] -> raise (Invalid_argument "RAL.set: wrong index")
  | (size,t) :: l' when i < size -> (size, tree_update size t i v) :: l'
  | (size,t) :: l' -> (size, t) :: set l' (i - size) v

let cons x l = match l with
  | (size1, t1) :: (size2, t2) :: l' ->
    if size1 = size2
      then (1 + size1 + size2, Node (x, t1, t2)) :: l'
      else (1, Leaf x) :: l
  | _ -> (1, Leaf x) :: l

let hd l = match l with
  | [] -> raise (Invalid_argument "RAL.hd: empty list")
  | (_, Leaf x) :: _ -> x
  | (_, Node (x, _, _)) :: _ -> x

let tl l = match l with
  | [] -> raise (Invalid_argument "RAL.tl: empty list")
  | (_, Leaf _) :: l' -> l'
  | (size, Node (_, t1, t2)) :: l' ->
    let size' = size / 2 in
    (size', t1) :: (size', t2) :: l'

let rec length l = match l with
  | [] -> 0
  | (size,_) :: l' -> size + length l'

let rec iter l f = match l with
  | [] -> ()
  | (_, Leaf x) :: l' -> f x; iter l' f
  | (_, t) :: l' -> iter_tree t f; iter l' f
and iter_tree t f = match t with
  | Leaf x -> f x
  | Node (x, t1, t2) -> f x; iter_tree t1 f; iter_tree t2 f

let rec map l f = match l with
  | [] -> []
  | (a, Leaf x) :: l' -> (a,Leaf (f x)) :: (map l' f)
  | (a, t) :: l' -> (a,map_tree t f) :: (map l' f)
and map_tree t f = match t with
  | Leaf x -> Leaf (f x)
  | Node (x, t1, t2) -> Node ( f x , map_tree t1 f , map_tree t2 f )

let rec fold l acc f = match l with
  | [] -> acc
  | (_, Leaf x) :: l' -> fold l' (f acc x) f
  | (_, t) :: l' ->
    let acc' = fold_tree t acc f in
    fold l' acc' f
and fold_tree t acc f = match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = f acc x in
    let acc = fold_tree t1 acc f in
    fold_tree t2 acc f

let of_list l = List.fold_right cons l empty

let to_list l = List.rev (fold l [] (fun l x -> x :: l))
